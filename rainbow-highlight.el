;; --- Rainbow Spotlight (percent sizing + centered + no line numbers) -------
;; Select a region → M-x rainbow-spotlight-region  (or hit C-c r)
;; Fullscreen, huge font sized as % of frame height, horizontally centered,
;; shimmering rainbow colors. Quit with `q`. Recompute size/centering with `R`.

(require 'cl-lib)   ;; for cl-loop, cl-destructuring-bind
(require 'color)

;; ==== User-tunable settings ==================================================
(defvar rainbow-spotlight-screen-percent 0.30) ;; ~30% of frame height per line
(defvar rainbow-spotlight-center-width 80)     ;; desired text width (columns)
(defvar rainbow-spotlight-tick 0.08)           ;; lower = faster shimmer
(defvar rainbow-spotlight-saturation 1.0)      ;; 0..1
(defvar rainbow-spotlight-lightness 0.6)       ;; 0..1
(defvar rainbow-spotlight-bg "#000000")

;; ==== Internal state (don’t edit) ============================================
(defvar-local rainbow-spotlight--timer nil)
(defvar-local rainbow-spotlight--overlays nil)
(defvar-local rainbow-spotlight--phase 0.0)
(defvar-local rainbow-spotlight--winconf nil)
(defvar-local rainbow-spotlight--remap-cookie nil)

;; --- Line numbers: keep them OFF in spotlight buffers ------------------------
(defvar display-line-numbers-exempt-modes nil
  "Modes in which global line numbers should not be activated.")
(add-to-list 'display-line-numbers-exempt-modes 'rainbow-spotlight-mode)

(defun rainbow-spotlight--really-no-line-numbers ()
  "Disable any known line-number mode in this buffer, and keep it off."
  ;; Vanilla Emacs:
  (setq-local display-line-numbers nil)
  (when (bound-and-true-p display-line-numbers-mode)
    (display-line-numbers-mode -1))
  ;; Doom/derivatives:
  (when (boundp 'display-line-numbers-type)
    (setq-local display-line-numbers-type nil))
  ;; Legacy packages:
  (when (fboundp 'nlinum-mode) (ignore-errors (nlinum-mode -1)))
  (when (fboundp 'linum-mode)  (ignore-errors (linum-mode -1))))

;; Block attempts to turn line numbers on in our mode (e.g. global hooks).
(with-eval-after-load 'display-line-numbers
  (defun rainbow-spotlight--inhibit-dln-turn-on (orig &rest args)
    (if (derived-mode-p 'rainbow-spotlight-mode)
        (progn (rainbow-spotlight--really-no-line-numbers) nil)
      (apply orig args)))
  (advice-add 'display-line-numbers--turn-on :around
              #'rainbow-spotlight--inhibit-dln-turn-on))

;; ==== Helpers =================================================================
(defun rainbow-spotlight--rgb (h)
  "Return an RGB hex for hue H using configured saturation/lightness."
  (cl-destructuring-bind (r g b)
      (color-hsl-to-rgb (mod h 1.0)
                        (max 0.0 (min 1.0 rainbow-spotlight-saturation))
                        (max 0.0 (min 1.0 rainbow-spotlight-lightness)))
    (color-rgb-to-hex r g b 2)))

(defun rainbow-spotlight--calc-height-for-frame (&optional percent frame)
  "Compute a :height so one line is about PERCENT of FRAME height."
  (let* ((pct (or percent rainbow-spotlight-screen-percent))
         (frm (or frame (selected-frame)))
         (frame-px (frame-pixel-height frm))
         (char-px (with-selected-frame frm (frame-char-height)))
         (desired (max 1 (floor (* pct frame-px))))
         (height (round (* 100.0 (/ desired (float (max 1 char-px)))))))
    (max 120 (min 4000 height)))) ;; sanity clamp

(defun rainbow-spotlight--apply-font-size ()
  "Apply dynamic font size based on frame height."
  (when rainbow-spotlight--remap-cookie
    (face-remap-remove-relative rainbow-spotlight--remap-cookie))
  (setq rainbow-spotlight--remap-cookie
        (face-remap-add-relative 'default
                                 :height (rainbow-spotlight--calc-height-for-frame)
                                 :background rainbow-spotlight-bg)))

(defun rainbow-spotlight--center ()
  "Center text horizontally. Prefer visual-fill-column; fallback to margins."
  (if (require 'visual-fill-column nil t)
      (progn
        (setq-local visual-fill-column-width rainbow-spotlight-center-width)
        (setq-local visual-fill-column-center-text t)
        (visual-fill-column-mode 1))
    ;; Fallback: center by window margins around a fixed width.
    (let* ((win (selected-window))
           (total (window-total-width win))
           (target (min rainbow-spotlight-center-width (max 20 total)))
           (side (max 0 (/ (- total target) 2))))
      (set-window-margins win side side))))

(defun rainbow-spotlight--make-overlays ()
  "Create one overlay per visible character so we can colorize them."
  (setq rainbow-spotlight--overlays nil)
  (save-excursion
    (goto-char (point-min))
    (while (< (point) (point-max))
      (unless (memq (char-after) '(?\n ?\t))
        (let ((ov (make-overlay (point) (1+ (point)) nil t t)))
          (overlay-put ov 'face 'default)
          (push ov rainbow-spotlight--overlays)))
      (forward-char 1)))
  (setq rainbow-spotlight--overlays (nreverse rainbow-spotlight--overlays)))

(defun rainbow-spotlight--tick ()
  "Update overlay colors each animation tick."
  (when (buffer-live-p (current-buffer))
    (let* ((len (max 1 (length rainbow-spotlight--overlays)))
           (p rainbow-spotlight--phase)
           (span (max 30 len)))
      (cl-loop for i from 0
               for ov in rainbow-spotlight--overlays do
               (overlay-put ov 'face
                            `(:foreground ,(rainbow-spotlight--rgb
                                            (+ p (/ i (float span))))
                              :weight bold))))
    (setq rainbow-spotlight--phase (mod (+ rainbow-spotlight--phase 0.01) 1.0))))

(defun rainbow-spotlight--start-timer ()
  "Start the shimmer animation timer."
  (setq rainbow-spotlight--timer
        (run-with-timer 0 rainbow-spotlight-tick
                        (lambda (buf)
                          (when (buffer-live-p buf)
                            (with-current-buffer buf
                              (rainbow-spotlight--tick))))
                        (current-buffer))))

(defun rainbow-spotlight--stop-timer ()
  "Stop the shimmer animation timer."
  (when (timerp rainbow-spotlight--timer)
    (cancel-timer rainbow-spotlight--timer)
    (setq rainbow-spotlight--timer nil)))

;; ==== Mode ====================================================================
(defvar rainbow-spotlight-mode-map
  (let ((m (make-sparse-keymap)))
    (define-key m (kbd "q") #'rainbow-spotlight-quit)
    (define-key m (kbd "R") #'rainbow-spotlight-recompute-font)
    m)
  "Keymap for rainbow-spotlight-mode.")

(define-derived-mode rainbow-spotlight-mode special-mode "Rainbow-Spotlight"
  "Fullscreen large-font rainbow shimmer mode."
  ;; Keep line numbers off, even if globally enabled:
  (rainbow-spotlight--really-no-line-numbers)

  (setq-local cursor-type nil
              mode-line-format nil
              header-line-format (propertize "  Rainbow Spotlight — q: quit, R: recompute font"
                                             'face '(:height 140 :weight bold))
              truncate-lines nil
              show-trailing-whitespace nil
              line-spacing 0.25
              bidi-display-reordering nil
              bidi-paragraph-direction 'left-to-right)

  (buffer-face-mode 1)
  (rainbow-spotlight--apply-font-size)
  (rainbow-spotlight--center)
  (read-only-mode 1))

(defun rainbow-spotlight-recompute-font ()
  "Recompute font size and centering from current frame height (use after resize)."
  (interactive)
  (rainbow-spotlight--apply-font-size)
  (rainbow-spotlight--center)
  (rainbow-spotlight--really-no-line-numbers)
  (message "Rainbow Spotlight: recomputed for %.0f%% of frame height"
           (* 100 rainbow-spotlight-screen-percent)))

(defun rainbow-spotlight-quit ()
  "Exit the spotlight buffer and restore previous window layout."
  (interactive)
  (rainbow-spotlight--stop-timer)
  (let ((buf (current-buffer))
        (conf rainbow-spotlight--winconf))
    (when (window-configuration-p conf)
      (set-window-configuration conf))
    (kill-buffer buf)))

(defun rainbow-spotlight-region (beg end)
  "Display region from BEG to END in fullscreen rainbow spotlight."
  (interactive "r")
  (unless (use-region-p) (user-error "Select some text first"))
  (let* ((text (buffer-substring-no-properties beg end))
         (buf (get-buffer-create "*Rainbow Spotlight*")))
    (setq rainbow-spotlight--winconf (current-window-configuration))
    (with-current-buffer buf
      (setq buffer-read-only nil)
      (erase-buffer)
      (insert text)
      (goto-char (point-min))
      (rainbow-spotlight-mode)
      (rainbow-spotlight--make-overlays)
      (rainbow-spotlight--start-timer)
      (setq buffer-read-only t))
    (pop-to-buffer buf)
    (delete-other-windows)
    ;; Gentle vertical centering kick-off:
    (recenter (/ (window-body-height) 2))))

(global-set-key (kbd "C-c r") #'rainbow-spotlight-region)
;; ---------------------------------------------------------------------------
