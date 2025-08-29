;; --- Rainbow Spotlight (single line, auto-fits window width) -----------------
;; Select a region → M-x rainbow-spotlight-region  (or hit C-c r)
;; Fullscreen, huge font sized to fill window width (single line),
;; shimmering rainbow colors. Quit with `q`. Manual refit (optional) with `R`.

(require 'cl-lib)
(require 'color)

;; ==== User-tunable settings ==================================================
(defvar rainbow-spotlight-tick 0.08)           ;; lower = faster shimmer
(defvar rainbow-spotlight-saturation 1.0)      ;; 0..1
(defvar rainbow-spotlight-lightness 0.6)       ;; 0..1
(defvar rainbow-spotlight-bg "#000000")

;; ==== Internal state =========================================================
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
  (setq-local display-line-numbers nil)
  (when (bound-and-true-p display-line-numbers-mode)
    (display-line-numbers-mode -1))
  (when (boundp 'display-line-numbers-type)
    (setq-local display-line-numbers-type nil))
  (when (fboundp 'nlinum-mode) (ignore-errors (nlinum-mode -1)))
  (when (fboundp 'linum-mode)  (ignore-errors (linum-mode -1))))

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

(defun rainbow-spotlight--set-font-height (height)
  "Apply a specific :height to the spotlight buffer's default face."
  (when rainbow-spotlight--remap-cookie
    (face-remap-remove-relative rainbow-spotlight--remap-cookie))
  (setq rainbow-spotlight--remap-cookie
        (face-remap-add-relative 'default
                                 :height height
                                 :background rainbow-spotlight-bg)))

(defun rainbow-spotlight--window-body-pixel-width (&optional win)
  "Return pixel width of WIN's text area."
  (window-body-width (or win (selected-window)) t))

(defun rainbow-spotlight--text-pixel-width ()
  "Return the pixel width of all buffer text (single line)."
  (car (window-text-pixel-size nil (point-min) (point-max))))

(defun rainbow-spotlight--fit-to-window-width (&optional frame)
  "Set the largest :height so the line fits the window width."
  (let* ((frm (or frame (selected-frame)))
         (win (or (get-buffer-window (current-buffer) frm)
                  (frame-selected-window frm)))
         (avail (max 1 (- (rainbow-spotlight--window-body-pixel-width win) 2)))
         (low 20) (high 10000) (best 20))
    (cl-loop repeat 18 do
             (let ((mid (floor (/ (+ low high) 2.0))))
               (rainbow-spotlight--set-font-height mid)
               (redisplay t)
               (if (<= (rainbow-spotlight--text-pixel-width) avail)
                   (setq best mid low (1+ mid))
                 (setq high (1- mid)))))
    (rainbow-spotlight--set-font-height best)))

(defun rainbow-spotlight--center ()
  "Ensure no margins/centering; we want full-width single line."
  (when (boundp 'visual-fill-column-mode)
    (ignore-errors (visual-fill-column-mode -1)))
  (let ((win (selected-window)))
    (set-window-margins win 0 0)))

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

;; Auto-refit on frame size change (GLOBAL hook; scans spotlight windows)
(defun rainbow-spotlight--on-size-change (frame)
  "Refit any Rainbow Spotlight windows on FRAME."
  (dolist (win (window-list frame 'no-mini))
    (with-selected-window win
      (when (derived-mode-p 'rainbow-spotlight-mode)
        (rainbow-spotlight--center)
        (rainbow-spotlight--fit-to-window-width frame)))))

;; Defer initial fit until AFTER fullscreening
(defun rainbow-spotlight--initial-fit-deferred (buf)
  "Run a first fit after the window layout has settled."
  (when (buffer-live-p buf)
    (let ((win (get-buffer-window buf t)))
      (when (window-live-p win)
        (with-selected-window win
          (with-current-buffer buf
            (rainbow-spotlight-recompute-font)))))))

;; ==== Mode ====================================================================
(defvar rainbow-spotlight-mode-map
  (let ((m (make-sparse-keymap)))
    (define-key m (kbd "q") #'rainbow-spotlight-quit)
    (define-key m (kbd "R") #'rainbow-spotlight-recompute-font)
    m)
  "Keymap for rainbow-spotlight-mode.")

(define-derived-mode rainbow-spotlight-mode special-mode "Rainbow-Spotlight"
  "Fullscreen large-font rainbow shimmer mode (single-line, full-width)."
  (rainbow-spotlight--really-no-line-numbers)
  (setq-local cursor-type nil
              mode-line-format nil
              header-line-format nil
              truncate-lines t
              word-wrap nil
              show-trailing-whitespace nil
              line-spacing 0.25
              bidi-display-reordering nil
              bidi-paragraph-direction 'left-to-right)
  (buffer-face-mode 1)
  (rainbow-spotlight--center)
  (read-only-mode 1)
  ;; GLOBAL hook (no LOCAL arg), we'll remove it on quit:
  (add-hook 'window-size-change-functions #'rainbow-spotlight--on-size-change))

(defun rainbow-spotlight-recompute-font ()
  "Re-fit the single-line text to the current window width."
  (interactive)
  (rainbow-spotlight--center)
  (rainbow-spotlight--fit-to-window-width)
  (rainbow-spotlight--really-no-line-numbers)
  (message "Rainbow Spotlight: refit to window width"))

(defun rainbow-spotlight-quit ()
  "Exit the spotlight buffer and restore previous window layout."
  (interactive)
  (rainbow-spotlight--stop-timer)
  (remove-hook 'window-size-change-functions #'rainbow-spotlight--on-size-change)
  (let ((buf (current-buffer))
        (conf rainbow-spotlight--winconf))
    (when (window-configuration-p conf)
      (set-window-configuration conf))
    (kill-buffer buf)))

(defun rainbow-spotlight-region (beg end)
  "Display region from BEG to END in fullscreen rainbow spotlight (one line)."
  (interactive "r")
  (unless (use-region-p) (user-error "Select some text first"))
  (let* ((raw (buffer-substring-no-properties beg end))
         (text (replace-regexp-in-string "[ \t\n]+" " " raw))
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
      (setq-local truncate-lines t word-wrap nil)
      (setq buffer-read-only t))
    ;; Show fullscreen first:
    (pop-to-buffer buf)
    (delete-other-windows)
    ;; Defer the first fit until after layout settles:
    (run-with-idle-timer 0 nil #'rainbow-spotlight--initial-fit-deferred buf)
    ;; Aesthetic recenter:
    (recenter (/ (window-body-height) 2))))

(global-set-key (kbd "C-c r") #'rainbow-spotlight-region)
;; ---------------------------------------------------------------------------
