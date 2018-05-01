;;; Time-stamp: <2018-04-27 16:31:15 jpm>

;;; Elisp specific settings
;;;###autoload
(defun auto-recompile-file ()
  "If this file has a compiled file, recompile it when saving."
  (if (file-exists-p (concat (buffer-file-name) "c"))
      (byte-compile-file (buffer-file-name))))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; My M2k mode
;;;###autoload
(defun my-m2k-mode ()
  (interactive)
  (if (feature-ready 'm2k-script)
      (m2k-script-mode)
    (c++-mode))
  (my-highlighting))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; My bluefile mode
;;;###autoload
(defun my-bluefile-mode ()
  (interactive)
  (if (feature-ready 'bluefile)
      (bluefile-mode)))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; Shell/term specific settings
;;;###autoload
(defun shell-send-tab ()
  (interactive)
  (comint-send-input t t)
  (process-send-string (get-buffer-process (current-buffer)) "\t"))

;;;###autoload
(defun shell-send-backspace ()
  (interactive)
  (comint-send-input t t)
  (process-send-string (get-buffer-process (current-buffer)) "\b"))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; Org mode specific settings
;;;###autoload
(defun my-org-clock-check ()
  "Check today's clock times and display them in a *Clock-check* buffer"
  (interactive (let* ((line (buffer-substring-no-properties
                             (point-at-bol) (point-at-eol)))
                      (start-time (if (string-match " \\([0-9:]*\\)]" line)
                                      (match-string-no-properties 1 line) ""))
                      (end-time (if (string-match "--.* \\([0-9:](\\)]" line)
                                    (match-string-no-properties 1 line) nil)))
                 (list (read-string "Start time: " start-time nil start-time)
                       (if end-time (read-string "End time: " end-time nil end-time) ""))))
  (save-excursion
    (beginning-of-line)
    (re-search-forward " [0-9:]*]" (point-at-eol) t 1)
    (replace-match (concat " " start-time "]"))
    (unless (equal "" end-time)
      (re-search-forward " [0-9:]*]" (point-at-eol) t 2)
      (replace-match (concat " " end-time "]"))
      (org-evaluate-time-range))))

;;;###autoload
(defun my-org-login-time ()
  "Display the login times for the current user"
  (interactive)
  (save-window-excursion
    (shell-command (concat "w " user-login-name) "*Login-time*")))

;;;###autoload
(defun my-org-week-format (base-format)
  "Modify the week format returned by `org-clock-special-range'.
  e.g. '23 - 27 Oct' or '30 Oct - 03 Nov'."
  (if (string-match "^week" (nth 2 base-format))
      (let* ((time-base (org-parse-time-string (car base-format)))
             (day (nth 3 time-base))
             (year (nth 4 time-base))
             (time-start (encode-time 0 0 0 day month year))
             (time-end (encode-time 0 0 0 (+ day 4) month year))
             (same-month (equal month (nth 4 time-end)))
             (start-str (format-time-string (if same-month "%d" "%d %b") time-start))
             (end-str (format-time-string "%d %b" time-end))
             (result (concat (nth 2 base-format)
                             " (" start-str " - " end-str ")")))
        (list (car base-format) (nth 1 base-format) result))
    base-format))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; OpenOffice files
;;;###autoload
(defun openoffice-external ()
  "Open external program, defined by `openoffice-command', for current file."
  (fundamental-mode)
  (start-process "External OpenOffice" nil openoffice-command (buffer-file-name))
  (kill-this-buffer))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; PDF files
;;;###autoload
(defun pdf-external ()
  "Open external program, defined by `external-pdf-command', for current file."
  (fundamental-mode)
  (start-process "External PDF viewer" nil external-pdf-command (buffer-file-name))
  (kill-this-buffer))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; Calc settings
;;;###autoload
(defun jpm-calc-yank ()
  "Version of `calc-yank' that will try to grab from the primary
selection before the kill-ring"
  (interactive)
  (require 'calc-yank)
  (calc-wrapper
   (calc-pop-push-record-list
    0 "yank"
    (let* ((primary-x (if (x-get-selection)
                          (substring-no-properties (x-get-selection))
                        ""))
           (thing (if (not (equal "" primary-x))
                      primary-x
                    (if (fboundp 'current-kill)
                        (current-kill 0 t)
                      (car kill-ring-yank-pointer)))))
      (if (eq (car-safe calc-last-kill) thing)
          (cdr calc-last-kill)
        (if (stringp thing)
            (let ((val (math-read-exprs (calc-clean-newlines thing))))
              (if (eq (car-safe val) 'error)
                  (progn
                    (setq val (math-read-exprs thing))
                    (if (eq (car-safe val) 'error)
                        (error "Bad format in yanked data")
                      val))
                val))))))))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; GDB command-line support
;;;###autoload
(defun jpm-gdb-command-line (switch)
  (let ((jpm-gdb-command (concat "gdb "
                                 (if jpm-new-emacs "-i=mi " "--annotate=3 ")
                                 "--args "
                                 (mapconcat 'identity command-line-args-left " "))))
    (setq command-line-args-left nil)
    (gdb jpm-gdb-command)))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(provide 'jpm-other)
