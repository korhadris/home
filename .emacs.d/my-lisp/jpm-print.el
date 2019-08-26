;;; Time-stamp: <2018-05-03 11:59:12 jpm>

(require 'jpm-base)
(require 'jpm-functions)

;;; PS print settings
(defvar pdf-viewer ""
  "PDF Viewer used by `my-print-buffer-with-faces'.
Defaults to \"\" until a PDF viewer has been found.")

;; I don't like the default colors that are printed, so I changed
;; them.  This could be done in customized variables, but I like them
;; in this file to keep the print settings together.
(ps-extend-face '(diary "Blue2") 'MERGE)
(ps-extend-face '(font-lock-builtin-face "SteelBlue") 'MERGE)
(ps-extend-face '(font-lock-comment-face "chocolate4") 'MERGE)
(ps-extend-face '(font-lock-constant-face "SeaGreen") 'MERGE)
(ps-extend-face '(font-lock-doc-face "DarkMagenta") 'MERGE)
(ps-extend-face '(font-lock-function-name-face "DarkCyan") 'MERGE)
(ps-extend-face '(font-lock-keyword-face "Blue2") 'MERGE)
(ps-extend-face '(font-lock-preprocessor-face "SteelBlue") 'MERGE)
(ps-extend-face '(font-lock-string-face "DarkMagenta") 'MERGE)
(ps-extend-face '(font-lock-type-face "ForestGreen") 'MERGE)
(ps-extend-face '(font-lock-variable-name-face "DarkGoldenrod") 'MERGE)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;###autoload
(defun my-print-buffer-with-faces (prefix)
  "Pretty print buffer to ~/emacs_print.ps and open with a PDF/PS viewer."
  (interactive "P")
  (let ((filename (expand-file-name "~/emacs_print.ps"))
        (viewers-to-check pdf-viewers)
        (pdf-process-name "PDF-Viewer"))
    (if (equal prefix nil)
        (ps-print-buffer-with-faces filename)
      (ps-print-buffer filename))
    (if (process-status pdf-process-name)
        (message (concat df-process-name " is still running"))
      (while (and (equal pdf-viewer "") viewers-to-check)
        (if (my-check-shell-command (car viewers-to-check))
            (setq pdf-viewer (car viewers-to-check)))
        (setq viewers-to-check (cdr viewers-to-check)))
      (if (equal pdf-viewer "")
          (setq pdf-viewer (read-string "PDF Viewer: ")))
      (if (equal pdf-viewer "")
          (message "Unable to find PDF viewer")
        (if (my-check-shell-command pdf-viewer)
            (start-process pdf-process-name nil pdf-viewer filename)
          (message (concat "Cannot find PDF Viewer: " pdf-viewer))
          (setq pdf-viewer ""))))))

;;;###autoload
(defun my-print-buffer-with-faces-full (prefix)
  "Pretty print full page (to .ps)."
  (interactive "P")
  (let ((ps-font-size '(10 . 10))
        (ps-number-of-columns 1)
        (ps-landscape-mode nil))
    (my-print-buffer-with-faces prefix)))

(provide 'jpm-print)
