;;; Time-stamp: <2018-04-27 16:39:28 jpm>

(require 'jpm-base)

;;;###autoload
(defun my-print-buffer-with-faces ()
  "Pretty print buffer to ~/emacs_print.ps and open with a PDF/PS viewer."
  (interactive)
  (let ((filename (expand-file-name "~/emacs_print.ps"))
        (viewers-to-check pdf-viewers))
    (ps-print-buffer-with-faces filename)
    (while (and (equal pdf-viewer "") viewers-to-check)
      (if (my-check-shell-command (car viewers-to-check))
          (setq pdf-viewer (car viewers-to-check)))
      (if (equal pdf-viewer "")
          (setq pdf-viewer (read-string "PDF Viewer: ")))
      (if (my-check-shell-command pdf-viewer)
          (start-process "PDF-Viewer" nil pdf-viewer filename)
        (message (concat "Cannot find PDF Viewer: " pdf-viewer))
        (setq pdf-viewer "")))))

;;;###autoload
(defun my-print-buffer-with-faces-full ()
  "Pretty print full page (to .ps)."
  (interactive)
  (let ((ps-font-size '(10 . 10))
        (ps-number-of-columns 1)
        (ps-landscape-mode nil))
    (my-print-buffer-with-faces)))

(provide 'jpm-print)
