;;; Time-stamp: <2018-05-02 08:48:43 jpm>

;; Specific settings for `ido-mode'
(when (feature-ready 'ido)
  (defun ido-save-history ()
    "JPM Overridden `ido-save-history'.
Added check that file is writable before trying to save.

Save Ido history and cache information between sessions."
    (interactive)
    (if (not (file-writable-p ido-save-directory-list-file))
        (message (concat "Unable to save ido file: " ido-save-directory-list-file))
      (when (and ido-last-directory-list ido-save-directory-list-file)
        (let ((buf (get-buffer-create " *ido session*"))
              (version-control 'never))
          (unwind-protect
              (with-current-buffer buf
                (erase-buffer)
                (insert ";;; -*- coding: utf-8 -*-\n")
                (setq buffer-file-coding-system 'utf-8)
                (ido-pp 'ido-last-directory-list)
                (ido-pp 'ido-work-directory-list)
                (ido-pp 'ido-work-file-list)
                (ido-pp 'ido-dir-file-cache "\n\n ")
                (if (listp ido-unc-hosts-cache)
                    (ido-pp 'ido-unc-hosts-cache)
                  (insert "\n;; ----- ido-unc-hosts-cache -----\nt\n"))
                (kill-buffer buf))))))))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(provide 'jpm-ido)
