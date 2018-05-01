;;; Time-stamp: <2018-04-27 16:37:12 jpm>

;;; From JPH
(require 'jpm-base)

(when jpm-new-emacs
  ;; (package-initialize)
  (require 'package)
  ;; (setq package-enable-at-startup nil)
  (setq package-archives `(("local-elpa" . ,(expand-file-name "elpa" user-path))))
  (package-archives nil)
  ;; Simplify loading of packages from the network with use-package.el
  ;; (source: https://github.com/jwiegley/use-package)
  (unless (package-installed-p 'use-package)
    (message "Refreshing package database...")
    (package-refresh-contents)
    (package-install 'use-package))
  (require 'use-package)
  (use-package async :ensure t)

  ;; Don't try to use unicode characters when in a non-windows environment
  (unless (display-graphic-p)
    (setq which-key-dont-use-unicode t))
  (when (feature-ready 'which-key)
    (which-key-mode 1)))

(provide 'jpm-package)
  
