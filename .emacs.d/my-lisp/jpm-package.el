;;; Time-stamp: <2018-05-02 09:14:45 jpm>

;;; From JPH
(require 'jpm-base)

(when jpm-new-emacs
  (defun jpm-install-package (package)
    (unless (package-installed-p package)
      (message "Refreshing package database...")
      (package-refresh-contents)
      (package-install package)))
  ;; (package-initialize)
  (require 'package)
  ;; (setq package-enable-at-startup nil)
  ;; (setq package-archives `(("local-elpa" . ,(expand-file-name "elpa" user-path))))
  (add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/"))
  (add-to-list 'package-archives '("org" . "http://orgmode.org/elpa/"))
  (package-initialize nil)
  ;; (package-refresh-contents)
  ;; Simplify loading of packages from the network with use-package.el
  ;; (source: https://github.com/jwiegley/use-package)
  (jpm-install-package 'use-package)
  (require 'use-package)
  (use-package async :ensure t)
  (use-package jpm-helm :defer 1)
  (jpm-install-package 'which-key)
  (use-package which-key
               :config
               ;; Don't try to use unicode characters when in a non-windows environment
               (unless (display-graphic-p)
                 (setq which-key-dont-use-unicode t))
               (which-key-mode 1))
  )

(provide 'jpm-package)
  
