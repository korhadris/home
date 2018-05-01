;;; Time-stamp: <2018-04-27 14:35:08 jpm>

;; Will only provide `jpm-helm' if running a supported emacs.

(require 'jpm-base)

(defvar jpm-helm-ready nil
  "Will be true if helm was successfully loaded")

(when jpm-new-emacs
  (use-package helm :ensure t)
  (require 'helm-config)
  (require 'helm-regexp)
  (define-key helm-map (kbd "<tab>") 'helm-execute-persistent-action) ; rebind tab to run persistent action
  (define-key helm-map (kbd "C-i") 'helm-execute-persistent-action) ; make TAB works in terminal
  (define-key helm-map (kbd "C-z")  'helm-select-action) ; list actions using C-z
  (define-key helm-command-prefix (kbd "o") 'helm-occur)
  (define-key helm-command-prefix (kbd "s") 'helm-do-grep)
  (define-key helm-command-prefix (kbd ":") 'helm-eval-expression-with-eldoc)
  (helm-mode 1)
  (setq jpm-helm-ready t)
  (global-set-key (kbd "C-;") 'helm-command-prefix)
  (global-unset-key (kbd "C-x c"))
  (global-set-key (kbd "M-x") 'helm-M-x)
  (global-set-key (kbd "M-y") 'helm-show-kill-ring)
  (message "Helm loaded"))

(provide 'jpm-helm)
