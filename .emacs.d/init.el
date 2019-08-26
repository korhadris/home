; Time-stamp: <2019-03-23 08:43:14 josh>

;; Added by Package.el.  This must come before configurations of
;; installed packages.  Don't delete this line.  If you don't want it,
;; just comment it out by adding a semicolon to the start of the line.
;; You may delete these explanatory comments.
(require 'package)
(add-to-list 'package-archives (cons "melpa" "https://melpa.org/packages/") t)

(package-initialize)

(defvar user-directory "~/.emacs.d")

(defvar user-path
  (expand-file-name (if (eq init-file-user "")
                        "~/.emacs.d"
                      (message "Using %s's init file" init-file-user)
                      (concat "/home/" init-file-user "/.emacs.d")))
  "Path that contains the user's Emacs files (init.el, etc.)")

(defun jpm-add-load-path (new-path)
  (if (file-exists-p new-path)
      (add-to-list 'load-path new-path)
    (message "WARNING: Unable to find path: %s (for load-path)" new-path)))

(jpm-add-load-path (expand-file-name "site-lisp" user-path))
(jpm-add-load-path (expand-file-name "my-lisp" user-path))

(setq custom-file (expand-file-name "custom.el" user-path))
(load custom-file)

;; Manual custom settings
(display-time)
(fset 'yes-or-no-p 'y-or-n-p)
;;;;(set-face-underline 'font-lock-warning-face "red")
;; Increase the max buffer size on 64-bit systems (See if it can compute 2^32)
(if (> (expt 2 32) 0) (setq large-file-warning-threshold 300000000))
;; Update timestamps when writing files.
(add-hook 'write-file-hooks 'time-stamp)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;; `require', `load', and `autoload' functions
(require 'jpm-base)
(require 'jpm-package)
(require 'jpm-ido)
;;(require 'jpm-hg)
(require 'jpm-unique nil 'NOERROR)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;; Global key bindings (Lots of different ways to specify them eh?)
;; I keep them to show the options
(define-key global-map "\C-x\C-d" 'dired)
(define-key global-map "\C-\M-q" 'view-mode)
(define-key global-map [delete] 'delete-char)
(define-key global-map (kbd "RET") 'newline-and-indent)
(define-key global-map (kbd "\C-c o") 'other-window)
(define-key global-map (kbd "\C-c b") 'switch-to-buffer)
(global-set-key [home] 'smart-beginning-of-line)
(global-set-key [(meta g)] 'goto-line)
(global-set-key "\M-s" 'isearch-forward-at-point)
(global-set-key "\C-c\C-s" 'isearch-forward-at-point)
(global-set-key [XF86Launch5] (lambda() (interactive) (find-file user-init-file)))
(global-set-key [XF86Launch6] (lambda() (interactive) (find-file (expand-file-name "~/todo.org"))))
(global-set-key [XF86Launch7] (lambda() (interactive) (switch-to-buffer "*scratch*")))
(global-set-key [XF86Launch8] (lambda() (interactive) (term "/bin/bash")))
(global-set-key [XF86Launch9] (lambda() (interactive) (term "/bin/bash")))
(global-set-key [XF86Back] 'previous-buffer)
(global-set-key [XF86Forward] 'next-buffer)
(global-set-key [f5] 'revert-buffer)
(global-set-key [f6] 'compile)
(global-set-key (kbd "<S-f6>") 'my-run-command)
(global-set-key [f7] 'ispell)
(global-set-key [(shift f7)] 'ispell-word)
(global-set-key (kbd "C-c e b") 'ediff-buffers)
(global-set-key (kbd "C-c e d") 'ediff-directories)
(global-set-key (kbd "C-c e f") 'ediff-files)
(global-set-key (kbd "C-c e r") 'ediff-regions-linewise)
(global-set-key (kbd "C-c e v") 'ediff-revision)

(global-set-key (kbd "C-c c") 'comment-start)
(global-set-key (kbd "C-c g") 'my-grep-find-at-point)
(global-set-key (kbd "C-c v") 'toggle-viper-mode)
(global-set-key (kbd "C-x K") 'close-all-other-buffers)
(global-set-key (kbd "C-c P") 'my-print-buffer-with-faces-full)
(global-set-key (kbd "C-c p") 'my-print-buffer-with-faces)
(global-set-key (kbd "C-c C-p") 'my-print-buffer-with-faces)
(global-set-key (kbd "C-c F") 'find-file-literally)
(global-set-key (kbd "C-c x") 'hexl-find-file)
(global-set-key (kbd "C-c y") 'my-numbered-yank)
(global-set-key (kbd "C-c C-y") 'my-numbered-yank)
(global-set-key (kbd "M-Y") 'my-reverse-yank-pop)
(global-set-key (kbd "C-c M-SPC") 'my-just-one-space-all)
(global-set-key (kbd "C-c i") (define-prefix-command 'my-insert-map))
(global-set-key (kbd "C-c i c") 'insert-comment)
(global-set-key (kbd "C-c i t") 'insert-comment-todo)

(unless jpm-new-emacs
  (global-set-key (kbd "C-;") (define-prefix-command 'my-misc-map))
  (global-set-key (kbd "C-; o") 'occur)
  (global-set-key (kbd "C-; s") 'find-grep)
  (global-set-key (kbd "C-; f") 'find-grep))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Custom extension mode settings
(add-to-list 'auto-mode-alist '("\\.atbl\\'" . python-mode))
(add-to-list 'auto-mode-alist '("\\.doc\\'" . text-mode))
(add-to-list 'auto-mode-alist '("\\.h\\'" . c++-mode))
(add-to-list 'auto-mode-alist '("\\.m2k\\'" . my-m2k-mode))
(add-to-list 'auto-mode-alist '("\\.m\\'" . octave-mode))
(add-to-list 'auto-mode-alist '("\\.orgtbl\\'" . orgtbl-mode))
(add-to-list 'auto-mode-alist '("\\.pyd\\'" . python-mode))
(add-to-list 'auto-mode-alist '("\\.tab\\'" . python-mode))
(add-to-list 'auto-mode-alist '("\\.tbl\\'" . c++-mode))
(add-to-list 'auto-mode-alist '("csh" . sh-mode))
(add-to-list 'auto-mode-alist '("Makefile" . makefile-mode))
(add-to-list 'auto-mode-alist '("\\.od[pst]$\\|\\.xlsx?$\\|\\.pptx?$" . openoffice-external))
(add-to-list 'auto-mode-alist '("/\\(mcr\\|home\\)/[^/]+\\.txt$\\|\\.mm$" . xmidas-mode))
(add-to-list 'auto-mode-alist '("\\.tmp\\'" . my-bluefile-mode))
(add-to-list 'auto-mode-alist '("\\.prm\\'" . my-bluefile-mode))
(add-to-list 'auto-mode-alist '("\\.proto\\'" . protobuf-mode))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(add-hook 'after-init-hook
          '(lambda() (message "Emacs took %s to initialize" (emacs-init-time))))

(setq frame-title-format (format "%s@%s[%d]" "%b" system-name (emacs-pid)))

(add-to-list 'command-switch-alist '("-hg_ediff" . hg-ediff-command-line))

(add-to-list 'command-switch-alist '("-jpm_gdb" . jpm-gdb-command-line))

(defun jpm-force-xmidas (foo)
  (interactive)
  (add-to-list 'auto-mode-alist '("\\.txt\\'" . xmidas-mode)))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(message "Done reading init.el")

;;  LocalWords:  el defvar eq init s's concat defun setq fset expt hg
;;  LocalWords:  timestamps autoload ido NOERROR dired kbd RET goto
;;  LocalWords:  isearch XF ispell ediff linewise hexl SPC atbl pyd
;;  LocalWords:  orgtbl tbl csh makefile openoffice xmidas tmp prm
;;  LocalWords:  proto protobuf pid gdb txt alist
