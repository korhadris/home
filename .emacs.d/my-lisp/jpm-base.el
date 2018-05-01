;;; Time-stamp: <2018-05-01 09:31:14 jpm>

;; File: jpm-base.el
;; Author: JPM
;; This file should contain the minimum amount of functions needed to
;; start up emacs. Fucntions that can be autoloaded should be placed
;; in separate files such as `jpm-functions.el' that can be loaded
;; later.

;;; Global functions
(defvar jpm-new-emacs (not (version< emacs-version "25.3"))
  "Is the current version of Emacs >= 25.3")

(defun feature-ready (feature)
  "Will load feature if it is not laoded.
Returns nil if feature cannot be loaded."
  (require feature nil 'NOERROR))

(defun make-backup-file-name (file-name)
  "Override built-in `make-backup-file-name' to store the path as part of the
file name.
The path is separated by kinda annoying | characters that have to be escaped,
but the idea is that these characters are unlikely to be in a file name."
  (require 'dired)
  (if (file-exists-p (expand-file-name "~/backups"))
      (concat (expand-file-name "~/backups/")
              (dired-replace-in-string "/" "|" file-name))
    (concat file-name "~")))

(defun kill-visual-line(&optional unused)
  "Override `kill-visual-line' from `visual-line-mode' with normal `kill-line'
command."
  (interactive)
  (kill-line))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; Custom parameters
(defcustom highlight-user-list `("jpm" "josh")
  "List of usernames that will be highlighted by `my-highlighting'."
  :group 'jpm
  :type '(repeat string))

(defcustom highlight-message-list '("TODO" "NOTE" "DEBUG" "UPDATE" "WARNING")
  "List of note messages that will be higlighted by `my-highlighting'."
  :group 'jpm
  :type '(repeat string))

(defcustom default-indentation 2
  "Default indentation used in `highlight-indentation'."
  :group 'jpm
  :type 'integer)

(defcustom switch-paths-to-check
  '("" ; same directory
    "../include" ; common path
    "../src" ; common source
    "../inc" ; secondary include
    "../../src" ; secondary source
    )
  "Paths to check with `switch-h-cc'"
  :group 'jpm
  :type '(repeat string))

(defcustom pdf-viewers '("okular" "kpdf" "evince")
  "List of PDF viewers to try and use. Used by `my-print-buffer-with-faces'."
  :group 'jpm
  :type '(repeat string))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; C/C++ specific settings
(add-hook 'c-mode-common-hook 'my-common-c)
(add-to-list 'command-switch-alist '("cclean" . command-line-c-cleanup))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; Python specific settings
(setq py-python-command "ipython")
(setq python-python-command "ipython")
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; PS print settings
(defvar pdf-viewer ""
  "PDF Viewer used by `my-print-buffer-with-faces'.
Defaults to \"\" until a PDF viewer has been found.")

;; I don't like the default colors that are printed, so I changed them.
;; This could be done in customized variables, but I like them in this file.
(ps-extend-face '(diary "Blue2") 'MERGE)
(ps-extend-face '(font-lock-builtin-face "SteelBlue") 'MERGE)
(ps-extend-face '(font-lock-comment-face "chocolate4") 'MERGE)
(ps-extend-face '(font-lock-constant-face "SeaGreen") 'MERGE)
(ps-extend-face '(font-lock-doc-face "DarkMagenta") 'MERGE)
(ps-extend-face '(font-lock-function-name-face "DeepSkyBlue") 'MERGE)
(ps-extend-face '(font-lock-keyword-face "Blue2") 'MERGE)
(ps-extend-face '(font-lock-preprocessor-face "SteelBlue") 'MERGE)
(ps-extend-face '(font-lock-string-face "DarkMagenta") 'MERGE)
(ps-extend-face '(font-lock-type-face "ForestGreen") 'MERGE)
(ps-extend-face '(font-lock-variable-name-face "DarkGoldenrod") 'MERGE)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Elisp specific settings
(add-hook 'emacs-lisp-mode-hook
          '(lambda()
             (local-set-key (kbd "C-c C-c") 'comment-region)))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; Calendar/Diary specific settings
(add-hook 'today-visible-calendar-hook 'calendar-star-date)
(add-hook 'initial-calendar-window-hook 'mark-diary-entries)
(add-hook 'diary-mode-hook 'my-highlighting)
(add-hook 'diary-display-hook '(lambda()
                                 (my-highlighting)
                                 (appt-activate)))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; View mode settings
(add-hook 'view-mode-hook
          '(lambda ()
             (define-key view-mode-map [?j] 'View-scroll-line-forward)
             (define-key view-mode-map [?k] 'View-scroll-line-backward)
             (define-key view-mode-map [?b] 'scroll-down)
             (define-key view-mode-map [?g] 'beginning-of-buffer)
             (define-key view-mode-map [?G] 'end-of-buffer)
             (define-key view-mode-map [?/] 'isearch-forward)
             (define-key view-mode-map [?n] 'isearch-repeat-forward)
             (define-key view-mode-map [?N] 'isearch-repeat-backward)
             (define-key view-mode-map [?q] 'kill-buffer)))

(defmacro do-not-exit-view-mode-unless-writable-advice (f)
  `(defadvice ,f (around do-not-exit-view-mode-unless-writable activate)
     (if (and (buffer-file-name)
              (not (file-writable-p (buffer-file-name))))
         (message "File is unwritable, so stay in view-mode.")
       ad-do-it)))

(do-not-exit-view-mode-unless-writable-advice view-mode-exit)
(do-not-exit-view-mode-unless-writable-advice view-mode-disable)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; Info mode specific setting
(add-hook 'info-mode-hook
          '(lambda ()
             (local-set-key (kbd "j") 'View-scroll-line-forward)
             (local-set-key (kbd "k") 'View-scroll-line-backward)))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; Dired mode specific settings
(add-hook 'dired-mode-hook
          '(lambda ()
             (define-key dired-mode-map [?j] 'dired-next-line)
             (define-key dired-mode-map [?k] 'dired-previous-line)
             (define-key dired-mode-map [?b] 'scroll-down)
             (define-key dired-mode-map (kbd "SPC") 'scroll-up)
             (define-key dired-mode-map [?/] 'isearch-forward)
             (define-key dired-mode-map [?U] 'dired-unmark-all-marks)
             (define-key dired-mode-map [?l] 'dired-find-file)))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;; Tar mode specific settings
(add-hook 'tar-mode-hook
          '(lambda ()
             (define-key tar-mode-map [?j] 'tar-next-line)
             (define-key tar-mode-map [?k] 'tar-previous-line)
             (define-key tar-mode-map [?b] 'scroll-down)
             (define-key tar-mode-map (kbd "SPC") 'scroll-up)
             (define-key tar-mode-map [?/] 'isearch-forward)
             (define-key tar-mode-map [?l] 'tar-extract)))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;; Ediff settings
(setq ediff-split-window-function 'split-window-horizontally)
(setq ediff-window-setup-function 'ediff-setup-windows-plain)

(defun command-line-ediff (switch)
  (let ((file1 (pop command-line-args-left))
        (file2 (pop command-line-args-left)))
    (ediff file1 file2)))

(add-to-list 'command-switch-alist '("diff" . command-line-ediff))
(add-to-list 'command-switch-alist '("ediff" . command-line-ediff))
(add-to-list 'command-switch-alist '("-ediff" . command-line-ediff))
(add-to-list 'command-switch-alist '("-hg_ediff" . hg-ediff-command-line))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; Shell/Term settings
(add-hook 'shell-mode-hook 'ansi-color-for-comint-mode-on)
(add-hook 'shell-mode-hook '(lambda()
                              (local-set-key [tab] 'shell-send-tab)
                              (local-set-key [backspace] 'shell-send-backspace)
                              (visual-line-mode 0)))
(add-hook 'term-mode-hook '(lambda()
                             (visual-line-mode -1)))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; Org mode settings
(add-hook 'org-mode-hook '(lambda()
                            (local-set-key (kbd "C-c C-x k") 'my-org-clock-check)
                            (local-set-key (kbd "C-c C-x C-k") 'my-org-clock-check)
                            (local-set-key (kbd "C-c C-u") 'my-org-modify-clock)
                            (local-set-key (kbd "C-c w") 'my-org-login-time)
                            (advice-add 'org-clock-special-range
                                        :filter-return 'my-org-week-format)
                            (when (equal (buffer-name) "todo.org")
                              (text-scale-adjust 0)
                              (text-scale-increase 2))))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; Fundamental mode settings
(add-hook 'fundamental-mode-hook 'my-highlighting)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; Text mode settings
(add-hook 'text-mode-hook '(lambda()
                             (my-highlighting)
                             (turn-on-visual-line-mode)))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; TeX/LaTeX mode settings
(defvar tex-verbatim-environments
  '("verbatim" "verbatim*" "Verbatim" "Verbatim*"))

(add-hook 'latex-mode-hook
          '(lambda ()
             (local-set-key (kbd "C-c C-s") 'isearch-forward-at-point)))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; OpenOffice files
(defcustom openoffice-command "oofice"
  "Program used by `openoffice-external' to load OpenOffice files."
  :group 'jpm
  :risky t
  :type 'string)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; PDF files
(defcustom external-pdf-command "okular"
  "Program used by `pdf-external' to load PDF files."
  :group 'jpm
  :risky t
  :type 'string)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; Calc settings
(add-hook 'calc-mode-hook '(lambda()
                             (local-set-key [mouse-2] 'jpm-calc-yank)))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; Autoloads
(require 'jpm-autos)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(provide 'jpm-base)
