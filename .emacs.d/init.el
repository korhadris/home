; Time-stamp: <2019-03-23 08:43:14 josh>

;; Added by Package.el.  This must come before configurations of
;; installed packages.  Don't delete this line.  If you don't want it,
;; just comment it out by adding a semicolon to the start of the line.
;; You may delete these explanatory comments.
(require 'package)
(add-to-list 'package-archives (cons "melpa" "https://melpa.org/packages/") t)

(package-initialize)

(defvar user-directory "~/.emacs.d")

(defvar my-site-lisp-path (expand-file-name "site-lisp" user-directory)
  "Path to third-party lisp files")

(defvar my-lisp-path (expand-file-name "my-lisp" user-directory)
  "Path to my lisp files")

(add-to-list 'load-path my-site-lisp-path)
(add-to-list 'load-path my-lisp-path)

(setq custom-file (expand-file-name "custom.el" user-directory))
(load custom-file)

;; (add-to-list 'custom-theme-load-path "~/.emacs.d/themes/")
;; (if (or window-system (daemonp)) (load-theme 'misterioso))

;; Manual custom settings
(display-time)
(fset 'yes-or-no-p 'y-or-n-p)
(set-face-underline 'font-lock-warning-face "red")
(setq bookmark-default-file (expand-file-name "bookmarks" user-directory)
      bookmark-save-flag 1)
(add-hook 'write-file-hooks 'time-stamp)
(setq default-indentation 4)
;"Default indentation used in `highlight-indendation'.")
(defvar buffers-not-to-kill-list '("*scratch*" "*Messages*" "diary" "todo.org")
  "Buffers that will not be closed by `close-all-other-buffers'")
; (set-face-attribute 'default nil :height 150)
(defvar my-python-path (expand-file-name "~/python")
  "Default path for Python scripts.")
(setq tags-file-name (expand-file-name "~/TAGS"))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;; `require', `load', and `autoload' functions
(require 'cl) ; Common Lisp: Need for remove-if-not, etc.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;; ps-print settings
(setq ps-bottom-margin 36)
(setq ps-font-size (quote (7 . 9)))
(setq ps-footer-font-size (quote (10 . 10)))
(setq ps-footer-offset 12)
(setq ps-header-font-size (quote (10 . 10)))
(setq ps-header-lines 1)
(setq ps-header-offset 12)
(setq ps-header-title-font-size (quote (10 . 10)))
(setq ps-inter-column 48)
(setq ps-landscape-mode t)
(setq ps-left-margin 36)
(setq ps-line-number nil)
(setq ps-line-number-step 1)
(setq ps-multibyte-buffer nil)
(setq ps-number-of-columns 2)
(setq ps-right-margin 36)
(setq ps-top-margin 36)
(setq ps-zebra-stripes nil)
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
(defvar pdf-viewers '("okular" "kpdf" "evince")
  "List of PDF viewers to try and use. Used by `my-print-buffer-with-faces'.")
(defvar pdf-viewer ""
  "PDF Viewer used by `my-print-buffer-with-faces'. Defaults to \"\" until a PDF viewer has been found.")
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Global Functions
(defun feature-ready (feature)
  "Will load feature if it is not loaded.
Returns nil if feature cannot be loaded."
  (require feature nil 'NOERROR))

(defvar highlight-user-list '("josh" "jpm")
  "List of names that will be highlighted by `my-highlighting'.")

(defvar highlight-message-list '("TODO" "NOTE" "DEBUG" "UPDATE" "WARNING")
  "List of note messages that will be highlighted by `my-highlighting'.")

(defun my-highlighting ()
  "Highlight dates, users (stored in `highlight-user-list'),
and some other messages (stored in `highlight-message-list')."
  (interactive)
  (highlight-regexp (concat "\\b\\(" (mapconcat 'identity highlight-message-list "\\|") "\\)\\b") 'scroll-bar)
  ;; Highlights dates in the form of 20110427. Only works in the 20th
  ;; and 21st century... 'Cause otherwise I'll be dead and won't care. Morbid I know...
  (highlight-regexp "\\b\\(19\\|20\\)[0-9]\\{6,6\\}\\b" 'underline)
  ;; Highlights dates in the forms of XX/XX/XX, XXXX,XX/XX, XX/XX/XXXX,
  ;; XX-XX-XX, XXXX-XX-XX, XX-XX-XXXX
  (highlight-regexp "\\b[0-9]\\{1,4\\}[-/][0-9]\\{1,2\\}[-/][0-9]\\{1,4\\}\\b" 'underline)
  ;; Highlights dates in the forms of XX/MON/XX, XXXX/MON/XX, XX/MON/XXXX,
  ;; XX-MON-XX, XXXX-MON-XX, XX-MON-XXXX
  (highlight-regexp "\\b[0-9]\\{1,4\\}[-/][a-zA-Z]\\{3,4\\}[-/][0-9]\\{1,4\\}\\b" 'underline)
  ;; Highlights user names (along with their UPCASED versions)
  (let ((temp-highlight-list ()))
    (dolist (highlight-user highlight-user-list)
      (add-to-list 'temp-highlight-list highlight-user)
      (add-to-list 'temp-highlight-list (upcase highlight-user)))
    (highlight-regexp (concat "\\b\\(" (mapconcat 'identity temp-highlight-list "\\|") "\\)\\b") 'bold))
  ;; Highlights words written like *important* *really_important* *oops*
  (highlight-regexp "\\b\\*[a-zA-Z_]*\\*\\b" 'bold)
  )

(defun make-backup-file-name (file-name)
  "Create the non-numeric backup file name for `file-name'."
  (require 'dired)
  (if (file-exists-p (expand-file-name "~/backups"))
      (concat (expand-file-name "~/backups/")
              (dired-replace-in-string "/" "|" file-name))
    (concat file-name "~")))

(defun smart-beginning-of-line ()
  "Move point between beginning of indentation and beginning of line."
  (interactive)
  (let ((old-pos (point)))
    (back-to-indentation)
    (and (= old-pos (point))
         (beginning-of-line))))

;; I-search with initial contents
(defvar isearch-initial-string nil
  "Used in `isearch-set-initial-string' and `isearch-forward-at-point'.")

(defun isearch-set-initial-string ()
  "Helper function for `isearch-forward-at-point'"
  (remove-hook 'isearch-mode-hook 'isearch-set-initial-string)
  (setq isearch-string isearch-initial-string)
  (isearch-search-and-update))

(defun isearch-forward-at-point (&optional regexp-p no-recursive-edit)
  "Interactive search forward for the word located around the point."
  (interactive "P\np")
  (if regexp-p (isearch-forward regexp-p no-recursive-edit)
    (let* ((end (progn (skip-syntax-forward "w_") (point)))
           (begin (progn (skip-syntax-backward "w_") (point))))
      (if (eq begin end)
          (isearch-forward regexp-p no-recursive-edit)
        (setq isearch-initial-string (buffer-substring begin end))
        (add-hook 'isearch-mode-hook 'isearch-set-initial-string)
        (isearch-forward regexp-p no-recursive-edit)))))

(defun disable-word-wrap ()
  "Turns off word-wrapping"
  (interactive)
  (if visual-line-mode (visual-line-mode 0))
  (setq word-wrap nil)
  (toggle-truncate-lines 1))

(defun enable-word-wrap ()
  "Turns on word-wrapping"
  (interactive)
  (if visual-line-mode (visual-line-mode 0))
  (setq word-wrap nil)
  (if truncate-lines (toggle-truncate-lines -1)))

(defun highlight-indendation ()
  "Highlights indentation levels."
  (interactive)
  (let ((indentation (cond ((eq major-mode 'python-mode)
                            (if (boundp 'python-indent)
                                python-indent default-indentation))
                           ((eq major-mode 'c-mode)
                            (if (boundp 'c-basic-offset)
                                c-basic-offset default-indentation))
                           ((eq major-mode 'c++-mode)
                            (if (boundp 'c-basic-offset)
                                c-basic-offset default-indentation))
                           (default-indentation))))
    (font-lock-add-keywords nil `((,(format "\\( \\) \\{%s\\}" (- indentation 1)) 1 'fringe)))))

(defun get-comment-string-open ()
  "Get the opening comment string for the current mode."
                                        ; (interactive)
  (cond ((eq major-mode 'python-mode) "#")
        ((eq major-mode 'c-mode) "/*")
        ((eq major-mode 'c++-mode) "//")
        ((eq major-mode 'm2k-mode) "//")
        ((eq major-mode 'fortran-mode) "c")
        ((eq major-mode 'sh-mode) "#")
        ((eq major-mode 'm2khelp-mode) "@")
        ((eq major-mode 'latex-mode) "%")
        ((eq major-mode 'emacs-lisp-mode) ";")
        ((eq major-mode 'makefile-gmake-mode) "#")
        ("#")))

(defun get-comment-string-close ()
  "Get the closing comment string for the current mode."
                                        ; (interactive)
  (cond ((eq major-mode 'c-mode) " */")
        ("")))

(defun comment-start ()
  "Start a comment line with \"USERNAME YYYYMMDD \""
  (interactive)
  (insert (get-comment-string-open) " " (upcase user-login-name) " "
          (format-time-string "%Y%m%d") " "))

(defun insert-comment ()
  "Insert a comment line with \"USERNAME YYYYMMDD \""
  (interactive)
  (beginning-of-line)
  (newline-and-indent)
  (previous-line)
  (indent-for-tab-command)
  (comment-start))

(defun close-all-other-buffers ()
  "Close all other file buffers except those in `buffers-not-to-kill-list'."
  (interactive)
  (mapc 'kill-buffer (delq (get-buffer "todo.org")
                           (delq (current-buffer)
                                 (remove-if-not 'buffer-file-name (buffer-list))))))

(defun my-check-shell-command (command-name)
  "Check for the existence of a command using \"where\"."
  (not (equal (shell-command-to-string (concat "where " command-name)) "")))

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

(defun my-print-buffer-with-faces-full ()
  "Pretty print full page (to .ps)."
  (interactive)
  (let ((ps-font-size '(10 . 10))
        (ps-number-of-columns 1)
        (ps-landscape-mode nil))
    (my-print-buffer-with-faces)))

(when (feature-ready 'bluefile)
  (defun orig-find-file (filename &optional wildcards)
    "Edit file FILENAME.
Switch to a buffer bisiting file FILENAME,
creating one if none already exists."
    (interactive "FFind File: \np")
    (let ((value (find-file-noselect filename nil nil wildcards)))
      (if (listp value)
          (mapcar 'switch-to-buffer (nreverse value))
        (switch-to-buffer value))))
  (defun my-find-file (file-name &optional wildcards)
    "Overriding the default `find-file'.
The original `find-file' is re-implemented as `orig-find-file'."
    (interactive "FFind File: \np")
    (let ((file-ext (file-name-extension file-name)))
      (if (or (equal file-ext "tmp") (equal file-ext "prm"))
          (view-bluefile file-name)
        (orig-find-file file-name wildcards))))
  (fset 'find-file 'my-find-file))

(defun truncate-string-end (string n)
  "Truncate N characters from the end of STRING."
  (substring string 0 (max 0 (- (length string) n))))

(defun my-buffer-open (buf-name)
  (let ((buf-found nil))
    (loop for buffer in (buffer-list) do
          (if (equal buf-name (buffer-name buffer))
              (setq buf-found t)))
    buf-found))

(defun my-delayed-thing ()
  (if (my-buffer-open "*ediff-diff*")
      (run-at-time "2 sec" nil 'my-delayed-thing)
    (message "scratch buffer 2 is gone")))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;; Global key bindings (Lots of different ways to specify them eh?)
(global-set-key [(meta g)] 'goto-line)
(global-set-key (kbd "C-c C-d") 'show-all-diary-entries)
(define-key global-map "\C-x\C-d" 'dired)
(define-key global-map "\C-\M-q" 'view-mode)
(define-key global-map [delete] 'delete-char)
(define-key global-map (kbd "RET") 'newline-and-indent)
(define-key global-map (kbd "\C-c o") 'other-window)
(define-key global-map (kbd "\C-c b") 'switch-to-buffer)
(global-set-key [XF86Back] 'previous-buffer)
(global-set-key [XF86Forward] 'next-buffer)
(global-set-key [f5] 'revert-buffer)
(global-set-key [f6] 'ediff-buffers)
(global-set-key [f7] 'ispell)
(global-set-key [(shift f7)] 'ispell-word)
(global-set-key (kbd "C-c e b") 'ediff-buffers)
(global-set-key (kbd "C-c e d") 'ediff-directories)
(global-set-key (kbd "C-c e f") 'ediff-files)
(global-set-key (kbd "C-c e r") 'ediff-regions-linewise)
(global-set-key (kbd "C-c e v") 'ediff-revision)
(global-set-key (kbd "C-c v") 'toggle-viper-mode)
(global-set-key (kbd "C-c F") 'find-file-literally)
(global-set-key (kbd "C-c x") 'hexl-find-file)

(define-prefix-command 'rect-map)
(define-key rect-map (vector ?c) 'clear-rectangle)
(define-key rect-map (vector ?d) 'delete-rectangle)
(define-key rect-map (vector ?k) 'kill-rectangle)
(define-key rect-map (vector ?t) 'string-rectangle)
(define-key rect-map (vector ?y) 'yank-rectangle)
(global-set-key "\M-r" 'rect-map)

;; These depend on custom functions defined above
(global-set-key [home] 'smart-beginning-of-line)
(global-set-key "\M-s" 'isearch-forward-at-point)
(global-set-key "\C-c\C-s" 'isearch-forward-at-point)
(global-set-key (kbd "C-c c") 'comment-start)
(global-set-key (kbd "C-c i") 'insert-comment)
(global-set-key (kbd "C-x K") 'close-all-other-buffers)
(global-set-key (kbd "C-c P") 'my-print-buffer-with-faces-full)
(global-set-key (kbd "C-c p") 'my-print-buffer-with-faces)
(global-set-key (kbd "C-c C-p") 'my-print-buffer-with-faces)

;; These depend on functions in other files
(when (feature-ready 'my-rect)
  (global-set-key (kbd "C-x r n") 'my-rectangle-number-lines))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;; Custom extension mode settings
(setq auto-mode-alist (cons '("csh" . sh-mode) auto-mode-alist))
(setq auto-mode-alist (cons '("\\.h\\'" . c++-mode) auto-mode-alist))
(setq auto-mode-alist (cons '("\\.m2k\\'" . c++-mode) auto-mode-alist))
(setq auto-mode-alist (cons '("\\.hdr\\'" . c++-mode) auto-mode-alist))
(setq auto-mode-alist (cons '("\\.tbl\\'" . c++-mode) auto-mode-alist))
(setq auto-mode-alist (cons '("\\.atbl\\'" . python-mode) auto-mode-alist))
(setq auto-mode-alist (cons '("\\.tab\\'" . python-mode) auto-mode-alist))
(setq auto-mode-alist (cons '("\\.pyd\\'" . python-mode) auto-mode-alist))
(setq auto-mode-alist (cons '("\\.orgtbl\\'" . orgtbl-mode) auto-mode-alist))
(setq auto-mode-alist (cons '("\\.m\\'" . octave-mode) auto-mode-alist))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;; Python specific settings
(setq py-python-command "ipython")
(setq python-python-command "python")
(setq python-python-command "ipython")

(defun my-python-shell ()
  (interactive)
  (require 'ansi-color)
  (require 'comint)
  (python-shell))

(fset 'jpm-uncomment
      [?\C-a ?\C-  down ?\C-u ?\C-c ?# ?\M-m ?\C-g])

(defun jpm-comment ()
  (interactive)
  (beginning-of-line)
  (let ((beg(point)))
    (next-line)
    (comment-region beg (point))))

(defun jpm-comment ()
  (interactive)
  (let ((start(point)))
    (beginning-of-line)
    (let ((beg(point)))
      (next-line)
      (uncomment-region beg (point)))
    (goto-char start)))

(setq output-my-py-help-in-shell nil) ; nil: my-py-help-at-point will put output into its own window.
                                        ;   t: my-py-help-at-point will put output into *PYTHON* shell if it's running.
(defun my-py-help-at-point (w)
  "Launch PyDOC on the Word at Point"
  (interactive
   (list (let* ((word (thing-at-point 'word))
                (input (read-string
                        (format "pydoc entry%s: "
                                (if (not word) "" (format " (default %s)" word))))))
           (if (string= input "")
               (if (not word) (error "No pydoc args given")
                 word)                  ;sinon word
             input)                     ;sinon input
           )))
  (if output-my-py-help-in-shell
      (progn
        (save-window-excursion
          (setq cmd (concat "import pydoc\n"
                            "try: pydoc.help('" w "')\n"
                            "except: print 'No help available on:', \"" w "\""))
          (message cmd)
          (py-execute-string cmd)
          (set-buffer "*Python Output*")
          )
        (view-buffer "*Python Output*" 'kill-buffer)
        )
    (progn
      (save-window-excursion
        (shell-command (concat py-python-command " -SE -c \"from pydoc import help;help(\'" w "\')\"") "*PYDOCS*"))
      (view-buffer "*PYDOCS*" 'kill-buffer))))

(add-hook 'python-mode-hook
          '(lambda()
             (define-key python-mode-map (kbd "C-c C-c") 'jpm-comment)
             (define-key python-mode-map (kbd "C-;") 'jpm-comment)
             (define-key python-mode-map (kbd "M-;") 'jpm-uncomment)
             (define-key python-mode-map "\C-c\C-h" 'my-py-help-at-point)
             (define-key python-mode-map "\C-ch" 'my-py-help-at-point)
             (define-key python-mode-map "\C-c\C-f" 'my-py-help-at-point)
             (modify-syntax-entry ?_ "w" python-mode-syntax-table)
             (local-set-key (kbd "C-c C-s") 'isearch-forward-at-point)
             (local-set-key "\M-p" 'my-python-shell)
             (defconst ediff-diff-options "")
             (imenu-add-menubar-index)
             (highlight-indendation)
             (my-highlighting)
             (font-lock-add-keywords nil '(("\\b\\(false\\|true\\)\\b" 0 font-lock-warning-face)))))
(add-hook 'py-shell-mode-hook 'ansi-color-for-comint-mode-on)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;; Calendar/Diary settings
(setq diary-file "~/diary")
(add-hook 'today-visible-calendar-hook 'calendar-star-date)
(add-hook 'initial-calendar-window-hook 'mark-diary-entries)
(add-hook 'diary-mode-hook 'my-highlighting)
(add-hook 'diary-display-hook '(lambda()
                                 (my-highlighting)
                                 (appt-make-list)))

;; Load up diary, but do not display the buffer
(when (file-exists-p (expand-file-name diary-file)) (diary) (diary 0))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;; View mode settings
(add-hook 'view-mode-hook
          '(lambda ()
             (define-key view-mode-map [?j]
               'View-scroll-line-forward)
             (define-key view-mode-map [?k]
               'View-scroll-line-backward)
             (define-key view-mode-map [?b]
               'scroll-down)
             (define-key view-mode-map [?g]
               'beginning-of-buffer)
             (define-key view-mode-map [?G]
               'end-of-buffer)
             (define-key view-mode-map [?/]
               'isearch-forward)
             (define-key view-mode-map [?n]
               'isearch-repeat-forward)
             (define-key view-mode-map [?N]
               'isearch-repeat-backward)
             (define-key view-mode-map [?q]
               'kill-buffer)))

(defmacro do-not-exit-view-mode-unless-writable-advice (f)
  `(defadvice ,f (around do-not-exit-view-mode-unless-writable activate)
     (if (and (buffer-file-name)
              (not (file-writable-p (buffer-file-name))))
         (message "File is unwritable, so stay in view-mode.")
       ad-do-it)))

(do-not-exit-view-mode-unless-writable-advice view-mode-exit)
(do-not-exit-view-mode-unless-writable-advice view-mode-disable)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;; Info mode specific setting
(add-hook 'info-mode-hook
          '(lambda ()
             (local-set-key (kbd "j") 'View-scroll-line-forward)
             (local-set-key (kbd "k") 'View-scroll-line-backward)))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;; Dired mode specific settings
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


;; Tar mode specific settings
(add-hook 'tar-mode-hook
          '(lambda ()
             (define-key tar-mode-map [?j] 'tar-next-line)
             (define-key tar-mode-map [?k] 'tar-previous-line)
             (define-key tar-mode-map [?b] 'scroll-down)
             (define-key tar-mode-map (kbd "SPC") 'scroll-up)
             (define-key tar-mode-map [?/] 'isearch-forward)
             (define-key tar-mode-map [?l] 'tar-extract)))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;; Ediff settings
(setq ediff-split-window-function 'split-window-horizontally)
(setq ediff-window-setup-function 'ediff-setup-windows-plain)

(defun command-line-ediff (switch)
  (let ((file1 (pop command-line-args-left))
        (file2 (pop command-line-args-left)))
    (ediff file1 file2)))

(add-to-list 'command-switch-alist '("diff" . command-line-ediff))
(add-to-list 'command-switch-alist '("ediff" . command-line-ediff))
(add-to-list 'command-switch-alist '("-ediff" . command-line-ediff))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;; C/C++ settings

;; My own C(++) style.
(defconst jpm-c-style
  '("gnu"
    (c-offsets-alist . ((innamespace . 0)
                        (inextern-lang . 0)
                        (brace-list-open . 0)
                        (inclass . ++)
                        (access-label . -)
                        (substatement-open . 0)
                        (case-label . +)
                        ))))
(c-add-style "jpm" jpm-c-style)

(defun insert-debug-statement ()
  (interactive)
  (previous-line)
  (end-of-line)
  (newline-and-indent)
  (insert "std::cerr << \"" (upcase user-login-name) " DEBUG: ")
  (save-excursion (insert "\" << std::endl;")))

(defvar switch-paths-to-check
  '("" ; Same directory
    "../include/" ; Common include directory
    "../src/" ; Common src directory
    )
  "Paths to check with `switch-h-cc'.")
(defun switch-h-cc ()
  "Try to switch between .h and .cc files. Only works with .h and .cc files.
Depends on paths listed in `switch-paths-to-check'. Inspired by `eassist-switch-h-cpp'."
  (interactive)
  (let* ((ext (file-name-extension (buffer-file-name)))
         (other-ext (if (equal ext "h") "cc" "h"))
         (base-name (truncate-string-end (buffer-name) (length ext)))
         (other-name (concat base-name other-ext))
         (base-path default-directory))
    (unless
        (or
         (when (bufferp (get-buffer other-name)) (switch-to-buffer other-name))
         (loop for test-file-name in (mapcar (lambda (path)
                                               (concat base-path path other-name))
                                             switch-paths-to-check)
               when (file-exists-p test-file-name) return (find-file test-file-name)))
      (message (concat "There is no corresponding file for " base-name " (" other-name ").")))))

(defun my-common-c ()
  (c-set-style "jpm")
  (local-set-key (kbd "C-c <right>") 'hs-show-block)
  (local-set-key (kbd "C-c <left>") 'hs-hide-block)
  (local-set-key (kbd "C-c <up>") 'hs-hide-all)
  (local-set-key (kbd "C-c <down>") 'hs-show-all)
  (local-set-key (kbd "C-c <C-right>") 'hs-show-block)
  (local-set-key (kbd "C-c <C-left>") 'hs-hide-block)
  (local-set-key (kbd "C-c <C-up>") 'hs-hide-all)
  (local-set-key (kbd "C-c <C-down>") 'hs-show-all)
  (local-set-key (kbd "C-c C-s") 'isearch-forward-at-point)
  (local-set-key (kbd "C-c d") 'insert-debug-statement)
  (local-set-key (kbd "C-M-q") 'view-mode)
  (local-set-key (kbd "C-c TAB") 'switch-h-cc)
  (hs-minor-mode t)
  (my-highlighting)
  (highlight-indendation)
  (imenu-add-menubar-index)
; (font-lock-add-keywords nil doxygen-keywords)
  )

(add-hook 'c-mode-common-hook 'my-common-c)

(defun c-cleanup ()
  (interactive)
  (indent-region (point-min) (point-max))
  (delete-trailing-whitespace))

(defun command-line-c-cleanup (switch)
  (let ((base-dir default-directory))
    (while command-line-args-left
      (let ((filename (pop command-line-args-left)))
        (cd base-dir)
        (find-file filename)
        (c-cleanup)
        (save-buffer)
        ))
    (save-buffers-kill-terminal)))

(add-to-list 'command-switch-alist '("cclean" . command-line-c-cleanup))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;; Shell/Term settings
(add-hook 'shell-mode-hook 'ansi-color-for-comint-mode-on)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;; Fundamental mode settings
(add-hook 'fundamental-mode-hook 'my-highlighting)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;; TeX/LaTeX mode settings
(defvar tex-verbatim-environments
  '("verbatim" "verbatim*" "Verbatim" "Verbatim*"))

(add-hook 'latex-mode-hook
          '(lambda ()
             (local-set-key (kbd "C-c C-s") 'isearch-forward-at-point)))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;; hg vc mode settings
;; Override 23.1 with version from 23.2
(require 'vc-hg)
(defun vc-hg-annotate-command (file buffer &optional revision)
  "Execute \"hg annotate\" on FILE, inserting the contents in BUFFER.
Optional arg REVISION is a revision to annotate from."
  (vc-hg-command buffer 0 file "annotate" "-d" "-n" "--follow" "-u"
                 (when revision (concat "-r" revision))))

;; Update re to account for -u and --follow switch
(defconst vc-hg-annotate-re
  "^[ \t]*[a-zA-Z0-9]+ \\([0-9]+\\) \\(.\\{30\\}\\)\\(?:\\(: \\)\\|\\(?: +\\( *[^ ]+\\):\\)\\)")
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(add-hook 'indented-text-mode-hook
          '(lambda () (make-local-variable 'font-lock-defaults)
             (setq font-lock-defaults '(unit-cfg-font-lock-keywords t))))

(defun gdb-command-line (switch)
  (let ((program-name (pop command-line-args-left)))
    (gdb (concat "gdb --annotate=3 " program-name))))

(add-to-list 'command-switch-alist '("gdb" . gdb-command-line))
(add-to-list 'command-switch-alist '("-gdb" . gdb-command-line))


(when (equal default-directory "~/Documents/")
  (setq default-directory "~/")
  (setq command-line-default-directory "~/"))

(defun my-insert-date ()
  "Insert a date. The formatting depends on the current mode."
  (interactive)
  (let ((date-format (case major-mode
                       ('org-mode "<%Y-%m-%d %a>")
                       (t "%Y-%m-%d"))))
    (insert (format-time-string date-format))))

;; 2016-01-28

(add-hook 'org-mode-hook
          '(lambda ()
             (local-set-key (kbd "C-c i d") 'my-insert-date)))

(require 'jpm-helm)
