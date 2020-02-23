;;; Time-stamp: <2016-01-30 20:49:39 josh>

;;; Global Functions
(defvar jpm-new-emacs (or (> emacs-major-version 24)
                          (and (= emacs-major-version 25)
                               (>= emacs-minor-version 3)))
  "Is the current version of Emacs >= 24.3")

;; Try to use cl-lib, but fall back to cl and set some function aliases
(if jpm-new-emacs
    (require 'cl-lib)
  (require 'cl)
  (defalias 'cl-loop 'loop)
  (defalias 'cl-remove-if-not 'remove-if-not))

(defun feature-ready (feature)
  "Will load feature if it is not loaded.
Returns nil if feature cannot be loaded."
  (require feature nil 'NOERROR))

(defcustom highlight-user-list '("josh" "jpm")
  "List of names that will be highlighted by `my-highlighting'."
  :group 'jpm
  :type '(repeat string))

(defcustom highlight-message-list '("TODO" "NOTE" "DEBUG" "UPDATE" "WARNING")
  "List of note messages that will be highlighted by `my-highlighting'."
  :group 'jpm
  :type '(repeat string))

(defun my-highlighting ()
  "Highlight dates, users (stored in `highlight-user-list'),
and some other messages (stored in `highlight-message-list')."
  (interactive)
  (highlight-regexp (concat "\\b\\(" (mapconcat 'identity highlight-message-list "\\|") "\\)\\b") 'scroll-bar)
  ;; Highlights dates in the form of 20110427. Only works in the 20th
  ;; and 21st century... 'Cause otherwise I'll be dead and won't care. Morbid I know...
  (highlight-regexp "\\b\\(19\\|20\\)[0-9]\\{2,2\\}[01][0-9][0-3][0-9]\\b" 'underline)
  ;; Highlights dates in the forms of XX/XX/XX, XXXX,XX/XX, XX/XX/XXXX,
  ;; XX-XX-XX, XXXX-XX-XX, XX-XX-XXXX
  (highlight-regexp "\\b[0-9]\\{1,4\\}[-/][0-9]\\{1,2\\}[-/][0-9]\\{1,4\\}\\b" 'underline)
  ;; Highlights dates in the forms of XX/MON/XX, XXXX/MON/XX, XX/MON/XXXX,
  ;; XX-MON-XX, XXXX-MON-XX, XX-MON-XXXX
  (highlight-regexp "\\b[0-9]\\{1,4\\}[-/][a-zA-Z]\\{3,4\\}[-/][0-9]\\{1,4\\}\\b" 'underline)
  ;; Highlights dates in the form of 2012:11:30::01:02:03(.012345) or 2012:11:30
  (highlight-regexp "\\b[0-9]\\{4,4\\}:[01][0-9]:[0-3][0-9]\\(:\\(:[0-9]\\{2,2\\}\\)\\{3,3\\}\\(\\.[0-9]+\\)?\\)?\\b" 'underline)
  ;; Highlights times in the form of 01:02:03(.012345)
  (highlight-regexp "\\b\\([0-1][0-9]\\|2[0-3]\\):[0-5][0-9]:[0-5][0-9]\\(\\.[0-9]+\\)?\\b" 'underline)
  ;; Highlights user names (along with their UPCASED versions)
  (let ((temp-highlight-list ()))
    (dolist (highlight-user highlight-user-list)
      (add-to-list 'temp-highlight-list highlight-user)
      (add-to-list 'temp-highlight-list (upcase highlight-user)))
    (highlight-regexp (concat "\\b\\(" (mapconcat 'identity temp-highlight-list "\\|") "\\)\\b") 'bold))
  ;; Highlights words written like *important* *really_important* *oops*
  (highlight-regexp "\\b\\*[a-zA-Z_]*\\*\\b" 'bold)
  ;; Highlight trailing whitespace
  (highlight-regexp " +$" 'fringe)
  ;; Highlight extra whitespace
  ;; (highlight-regexp "[^\s\n]\\(  +\\)" 'fringe) ; Highlights the first char too :(
  (font-lock-add-keywords nil '(("[^\s\n]\\(  +\\)" 1 'fringe))))

(defun make-backup-file-name (file-name)
  "Override built-in `make-backup-file-name' to store the path.
The path is separated kinda annoying | characters that have to be escaped, but the idea is that these characters are unlikely to be in a filename."
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
  (visual-line-mode 1))

(defcustom default-indentation 2
  "Default indentation used in `highlight-indentation'."
  :group 'jpm
  :type 'integer)

(defcustom default-python-indentation 4
  "Default Python indentation used int `highlight-indentation'."
  :group 'jpm
  :type 'integer)

(defun highlight-indendation ()
  "Highlights indentation levels."
  (interactive)
  (let ((indentation (cond ((eq major-mode 'python-mode)
                            (if (boundp 'python-indent)
                                python-indent default-python-indentation))
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
  (cond ((eq major-mode 'python-mode) "#")
        ((eq major-mode 'c-mode) "/*")
        ((eq major-mode 'c++-mode) "//")
        ((eq major-mode 'm2k-mode) "//")
        ((eq major-mode 'fortran-mode) "c")
        ((eq major-mode 'sh-mode) "#")
        ((eq major-mode 'm2khelp-mode) "@")
        ((eq major-mode 'latex-mode) "%")
        ((eq major-mode 'emacs-lisp-mode) ";;")
        ((eq major-mode 'lisp-interaction-mode) ";;")
        ((eq major-mode 'makefile-gmake-mode) "#")
        ("#")))

(defun get-comment-string-close ()
  "Get the closing comment string for the current mode."
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

(defun insert-comment-todo ()
  "Insert a comment line with \"USERNAME YYYYMMDD TODO: \""
  (interactive)
  (insert-comment)
  (insert "TODO: "))

;; These buffers will not be kiled when performing a `close-all-other-buffers'"
;; (setq bffers-not-to-kill-list '("*scratch*" "*Messages*" "todo.org"))
(defun close-all-other-buffers (kill-non-file-buffers)
  "Close all other file buffers (optionally kill all other buffers)
Buffers that are always omitted are: todo.org, *scratch*, and *Messages*"
  (interactive "P")
  (let ((buffers-to-kill (delq (current-buffer)
                               (delq (get-buffer "todo.org")
                                     (buffer-list)))))
    (if kill-non-file-buffers
        (setq buffers-to-kill (delq (get-buffer "*scratch*")
                                    (delq (get-buffer "*Messages*")
                                          buffers-to-kill)))
      (setq buffers-to-kill (cl-remove-if-not 'buffer-file-name buffers-to-kill)))
    (mapc 'kill-buffer buffers-to-kill))
  (message "All other file buffers closed"))

(defun my-check-shell-command (command-name)
  "Check for the existence of a command using \"which\"."
  (not (equal (shell-command-to-string (concat "which " command-name)) "")))

(defun truncate-string-end (string n)
  "Truncate N characters from the end of STRING."
  (substring string 0 (max 0 (- (length string) n))))

(defun my-reverse-yank-pop ()
  "Traverse the kill-buffer in reverse direction.
Save as `yank-pop' with an argument of -1"
  (interactive)
  (yank-pop -1))

(defun find-file-upwards (file-to-find)
  "Recursively searches each parent directory starting from the
`default-directory' looking for a file with the name
file-to-find. Returns the path to the file or nill if not found."
  (labels
      ((find-file-r (path)
                    (let* ((parent (file-name-directory path))
                           (possible-file (concat parent file-to-find)))
                      (cond
                       ((file-exists-p possible-file) possible-file)
                       ((or (null parent) (equal parent (directory-file-name parent))) nil) ; not found
                       (t (fild-file-r (directory-file-name parent))))))) ; continue recursion
    (find-file-r default-directory)))

(defun my-find-tag ()
  "Checks to see if current `tags-file-name' is set for the current file/path.
If the TAGS file should be changed, it will be loaded before `find-tag' is called."
  (interactive)
  (let ((new-tags-file (find-file-upwards "TAGS")))
    (unless (equal new-tags-file tags-file-name)
      (message "Loading new TAGS file: %s" new-tags-file)
      (visit-tags-table new-tags-file)
      (message "Done with `visit-tags-table'")))
  (call-interactively 'find-tag))

;;; Didn't do my-numbered-yank

;;; Didn't do my-find-files

(defun my-buffer-open (buf-name)
  (let ((buf-found nil))
    (loop for buffer in (buffer-list) do
          (if (equal buf-name (buffer-name buffer))
              (setq buf-found t)))
    buf-found))

(defun my-grep-find-at-point ()
  "Run `grep-find' on the current word at `point'."
  (interactive)
  (let* ((word (substring-no-properties (symbol-name (symbol-at-point))))
         (command (concat grep-find-command word)))
    (add-to-list 'grep-find-history command)
    (grep-find command)))

(defun my-run-command ()
  "Try to run an executable that has the same name as the current
buffer, without an extension"
  (interactive)
  (let* ((command (file-name-sans-extension buffer-file-name))
         (new-buffer-name (concat "*" (file-name-nondirectory- command) " output *"))
         (max-mini-window-height 0))
    (if (not (file-exists-p command))
        (message (concat "Cannot find executable: " command))
      (shell-command (concat "date -u +'**** Started at:    %F %T.%N ****%n';"
                             "time " command
                             "; date -u +'%n**** Completed at:  %F %T.%N ****'")
                     new-buffer-name)
      (display-buffer new-buffer-name))))

(defun my-just-one-space-all ()
  "Replaces multiple spaces within a line with a single space.
e.g. 'This line     has extra space.' -> 'This line ahs extra space.'"
  (interactive)
  (save-excursion
    (goto-char (point-min))
    (while (re-search-forward "\\([[:graph:]]\\)  *\\([[:graph:]]\)" nil t)
      (replace-math "\\1 \\2" nil nil)
      (beginning-of-line))))
  
(defun my-insert-date ()
  "Insert a date. The formatting depends on the current mode."
  (interactive)
  (let ((date-format (case major-mode
                       ('org-mode "<%Y-%m-%d %a>")
                       (t "%Y-%m-%d"))))
    (insert (format-time-string date-format))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(provide 'jpm-functions)
