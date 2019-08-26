;;; Time-stamp: <2018-05-24 18:04:41 jpm>

;; File: jpm-cxx.el
;; Author: JPM
;; C/C++ specific settings

(require 'jpm-base)
(require 'jpm-functions)

(defconst jpm-c-style
  '("gnu"
    (c-basic-offset . 2)
    (c-offsets-alist . ((innamespace . 0)
                        (inextern-lang . 0)
                        (brace-list-open . 0)
                        (inclass . +)
                        (access-label . /)
                        (substatement-open . 0)
                        (case-label . +))))
  "JPM's preferred style. Similar to Google Style Guide.")

(c-add-style "jpm" jpm-c-style)

(defconst lg-c-style
  '("gnu"
    (c-basic-offset . 2)
    (c-offsets-alist . ((innamespace . 0)
                        (inextern-lang . 0)
                        (brace-list-open . 0)
                        (inclass . ++)
                        (access-label . -)
                        (substatement-open . 0)
                        (case-label . +))))
  "LG / M2k style.")
(c-add-style "lg" lg-c-style)

(defconst badger-c-style
  '("gnu"
    (c-basic-offset . 2)
    (c-offsets-alist . ((innamespace . 0)
                        (inextern-lang . 0)
                        (brace-list-open . 0)
                        (inclass . +)
                        (access-label . -)
                        (substatement-open . 0)
                        (case-label . +)
                        (statement-case-open . 0))))
  "Badger style.")
(c-add-style "badger" badger-c-style)

(defconst doxygen-keywords
  (list
   (list
    "\\([@\\\\]\\(file\\|detail\\(?:s?\\)\\|version\\|name\\|return\\|brief\\|note\\)\\)\\b"
    '(0 font-lock-keyword-face prepend))
   (list
    "\\([@\\\\]\\(attention\\|warning\\|todo\\|bug\\)\\)\\b"
    '(0 font-lock-warning-face prepend))
   (list
    (concat "\\([@\\\\]\\(param\\(?:\\s-*\\[\\(?:in\\|out\\|in,out\\)\\]\\)?"
            "\\|a\\|namespace\\|relates\\(also\\)?"
            "\\|var\\|def\\)\\)\\s-+\\(\\sw+\\)")
    '(1 font-lock-keyword-face prepend)
    '(4 font-lock-variable-name-face prepend))
   (list
    (concat "\\([@\\\\]\\(class\\|struct\\|union\\|exception\\|enum"
            "\\|throw\\|interface\\|protocol\\)\\)\\s-+\\(\\(\\sw\\|:\\)+\\)")
    '(1 font-lock-keyword-face prepend)
    '(3 font-lock-type-face prepend))
   (list
    "\\([@\\\\]b\\)\\s-+\\([^ \t\n]+\\)"
    '(1 font-lock-keyword-face prepend)
    '(2 'bold prepend))
   (list
    "\\([@\\\\][cp]\\)\\s-+\\([^ \t\n]+\\)"
    '(1 font-lock-keyword-face prepend)
    '(2 'underline prepend))
   (list
    "\\([@\\\\]e\\(m\\)?\\)\\s-+\\([^ \t\n]+\\)"
    '(1 font-lock-keyword-face prepend)
    '(3 'italic prepend))))

;;;###autoload
(defun my-insert-debug-statement ()
  "Inserts a C++ debug statement."
  (interactive)
  (forward-line -1)
  (end-of-line)
  (newline-and-indent)
  (insert "std::cerr << \"" (upcase user-login-name)
          " DEBUG: \" << __FILE__ << ':' << __LINE__\n << \" ")
  (c-indent-line-or-region)
  (save-excursion
    (insert "\" << '\\n';")))

;;;###autoload
(defun my-insert-cpp-debug-variable (variable-name)
  "Inserts a C++ debug statement for a given variable"
  (interactive "sVariable: ")
  (save-excursion
    (forward-line -1)
    (end-of-line)
    (newline-and-indent)
    (insert "std::cerr << \"" (upcase user-login-name)
            " DEBUG: \" << __FILE__ << ':' << __LINE__\n << \" "
            variable-name " = \" << " variable-name)
    (c-indent-line-or-region)
    (newline-and-indent)
    (insert "<< '\\n';")
    (c-indent-line-or-region)))
    
;;;###autoload
(defun my-insert-cpp-debug-variable-add (variable-name)
  "Add a debug statement for a given variable to the current C++ debug"
  (interactive "sVariable: ")
  (save-excursion
    (forward-line -1)
    (end-of-line)
    (newline-and-indent)
    (insert "<< \" " variable-name " = \" << " variable-name)
    (c-indent-line-or-region)))

;;;###autoload
(defun my-insert-cpp-class (class-name)
  "Inserts a C++ class code skeleton"
  (interactive "sClass name: ")
  (let ((start-point (point)))
    (forward-line 0)
    (save-excursion
      (insert "class " class-name " {\npublic:\n"
              class-name "();\n\n"
              "// Destructor\n// virtual ~" class-name "();\n\n"
              "// Copy constructor\n// " class-name "(const " class-name "& other);\n\n"
              "// Assignment operator\n// " class-name "& operator= (const "
              class-name "& other);\n\n"
              ;; "// std::string DebugString(const size_t indent = 0) const;\n\n"
              "protected:\n\nprivate:\n};  // class " class-name "\n\n")
      (indent-region start-point (point)))))

;;;###autoload
(defun my-insert-cpp-struct (struct-name)
  "Inserts a C++ struct code skeleton"
  (interactive "sStruct name: ")
  (let ((start-point (point)))
    (forward-line 0)
    (save-excursion
      (insert "struct " struct-name " {\n"
              struct-name "();\n"
              ;; "// Destructor\n// ~" struct-name "();\n\n"
              ;; "// Copy constructor\n// " struct-name "(const " struct-name "& other);\n\n"
              ;; "// Assignment operator\n// " struct-name "& operator= (const "
              ;; struct-name "& other);\n\n"
              ;; "// std::string DebugString(const size_t indent = 0) const;\n"
              "};  // struct " struct-name "\n\n")
      (indent-region start-point (point)))))

;;;###autoload
(defun my-insert-cpp-enum (enum-name)
  "Inserts a C++ enum code skeleton"
  (interactive "sEnum name: ")
  (let ((start-point (point)))
    (forward-line 0)
    (save-excursion
      (insert "enum " enum-name " {\n"
              "k" enum-name "Length\n"
              "};  // enum " enum-name "\n\n")
      (indent-region start-point (point)))))


;;;###autoload
(defun my-insert-cpp-include (include-name)
  "Inserts a #include <header> statement"
  (interactive "sInclude (iostream): ")
  (if (equal include-name "") (setq include-name "iostream"))
  (forward-line 0)
  (if (or (not (cl-search "." include-name))
          (file-exists-p (concat "/usr/include/" include-name)))
      (insert "#include <" include-name ">\n")
    (insert "#include \"" include-name "\"\n")))

;;;###autoload
(defun my-insert-cpp-namespace (namespace)
  "Inserts a namespace on the current two lines in the format:
namespace foo {
}  // namespace foo"
  (interactive "sNamespace: ")
  (forward-line 0)
  (insert "namespace " namespace " {\n}  // namespace " namespace "\n")
  (forward-line -1))

;;;###autoload
(defun my-insert-cpp-main ()
  "Insert a C++ main function"
  (interactive)
  (let ((start-point (point)))
    (forward-line 0)
    (insert "int main()\n{")
    (newline-and-indent)
    (save-excursion
      (insert "\nreturn 0;\n}  // main\n")
      (indent-region start-point (point)))))

;;;###autoload
(defun my-insert-c-include-guard ()
  "Insert include guards in current file"
  (interactive)
  (let ((guard-name
         (concat (upcase (replace-regexp-in-string
                          "[-.]" "_"
                          (file-name-nondirectory (buffer-file-name))))
                 "_")))
    (message guard-name)
    (save-excursion
      (goto-char (point-min))
      (insert "#ifndef " guard-name "\n#define " guard-name "\n\n")
      (goto-char (point-max))
      (insert "\n#endif  " (get-comment-string-open) " " guard-name
              (get-comment-string-close) "\n"))))

;;;###autoload
(defun my-insert-cpp-function-debug (debug)
  "Inserts a debug statement to print all arguments to a function"
  (interactive "P")
  (let ((function-string "")
        (function-name "")
        (start-point nil)
        (end-point nil)
        (arguments nil)
        (loop-index 0))
    (c-beginning-of-defun)
    (setq start-point (point))
    (search-forward "(")
    (backward-char)
    (re-search-backward "[^ ]") ; start-point 'LIMIT)
    (forward-char)
    (setq end-point (point))
    (re-search-backward "[ \n]")
    (forward-char)
    (setq function-name (buffer-substring-no-properties (point) end-point))
    (if debug
        (message (concat "function-name : '" function-name "'")))
    (search-forward "(")
    (setq start-point (point))
    (search-forward "{")
    (search-backward ")")
    (setq function-string (buffer-substring-no-properties start-point (point)))
    (if debug
        (message (concat "function-string : " function-string)))
    (setq arguments (split-string function-string " *,"))
    (search-forward "{")
    (end-of-line)
    (newline-and-indent)
    (setq start-point (point))
    (forward-line 0)
    (insert "std::cerr << \"" (upcase user-login-name)
            " DEBUG: \" << __FILE__ << ':' << __LINE__\n << \" "
            function-name " arguments:\"")
    (c-indent-line-or-region)
    (newline-and-indent)
    (cl-loop for arg in arguments do
             (setq arg (replace-regexp-in-string "\n" " " arg))
             (setq arg (replace-regexp-in-string ".*[ &*]+" "" arg))
             (if debug
                 (message (concat "Arg " (int-to-string loop-index) ": '" arg "'")))
             (insert "<< \"\\n    " arg ": \" << " arg)
             (c-indent-line-or-region)
             (newline-and-indent)
             (setq loop-index (1+ loop-index)))
    (insert "<< \'\\n\';")
    (c-indent-line-or-region)))

;;;###autoload
(defun switch-h-cc ()
  "Try to switch between paired header / code files.
e.g. something.h <-> something.cc or something.c
Depends on relative paths setup in `switch-paths-to-check'.
Inspired by `eassist-switch-h-cpp'"
  (interactive)
  (let* ((ext (file-name-extension (buffer-file-name)))
         (other-ext (cond ((equal ext "h") '("cc" "cpp" "c"))
                          ((equal ext "hh") '("cc" "cpp"))
                          ((equal ext "hpp") '("cpp" "cc"))
                          ((equal ext "c") '("h"))
                          ((equal ext "cc") '("hh" "h"))
                          ((equal ext "cpp") '("hpp" "hh" "h"))
                          ((equal ext "for") '("inc"))
                          ((equal ext "inc") '("for"))
                          (t nil)))
         (base-name (truncate-string-end (buffer-name) (length ext)))
         (base-path default-directory))
    (if other-ext
        (unless
            (cl-loop for other-name in (mapcar (lambda (try-ext)
                                                 (concat base-name try-ext))
                                               other-ext)
                     when (or
                           (when (bufferp (get-buffer other-name)) (switch-to-buffer other-name))
                           (cl-loop for test-file-name in (mapcar (lambda (path)
                                                                    (concat base-path path other-name))
                                                                  switch-paths-to-check)
                                    when (file-exists-p test-file-name) return (find-file test-file-name)))
                     return t)
          (message "There is no corresponding pair (header or body) file."))
      (message (format "Unknown extension: %s What is its better half?" ext)))))

;;;###autoload
(defun my-get-function-name (short-mode)
  "Get the name of the current function. Includes arguments and return type"
  (interactive "P")
  (let ((function-name "")
        (start-point nil))
    (save-excursion
      (c-beginning-of-defun)
      (setq start-point (point))
      (if short-mode
          (progn (search-forward "(")
                 (end-of-line)
                 (setq function-name
                       (replace-regexp-in-string "\n" " " (buffer-substring start-point (point)))))
        (search-forward "{")
        (re-search-backward "[^ {\n]")
        (forward-char)
        (setq function-name (buffer-substring start-point (point))))
      (message function-name))))

(defvar my-function-name-timer nil
  "Timer that calls `my-function-name-timer'")

(defun my-get-function-name-short ()
  "Calls `my-get-function-name' with the prefix arg set.
  For use with `my-function-name-timer'"
  (my-get-function-name t))

;;;###autoload
(defun my-get-function-name-timer-toggle ()
  "Toggles idle-timer call to `my-get-function-name-short'"
  (interactive)
  (if my-function-name-timer
      (progn (cancel-timer my-function-name-timer)
             (message "`my-get-function-name-timer' stopped.")
             (setq my-function-name-timer nil))
    (my-get-function-name-short)
    (setq my-function-name-timer
          (run-with-idle-timer 2 'REPEAT 'my-get-function-name-short))))

;;;###autoload
(defun my-common-c ()
  (when (buffer-file-name)
    (if (not (string-match "badger" (buffer-file-name)))
        (c-set-style "jpm")
      (c-set-style "badger")
      (whitespace-mode)))
  (local-set-key (kbd "C-c C-s") 'isearch-forward-at-point)
  (local-set-key (kbd "C-c d") 'my-insert-debug-statement)
  (local-set-key (kbd "C-c i a") 'my-insert-cpp-function-debug)
  (local-set-key (kbd "C-c i c") 'my-insert-cpp-class)
  (local-set-key (kbd "C-c i d") 'my-insert-cpp-debug-variable)
  (local-set-key (kbd "C-c i D") 'my-insert-cpp-debug-variable-add)
  (local-set-key (kbd "C-c i e") 'my-insert-cpp-enum)
  (local-set-key (kbd "C-c i g") 'my-insert-c-include-guard)
  (local-set-key (kbd "C-c i i") 'my-insert-cpp-include)
  (local-set-key (kbd "C-c i m") 'my-insert-cpp-main)
  (local-set-key (kbd "C-c i n") 'my-insert-cpp-namespace)
  (local-set-key (kbd "C-c i s") 'my-insert-cpp-struct)
  (local-set-key (kbd "C-M-q") 'view-mode)
  (local-set-key (kbd "C-c TAB") 'switch-h-cc)
  (local-set-key (kbd "M-.") 'my-find-tag)
  (local-set-key (kbd "C-c C-a") 'c-beginning-of-defun)
  (local-set-key (kbd "C-c C-e") 'c-end-of-defun)
  (local-set-key (kbd "C-c C-l") 'my-get-function-name)
  (local-set-key (kbd "C-c M-l") 'my-get-function-name-timer-toggle)
  (modify-syntax-entry ?_ "w" c-mode-syntax-table)
  (my-highlighting)
  (highlight-indendation)
  (unless (eq major-mode 'protobuf-mode)
    (imenu-add-menubar-index))
  (font-lock-add-keywords nil doxygen-keywords)
  (if (feature-ready 'projectile)
      (projectile-mode))
  (when buffer-file-name
    (unless (or (file-exists-p "Makefile")
                (file-exists-p "makefile"))
      (set (make-local-variable 'compile-command)
           (concat "g++ " buffer-file-name " -o "
                   (file-name-sans-extension buffer-file-name)
                   " ")))))


;;;###autoload
(defun c-cleanup ()
  (interactive)
  (untabify (point-min) (point-max))
  (indent-region (point-min) (point-max))
  (delete-trailing-whitespace))

;;;###autoload
(defun command-line-c-cleanup (switch)
  (let ((base-dir default-directory))
    (while command-line-args-left
      (let ((filename (pop command-line-args-left)))
        (cd base-dir)
        (find-file filename)
        (c-cleanup)
        (save-buffer)))
    (save-buffers-kill-terminal)))

(provide 'jpm-cxx)
