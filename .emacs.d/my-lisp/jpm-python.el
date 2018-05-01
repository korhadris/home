;;; Time-stamp: <2018-05-01 09:31:17 jpm>

;;; Python specific functions

;;;###autoload
(defun my-python-shell ()
  (interactive)
  (require 'ansi-color)
  (require 'comint)
  (python-shell))

(fset 'jpm-uncomment
      [?\C-a ?\C-  down ?\C-u ?\C-c ?# ?\M-m ?\C-g])

;;;###autoload
(defun jpm-comment ()
  (interactive)
  (beginning-of-line)
  (let ((beg(point)))
    (next-line)
    (comment-region beg (point))))

;;;###autoload
(defun jpm-uncomment ()
  (interactive)
  (let ((start(point)))
    (beginning-of-line)
    (let ((beg(point)))
      (next-line)
      (uncomment-region beg (point)))
    (goto-char start)))

(setq output-my-py-help-in-shell nil) ; nil: my-py-help-at-point will put output into its own window.
                                        ;   t: my-py-help-at-point will put output into *PYTHON* shell if it's running.

;;;###autoload
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

;;;###autoload
(defun jpm-insert-python-main ()
  "Insert a Python main function"
  (interactive)
  (forward-line 0)
  (insert "def main(args):")
  (newline-and-indent)
  (save-excursion
    (insert "\n\n\nif __name__ == '__main__':")
    (newline-and-indent)
    (insert "main(sys.argv[1:])\n")))

;; Are these too late here?
(add-hook 'python-mode-hook
          '(lambda()
             (define-key python-mode-map (kbd "C-c C-c") 'jpm-comment)
             ;; (define-key python-mode-map (kbd "C-;") 'jpm-comment)
             ;; (define-key python-mode-map (kbd "M-;") 'jpm-uncomment)
             (define-key python-mode-map "\C-c\C-h" 'my-py-help-at-point)
             (define-key python-mode-map "\C-ch" 'my-py-help-at-point)
             (define-key python-mode-map "\C-c\C-f" 'my-py-help-at-point)
             (modify-syntax-entry ?_ "w" python-mode-syntax-table)
             (local-set-key (kbd "C-c C-s") 'isearch-forward-at-point)
             (local-set-key "\M-p" 'my-python-shell)
             (local-set-key (kbd "C-c i m") 'jpm-insert-python-main)
             (defconst ediff-diff-options "")
             (imenu-add-menubar-index)
             (highlight-indendation)
             (my-highlighting)
             ;; Highlight stupid Python's True/False vs. true/false
             (font-lock-add-keywords nil '(("\\b\\(false\\|true\\)\\b" 0 font-lock-warning-face)))
             (set (make-local-variable 'compile-command)
                  (concat "pylink " buffer-file-name))))
(add-hook 'py-shell-mode-hook 'ansi-color-for-comint-mode-on)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(provide 'jpm-python)
