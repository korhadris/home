; Time-stamp: <2020-06-30 19:35:34 josh>

;; Custom vars from emacs configure
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(TeX-PDF-mode t)
 '(TeX-newline-function (quote newline-and-indent))
 '(abbrev-file-name "~/.emacs.d/abbrev_defs")
 '(appt-display-duration 55)
 '(appt-display-interval 1)
 '(archive-superior-buffer nil t)
 '(auto-compression-mode t nil (jka-compr))
 '(case-fold-search t)
 '(column-number-mode t)
 '(comint-move-point-for-output t)
 '(comint-scroll-to-bottom-on-input t)
 '(current-language-environment "UTF-8")
 '(custom-enabled-themes (quote (misterioso)))
 '(default-frame-alist
    (quote
     ((background-color . "Black")
      (foreground-color . "White")
      (cursor-color . "red")
      (width . 120)
      (height . 42)
      (vertical-scroll-bars)
      (horizontal-scroll-bars)
      (menu-bar-lines . 1))))
 '(delete-selection-mode t nil (delsel))
 '(diary-file "~/.emacs.d/diary")
 '(diff-mode-hook (quote (view-mode)))
 '(dired-copy-preserve-time t)
 '(dired-keep-marker-copy 67)
 '(display-time-mode t)
 '(european-calendar-style t)
 '(f90-do-indent 2)
 '(f90-if-indent 2)
 '(f90-type-indent 2)
 '(focus-follows-mouse t)
 '(fortran-line-length 131)
 '(fortran-structure-indent 2)
 '(fortran-tab-mode-default nil t)
 '(gdb-find-source-frame t)
 '(gdb-show-main t)
 '(gdb-use-colon-colon-notation t)
 '(global-auto-revert-ignore-buffer nil t)
 '(global-font-lock-mode t nil (font-lock))
 '(grep-find-command
   "find -L . -type f -not -name '*.d' -not -name '*.orig' -not -name '*#' -print0 | xargs -0 -e grep -nIH -e ")
 '(helm-M-x-fuzzy-match t)
 '(helm-apropos-fuzzy-match t)
 '(helm-buffers-fuzzy-matching t)
 '(helm-ff-file-name-history-use-recentf t)
 '(helm-menu-fuzzy-match t)
 '(helm-move-to-line-cycle-in-source t)
 '(helm-recentf-fuzzy-match t)
 '(helm-scroll-amount 8)
 '(helm-semantic-fuzzy-match t)
 '(helm-split-window-in-side-p t)
 '(ibuffer-always-compile-formats nil)
 '(ido-default-buffer-method (quote selected-window))
 '(ido-default-file-method (quote selected-window))
 '(ido-enable-flex-matching t)
 '(ido-mode (quote both) nil (ido))
 '(ido-save-directory-list-file "~/.emacs.d/ido.last")
 '(imaxima-fnt-size "Large")
 '(imaxima-pt-size 12)
 '(indent-tabs-mode nil)
 '(indicate-buffer-boundaries t)
 '(inhibit-startup-screen t)
 '(initial-frame-alist
   (quote
    ((background-color . "Black")
     (foreground-color . "White")
     (cursor-color . "red")
     (width . 120)
     (height . 42)
     (vertical-scroll-bars)
     (horizontal-scroll-bars))))
 '(kill-whole-line t)
 '(latex-run-command "pdflatex")
 '(lazy-highlight-cleanup nil)
 '(mark-diary-entries-in-calendar t)
 '(minibuffer-message-timeout 2 t)
 '(mouse-buffer-menu-maxlen 30)
 '(mouse-buffer-menu-mode-mult 10)
 '(mouse-wheel-follow-mouse t)
 '(mouse-wheel-mode t nil (mwheel))
 '(mouse-wheel-progressive-speed nil)
 '(msb-max-menu-items 30)
 '(next-line-add-newlines nil)
 '(org-babel-load-languages (quote ((emacs-lisp . t) (dot . t))))
 '(org-clock-history-length 10)
 '(org-clock-into-drawer "CLOCK")
 '(org-clock-mode-line-total (quote today))
 '(org-clock-modeline-total (quote today))
 '(org-clock-persist (quote today))
 '(org-enforce-todo-checkbox-dependencies t)
 '(org-enforce-todo-dependencies t)
 '(org-journal-dir "~/.emacs.d/journal/")
 '(org-link-frame-setup
   (quote
    ((vm . vm-visit-folder-other-frame)
     (gnus . gnus-other-frame)
     (file . find-file))))
 '(org-log-done nil)
 '(org-log-note-clock-out nil)
 '(org-return-follows-link t)
 '(package-selected-packages (quote (helm async helm-projectile which-key markdown-mode use-package)))
 '(org-time-clocksum-use-fractional t)
 '(org-todo-keyword-faces
   (quote
    (("STARTED" . "yellow")
     ("COMPLETED" . "green")
     ("REPORTED" . "blue"))))
 '(package-enable-at-startup nil)
 '(projectile-completion-system (quote helm))
 '(ps-bottom-margin 36)
 '(ps-font-size (quote (7 . 9)))
 '(ps-footer-font-size (quote (10 . 10)))
 '(ps-footer-offset 12)
 '(ps-header-font-size (quote (10 . 10)))
 '(ps-header-lines 1)
 '(ps-header-offset 12)
 '(ps-header-title-font-size (quote (10 . 10)))
 '(ps-inter-column 48)
 '(ps-landscape-mode t)
 '(ps-left-margin 36)
 '(ps-line-number nil)
 '(ps-multibyte-buffer nil)
 '(ps-right-margin 36)
 '(ps-top-margin 36)
 '(py-block-comment-prefix "# ")
 '(python-mode-hook (quote (imenu-add-menubar-index)) t)
 '(require-final-newline t)
 '(safe-local-variable-values
   (quote
    ((eval replace-string "||" "|" 1 -100)
     (eval setq p2
           (point))
     (eval skip-chars-forward "Local variables")
     (eval add-hook
           (quote write-file-hooks)
           (quote time-stamp)))))
 '(save-place t nil (saveplace))
 '(save-place-file "~/.emacs.d/emacs-places")
 '(save-place-limit 50)
 '(scroll-conservatively 1)
 '(send-mail-function (quote sendmail-send-it))
 '(sh-basic-offset 2)
 '(sh-indentation 2)
 '(shell-command-default-error-buffer nil t)
 '(show-paren-mode t nil (paren))
 '(show-paren-style (quote expression))
 '(show-trailing-whitespace nil)
 '(tab-width 4)
 '(tags-add-tables (quote ask-user))
 '(text-mode-hook (quote (turn-on-auto-fill text-mode-hook-identify)))
 '(tool-bar-mode nil)
 '(trace-buffer "*trace-output*")
 '(track-mouse nil t)
 '(uniquify-buffer-name-style (quote forward) nil (uniquify))
 '(vc-make-backup-files t)
 '(vc-svn-diff-switches "-x-bw")
 '(view-read-only t)
 '(visible-bell t)
 '(whitespace-style
   (quote
    (face tabs trailing lines-tail space-before-tab empty tab-mark))))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:inherit nil :stipple nil :background "Black" :foreground "White" :inverse-video nil :box nil :strike-through nil :overline nil :underline nil :slant normal :weight normal :height 160 :width normal :foundry "unknown" :family "Monospace"))))
 '(fringe ((t (:background "gray10"))))
 '(scroll-bar ((t (:background "grey" :foreground "#000000"))))
 '(show-paren-match ((t (:background "grey20"))))
 '(trailing-whitespace ((((class color) (background dark)) (:background "orange")))))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defgroup jpm nil "Custom settings for JPM's functions" :group 'local)
