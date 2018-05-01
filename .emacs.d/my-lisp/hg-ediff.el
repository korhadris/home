;; Last modified: Time-stamp: <2018-04-13 14:21:00 jpm>

;; Add the following to your ~/.emacs or ~/.emacs.d/init.el
;; (if (require 'hg-ediff nil 'NOERROR)
;;     (add-to-list 'command-switch-alist '("-hg_ediff" . hg-ediff-command-line)))

(require 'button)
(require 'jpm-functions)

(defvar hg-ediff-buffer "*hg-ediff changes*")
(defvar hg-ediff-list1 '())
(defvar hg-ediff-list2 '())
(defvar hg-ediff-modified '())
(defvar hg-ediff-deleted '())
(defvar hg-ediff-added '())
(defvar hg-ediff-dir1 "")
(defvar hg-ediff-dir2 "")
(defvar hg-ediff-wait "1 sec")
(defvar hg-ediff-timer nil)
(defvar hg-ediff-keymap (make-sparse-keymap))

(define-key hg-ediff-keymap "\r" 'push-button)
(define-key hg-ediff-keymap [mouse-1] 'push-button)
(define-key hg-ediff-keymap [mouse-2] 'push-button)

