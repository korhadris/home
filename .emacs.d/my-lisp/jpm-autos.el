;;; jpm-autos.el --- automatically extracted autoloads
;;
;;; Code:


;;;### (autoloads nil "jpm-cxx" "jpm-cxx.el" (23275 16172 414755
;;;;;;  435000))
;;; Generated autoloads from jpm-cxx.el

(autoload 'my-insert-debug-statement "jpm-cxx" "\
Inserts a C++ debug statement.

\(fn)" t nil)

(autoload 'my-insert-cpp-debug-variable "jpm-cxx" "\
Inserts a C++ debug statement for a given variable

\(fn VARIABLE-NAME)" t nil)

(autoload 'my-insert-cpp-debug-variable-add "jpm-cxx" "\
Add a debug statement for a given variable to the current C++ debug

\(fn VARIABLE-NAME)" t nil)

(autoload 'my-insert-cpp-class "jpm-cxx" "\
Inserts a C++ class code skeleton

\(fn CLASS-NAME)" t nil)

(autoload 'my-insert-cpp-struct "jpm-cxx" "\
Inserts a C++ struct code skeleton

\(fn STRUCT-NAME)" t nil)

(autoload 'my-insert-cpp-enum "jpm-cxx" "\
Inserts a C++ enum code skeleton

\(fn ENUM-NAME)" t nil)

(autoload 'my-insert-cpp-include "jpm-cxx" "\
Inserts a #include <header> statement

\(fn INCLUDE-NAME)" t nil)

(autoload 'my-insert-cpp-namespace "jpm-cxx" "\
Inserts a namespace on the current two lines in the format:
namespace foo {
}  // namespace foo

\(fn NAMESPACE)" t nil)

(autoload 'my-insert-cpp-main "jpm-cxx" "\
Insert a C++ main function

\(fn)" t nil)

(autoload 'my-insert-c-include-guard "jpm-cxx" "\
Insert include guards in current file

\(fn)" t nil)

(autoload 'my-insert-cpp-function-debug "jpm-cxx" "\
Inserts a debug statement to print all arguments to a function

\(fn DEBUG)" t nil)

(autoload 'switch-h-cc "jpm-cxx" "\
Try to switch between paired header / code files.
e.g. something.h <-> something.cc or something.c
Depends on relative paths setup in `switch-paths-to-check'.
Inspired by `eassist-switch-h-cpp'

\(fn)" t nil)

(autoload 'my-get-function-name "jpm-cxx" "\
Get the name of the current function. Includes arguments and return type

\(fn SHORT-MODE)" t nil)

(autoload 'my-get-function-name-timer-toggle "jpm-cxx" "\
Toggles idle-timer call to `my-get-function-name-short'

\(fn)" t nil)

(autoload 'my-common-c "jpm-cxx" "\


\(fn)" nil nil)

(autoload 'c-cleanup "jpm-cxx" "\


\(fn)" t nil)

(autoload 'command-line-c-cleanup "jpm-cxx" "\


\(fn SWITCH)" nil nil)

;;;***

;;;### (autoloads nil "jpm-functions" "jpm-functions.el" (24355 38597
;;;;;;  210303 172000))
;;; Generated autoloads from jpm-functions.el

(autoload 'my-highlighting "jpm-functions" "\
Highlight dates, users (stored in `highlight-user-list'),
and some other messages (stored in `highlight-message-list').

\(fn)" t nil)

(autoload 'smart-beginning-of-line "jpm-functions" "\
Move point between beginning of indentation and beginning of line.

\(fn)" t nil)

(autoload 'isearch-forward-at-point "jpm-functions" "\
Interactive search forward for the word located around the point.

\(fn &optional REGEXP-P NO-RECURSIVE-EDIT)" t nil)

(autoload 'disable-word-wrap "jpm-functions" "\
Turns off word-wrapping

\(fn)" t nil)

(autoload 'enable-word-wrap "jpm-functions" "\
Turns on word-wrapping

\(fn)" t nil)

(autoload 'highlight-indendation "jpm-functions" "\
Highlights indentation levels.

\(fn)" t nil)

(autoload 'insert-comment "jpm-functions" "\
Insert a comment line with \"USERNAME YYYYMMDD \"

\(fn)" t nil)

(autoload 'insert-comment-todo "jpm-functions" "\
Insert a comment line with \"USERNAME YYYYMMDD TODO(user): \"

\(fn)" t nil)

(autoload 'close-all-other-buffers "jpm-functions" "\
Close all other file buffers (optionally kill all other buffers)
Buffers that are always omitted are: todo.org, *scratch*, and *Messages*

\(fn KILL-NON-FILE-BUFFERS)" t nil)

(autoload 'my-reverse-yank-pop "jpm-functions" "\
Traverse the kill-buffer in reverse direction.
Save as `yank-pop' with an argument of -1

\(fn)" t nil)

(autoload 'find-file-upwards "jpm-functions" "\
Recursively searches each parent directory starting from the
`default-directory' looking for a file with the name
file-to-find. Returns the path to the file or nill if not found.

\(fn FILE-TO-FIND)" nil nil)

(autoload 'my-find-tag "jpm-functions" "\
Checks to see if current `tags-file-name' is set for the current file/path.
If the TAGS file should be changed, it will be loaded before `find-tag' is called.

\(fn)" t nil)

(autoload 'my-numbered-yank "jpm-functions" "\
Paste from current kill-buffer and insert `my-numbering-counter'.
Prefix arg will set teh couter to its value.

\(fn PREFIX)" t nil)

(autoload 'my-find-files "jpm-functions" "\
Create a list of files in a given directory and sub-directories.

\(fn DIRECTORY)" t nil)

(autoload 'my-buffer-open "jpm-functions" "\


\(fn BUF-NAME)" nil nil)

(autoload 'my-grep-find-at-point "jpm-functions" "\
Run `grep-find' on the current word at `point'.

\(fn)" t nil)

(autoload 'my-run-command "jpm-functions" "\
Try to run an executable that has the same name as the current
buffer, without an extension

\(fn)" t nil)

(autoload 'my-just-one-space-all "jpm-functions" "\
Replaces multiple spaces within a line with a single space.
e.g. 'This line     has extra space.' -> 'This line ahs extra space.'

\(fn)" t nil)

(autoload 'my-bind-tab-to-separator "jpm-functions" "\
Bind the TAB key to `my-separator-or-tab', set `my-separator' and `my-tab-command'

\(fn SEPARATOR)" nil nil)

;;;***

;;;### (autoloads nil "jpm-other" "jpm-other.el" (23267 45763 633397
;;;;;;  528000))
;;; Generated autoloads from jpm-other.el

(autoload 'auto-recompile-file "jpm-other" "\
If this file has a compiled file, recompile it when saving.

\(fn)" nil nil)

(autoload 'my-m2k-mode "jpm-other" "\


\(fn)" t nil)

(autoload 'my-bluefile-mode "jpm-other" "\


\(fn)" t nil)

(autoload 'shell-send-tab "jpm-other" "\


\(fn)" t nil)

(autoload 'shell-send-backspace "jpm-other" "\


\(fn)" t nil)

(autoload 'my-org-clock-check "jpm-other" "\
Check today's clock times and display them in a *Clock-check* buffer

\(fn)" t nil)

(autoload 'my-org-login-time "jpm-other" "\
Display the login times for the current user

\(fn)" t nil)

(autoload 'my-org-week-format "jpm-other" "\
Modify the week format returned by `org-clock-special-range'.
  e.g. '23 - 27 Oct' or '30 Oct - 03 Nov'.

\(fn BASE-FORMAT)" nil nil)

(autoload 'openoffice-external "jpm-other" "\
Open external program, defined by `openoffice-command', for current file.

\(fn)" nil nil)

(autoload 'pdf-external "jpm-other" "\
Open external program, defined by `external-pdf-command', for current file.

\(fn)" nil nil)

(autoload 'jpm-calc-yank "jpm-other" "\
Version of `calc-yank' that will try to grab from the primary
selection before the kill-ring

\(fn)" t nil)

(autoload 'jpm-gdb-command-line "jpm-other" "\


\(fn SWITCH)" nil nil)

;;;***

;;;### (autoloads nil "jpm-print" "jpm-print.el" (23267 46256 701163
;;;;;;  956000))
;;; Generated autoloads from jpm-print.el

(autoload 'my-print-buffer-with-faces "jpm-print" "\
Pretty print buffer to ~/emacs_print.ps and open with a PDF/PS viewer.

\(fn)" t nil)

(autoload 'my-print-buffer-with-faces-full "jpm-print" "\
Pretty print full page (to .ps).

\(fn)" t nil)

;;;***

;;;### (autoloads nil "jpm-python" "jpm-python.el" (23267 46681 604929
;;;;;;  944000))
;;; Generated autoloads from jpm-python.el

(autoload 'my-python-shell "jpm-python" "\


\(fn)" t nil)

(autoload 'jpm-comment "jpm-python" "\


\(fn)" t nil)

(autoload 'jpm-uncomment "jpm-python" "\


\(fn)" t nil)

(autoload 'my-py-help-at-point "jpm-python" "\
Launch PyDOC on the Word at Point

\(fn W)" t nil)

(autoload 'jpm-insert-python-main "jpm-python" "\
Insert a Python main function

\(fn)" t nil)

;;;***

(provide 'jpm-autos)
;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; jpm-autos.el ends here
