(require 'rect)

(unless (boundp 'rectangle-number-lines)

  ;; Line numbers for `rectangle-number-line-callback'.
  (defvar rectangle-number-line-counter)

  (defun rectangle-number-line-callback (start _end format-string)
    (move-to-column start t)
    (insert (format format-string rectangle-number-line-counter))
    (setq rectangle-number-line-counter
          (1+ rectangle-number-line-counter)))

  (defun rectange--default-line-number-format (start end start-at)
    (concat "%"
            (int-to-string (length (int-to-string (+ (count-lines start end)
                                                     start-at))))
            "d "))
)

(defun my-rectangle-number-lines (start end start-at &optional format)
  "Insert numbers in front of the region-rectangle.

START-AT, if non-nil, should be a number from which to begin
counting.  FORMAT, if non-nil, should be a format string to pass
to `format' along with the line count.  When called interactively
with a prefix argument, prompt for START-AT and FORMAT."
  (interactive
   (let* ((start (region-beginning))
	      (end   (region-end))
	      (start-at (read-number "Number to count from: " 1)))
	 (list start end start-at
	       (read-string "Format string: "
                        (rectange--default-line-number-format
                         start end start-at))))
   (list (region-beginning) (region-end) 1 nil))
  (unless format
    (setq format (rectange--default-line-number-format start end start-at)))
  (let ((rectangle-number-line-counter start-at))
    (apply-on-rectangle 'rectangle-number-line-callback
                        start end format)))

(provide 'my-rect)

;;; my-rect.el ends here
