(defvar iracer-process nil
  "Process used for i-Racer control. Set by `iracer-connect'")


(defvar iracer-last-speed 0
  "Used as the default speed when speed is not specified. Set by `iracer-get-speed'.")


(defvar iracer-last-direction 0
  "Used to reset direction after speed changes. Set by `iracer-command'.")


(defun iracer-connect (&optional port)
  "Connect to the serial port for the i-Racer and stores the process as `iracer-process'"
  (interactive "sComm port: ")
  (if (equal port nil)
      (setq port "/dev/rfcomm0"))
  (setq iracer-process (make-serial-process :port port :speed 9600)))


(defun iracer-check-connection ()
  "Checks `iracer-process' to see that the i-Racer process is open. Called by `iracer-command'."
  (if (processp iracer-process)
      (if (equal (process-status iracer-process) 'open)
          t
        (message "i-Racer process has closed. Please reconnect with `iracer-connect'")
        nil)
    (message "Please connect to i-Racer first with `iracer-connect'")
    nil))


(defun iracer-get-speed (speed)
  "Converts speed value (0-10) to (0,6-15) for sending speed with `iracer-command'.
'Stores `iracer-last-speed'."
  (let ((speed-value 0))
    (if speed
        (setq speed-value speed)
      (setq speed-value iracer-last-speed))
    (if (> speed-value 10)
        (setq speed-value 10)
      (if (< speed-value 0)
          (setq speed-value 0)))
    (setq iracer-last-speed speed-value)
    (if (> speed-value 0)
        (+ speed-value 5)
      speed-value)))


(defun iracer-command (direction-offset speed)
  "Sends a command to the i-Racer process. Stores the `iracer-last-direction'."
  (when (iracer-check-connection)
    (setq iracer-last-direction direction-offset)
    (message (format "Sending %X" (+ direction-offset (iracer-get-speed speed))))
    (process-send-string iracer-process
                        (string (+ direction-offset
                                   (iracer-get-speed speed))))))


(defun iracer-stop ()
  (interactive)
  (iracer-command 0 iracer-last-speed))


(defun iracer-wait (time)
  (if time (sleep-for time)))


(defun iracer-forward (&optional speed time)
  (interactive)
  (iracer-command 16 speed)
  (when time (iracer-wait time) (iracer-stop)))


(defun iracer-backward (&optional speed time)
  (interactive)
  (iracer-command 32 speed)
  (when time (iracer-wait time) (iracer-stop)))


(defun iracer-left (&optional speed time)
  (interactive)
  (iracer-command 48 speed)
  (when time (iracer-wait time) (iracer-stop)))


(defun iracer-right (&optional speed time)
  (interactive)
  (iracer-command 64 speed)
  (when time (iracer-wait time) (iracer-stop)))


(defun iracer-forward-left (&optional speed time)
  (interactive)
  (iracer-command 80 speed)
  (when time (iracer-wait time) (iracer-stop)))


(defun iracer-forward-right (&optional speed time)
  (interactive)
  (iracer-command 96 speed)
  (when time (iracer-wait time) (iracer-stop)))


(defun iracer-backward-left (&optional speed time)
  (interactive)
  (iracer-command 112 speed)
  (when time (iracer-wait time) (iracer-stop)))


(defun iracer-backward-right (&optional speed time)
  (interactive)
  (iracer-command 128 speed)
  (when time (iracer-wait time) (iracer-stop)))


(defun iracer-increase-speed ()
  (interactive)
  (iracer-command iracer-last-direction (+ iracer-last-speed 1)))


(defun iracer-decrease-speed ()
  (interactive)
  (iracer-command iracer-last-direction (- iracer-last-speed 1)))


(defun iracer-drive (direction speed)
  "Command the i-Racer to drive in the named direction at the given speed."
  (cond ((equal direction "forward") (iracer-forward speed))
        ((equal direction "backward") (iracer-backward speed))
        ((equal direction "left") (iracer-left speed))
        ((equal direction "right") (iracer-right speed))
        ((or (equal direction "left forward")
             (equal direction "forward left")) (iracer-forward-left speed))
        ((or (equal direction "right forward")
             (equal direction "forward right")) (iracer-forward-right speed))
        ((or (equal direction "left backward")
             (equal direction "backward left")) (iracer-backward-left speed))
        ((or (equal direction "right backward")
             (equal direction "backward right")) (iracer-backward-right speed))
        (iracer-stop)))


(defun iracer-drive-for (direction speed time)
  (interactive "sDirection: \nnSpeed (0-10): \nnTime: ")
  (iracer-drive direction speed)
  (iracer-wait time)
  (iracer-stop))


(defun iracer-example ()
  (interactive)
  (message "Start of example")
  (iracer-forward 2 2)
  (iracer-backward 2 2)
  (iracer-drive-for "forward" 2 2)
  (iracer-drive-for "backward" 2 2)
  (iracer-forward-right 2 2)
  (iracer-backward-left 2 2)
  (iracer-drive-for "forward right" 2 2)
  (iracer-drive-for "backward left" 2 2)
  (iracer-drive-for "right forward" 2 2)
  (iracer-drive-for "left backward" 2 2)
  (iracer-forward 2)
  (iracer-wait 0.5)
  (iracer-forward-left 10)
  (iracer-wait 0.2)
  (iracer-forward-right 10 1)
  (message "End of example"))


(defun iracer-remote-control (port)
  (interactive "sSerial Port: ")
  (iracer-connect port)
  (switch-to-buffer (generate-new-buffer "*i-Racer Control*"))
  (local-set-key (kbd "<kp-0>") 'iracer-stop)
  (local-set-key (kbd "<kp-1>") 'iracer-backward-left)
  (local-set-key (kbd "<kp-2>") 'iracer-backward)
  (local-set-key (kbd "<kp-3>") 'iracer-backward-right)
  (local-set-key (kbd "<kp-4>") 'iracer-left)
  (local-set-key (kbd "<kp-5>") 'iracer-stop)
  (local-set-key (kbd "<kp-6>") 'iracer-right)
  (local-set-key (kbd "<kp-7>") 'iracer-forward-left)
  (local-set-key (kbd "<kp-8>") 'iracer-forward)
  (local-set-key (kbd "<kp-9>") 'iracer-forward-right)
  (local-set-key (kbd "<kp-subtract>") 'iracer-decrease-speed)
  (local-set-key (kbd "<kp-add>") 'iracer-increase-speed)
  )

(provide 'iracer)
