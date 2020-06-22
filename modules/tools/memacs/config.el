;;; Memacs functions
(defun as/get-chromehist ()
  (interactive)
  (shell-command "pkill chrome")
  (message "Killed Google Chrome")
  (shell-command "python3 ~/.bin/Memacs/bin/memacs_chrome.py -f '/home/alexander/.config/google-chrome/Default/History' -o '/home/alexander/2-Reference/2-Time/Archive/chromehist.org_archive'")
  (message (format-time-string "%Y-%m-%d-%H%M - Created chromehist.org_archive")))

(defun as/add-chromehist-to-agenda ()
  (interactive)
    (let ((buffer (current-buffer)))
      (with-temp-buffer
        (find-file "/home/alexander/1-Agenda/2-Time/chromehist.org")
        (org-agenda-file-to-front)
        (kill-buffer "chromehist.org")
        (switch-to-buffer buffer))))

(defun as/load-chromehist ()
  (interactive)
  (as/add-chromehist-to-agenda)
  (org-agenda nil "a")
  (org-agenda-archives-mode t)
  )


(defun as/write-chromehist ()
  (interactive)
  (let ((buffer (current-buffer)))
    (with-temp-buffer
      (org-agenda nil "a")
      (org-agenda-archives-mode t)
      (org-agenda-write (format-time-string "/home/alexander/2-Reference/2-Time/Journal/%Y/%m/%Y-%m-%d.org"))
      (switch-to-buffer buffer))
    ))

(defun as/memacs-chrome ()
  (interactive)
  (as/get-chromehist)
  (as/add-chromehist-to-agenda)
  (as/load-chromehist)
  (org-agenda nil "a"))
