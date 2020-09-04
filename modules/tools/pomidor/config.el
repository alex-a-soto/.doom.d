;;; tools/pomidor/config.el -*- lexical-binding: t; -*-

(setq pomidor-seconds (* 25 60)) ; 25 minutes for the work period
(setq pomidor-break-seconds (* 5 60)) ; 5 minutes break time

(setq pomidor-breaks-before-long 4) ; wait 4 short breaks before long break
(setq pomidor-long-break-seconds (* 20 60)) ; 20 minutes long break time

(setq pomidor-sound-tick t
      pomidor-sound-tack t)
;      pomidor-sound-overwork (expand-file-name (concat pomidor-dir "overwork.wav"))
;      pomidor-sound-break-over (expand-file-name (concat (getenv "HOME") "/Music/overwork.wav")))

(setq pomidor-time-format "%l:%M %p")

(setq alert-default-style 'libnotify)

;; (setq global-mode-string
;;              '(:eval
;;                (format-time-string "Pomodoro: %M:%S " (pomidor-total-duration) t ) ))
