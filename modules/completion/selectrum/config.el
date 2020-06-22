(selectrum-mode +1)

;; to make sorting and filtering more intelligent
(selectrum-prescient-mode +1)

(setq selectrum-num-candidates-displayed 20)

(defvar selectrum-swiper-history nil "Submission history for `selectrum-swiper'.")
(defun selectrum-swiper ()
  "Search for a matching line and jump to the beginning of its text.  Obeys narrowing."
  (interactive)
  (let* ((selectrum-should-sort-p nil)
         (line-choices (cl-loop
                        with minimum-line-number = (line-number-at-pos (point-min) t)
                        with buffer-text-lines = (split-string (buffer-string) "\n")
                        with number-format = (format "L%%0%dd: " (length (number-to-string (length buffer-text-lines))))
                        for txt in buffer-text-lines
                        for num from minimum-line-number to (+ minimum-line-number
                                                               (1- (length buffer-text-lines)))
                        unless (string-empty-p txt) ; Just skip empty lines.
                        collect (concat (format number-format num) txt)))
         ;; Get the matching line.
         (chosen-line (completing-read "Jump to matching line: " line-choices
                                       nil t nil 'selectrum-swiper-history))
         ;; Stop at the ":". It is followed by one " ".
         (line-number-prefix (seq-take-while (lambda (char)
                                               (not (char-equal ?: char)))
                                             chosen-line))
         ;; Get the corresponding line number, skipping the "L" in line-number-prefix.
         (chosen-line-number (string-to-number (substring line-number-prefix 1)))
         ;; Get the current line number for determining the travel distance.
         (current-line-number (line-number-at-pos (point) t)))

    (push-mark (point) t)
    ;; Manually edit history to remove line numbers.
    (setcar selectrum-swiper-history (substring chosen-line
                                                ;; Want after line-prefix followed by ": ".
                                                (+ (length line-number-prefix) 2)))
    (forward-line (- chosen-line-number current-line-number))
    (beginning-of-line-text 1)))
