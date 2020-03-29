;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

;; Place your private configuration here! Remember, you do not need to run 'doom
;; sync' after modifying this file!
;;
;; Here are some additional functions/macros that could help you configure Doom:
;;
;; - `load!' for loading external *.el files relative to this one
;; - `use-package' for configuring packages
;; - `after!' for running code after a package has loaded
;; - `add-load-path!' for adding directories to the `load-path', relative to
;;   this file. Emacs searches the `load-path' when you load packages with
;;   `require' or `use-package'.
;; - `map!' for binding new keys
;;
;; To get information about any of these functions/macros, move the cursor over
;; the highlighted symbol at press 'K' (non-evil users must press 'C-c g k').
;; This will open documentation for it, including demos of how they are used.
;;
;; You can also try 'gd' (or 'C-c g d') to jump to their definition and see how
;; they are implemented.


;;; Configuration
;;;; Personal Information
(setq user-full-name "Alexander Soto"
      user-mail-address "alexander.soto@gmail.com")
;;;; Fonts
;; Doom exposes five (optional) variables for controlling fonts in Doom. Here
;; are the three important ones:
;;
;; + `doom-font'
;; + `doom-variable-pitch-font'
;; + `doom-big-font' -- used for `doom-big-font-mode'; use this for
;;   presentations or streaming.
;;
;; They all accept either a font-spec, font string ("Input Mono-12"), or xlfd
;; font string. You generally only need these two:
(setq doom-font (font-spec :family "monospace" :size 14))

;;;; Default Theme
;; There are two ways to load a theme. Both assume the theme is installed and
;; available. You can either set `doom-theme' or manually load a theme with the
;; `load-theme' function. This is the default:

(setq doom-theme 'doom-one)

;;;; Constants
;; If you use `org' and don't want your org files in the default location below,
;; change `org-directory'. It must be set before org loads!

(setq org-directory "~/Sync/org/")
(setq org-inbox-file (concat org-directory "inbox.org"))
(setq org-journal-file (concat org-directory "journal.org"))

;; This determines the style of line numbers in effect. If set to `nil', line
;; numbers are disabled. For relative line numbers, set this to `relative'.

;;;; Display Luine Numbers
(setq display-line-numbers-type t)
;;;; Display time and battery
(display-time-mode 1)
(display-battery-mode 1)

;;;; Delete by moving to trash
(setq delete-by-moving-to-trash  t)

;;;; eww
(add-hook 'eww-after-render-hook 'eww-readable)
(add-hook 'eww-after-render-hook 'writeroom-mode)
(add-hook 'eww-after-render-hook 'visual-line-mode)

(setf org-agenda-bulk-custom-functions
      '((?n org-now-agenda-refile-to-now)
        (?P org-now-agenda-refile-to-previous-location)))

(defvar as/toggle-one-window--buffer-name nil
  "Variable to store the name of the buffer for which the `as/toggle-one-window'
   function is called.")
(defvar as/toggle-one-window--window-configuration nil
  "Variable to store the window configuration before `as/toggle-one-window'
   function was called.")
(defun as/toggle-one-window (&optional force-one-window)
  "Toggles the frame state between deleting all windows other than
   the current window and the windows state prior to that."
  (interactive "P")
  (if (or (null (one-window-p))
	        force-one-window)
	    (progn
	      (setq as/toggle-one-window--buffer-name (buffer-name))
	      (setq as/toggle-one-window--window-configuration (current-window-configuration))
	      (delete-other-windows))
    (progn
	    (when as/toggle-one-window--buffer-name
	      (set-window-configuration as/toggle-one-window--window-configuration)
	      (switch-to-buffer as/toggle-one-window--buffer-name)))))


(map! :leader
      :prefix "w"
      :map winner-mode-map
      :desc "Toggle Window" "SPC" #'as/toggle-one-window)

;;;; Fold
(map! :leader
      :desc "Fold" "TAB" #'+fold/toggle)


;;;; Add some hooks for saving buffers
 (add-hook 'focus-out-hook (lambda () (interactive)(save-some-buffers t)))
 ;; save when frame is closed
 (add-hook 'delete-frame-functions (lambda () (interactive)(save-some-buffers t)))

;;;; Enable narrow functions
    (put 'narrow-to-page 'disabled nil)
(put 'narrow-to-region 'disabled nil)
;;; Packages
;;;; Which Key
(use-package! which-key
  :diminish
  :config
  (setq which-key-sort-order #'which-key-prefix-then-key-order
	      which-key-sort-uppercase-first nil
	      which-key-add-column-padding 1
	      which-key-max-display-columns nil
	      which-key-idle-delay 0.0
	      which-key-special-keys nil
	      which-key-min-display-lines 7)
  (set-face-attribute 'which-key-local-map-description-face nil :weight 'bold)
  (which-key-setup-side-window-bottom)
  :custom
  (which-key-separator " ")
  (which-key-prefix-prefix "+"))
;;;; Company
(after! company
  (setq company-echo-delay 0)
  (setq company-idle-delay 0.1)
  (setq company-minimum-prefix-length 2)
  (setq company-tooltip-limit 20))

(custom-set-faces
 '(aw-leading-char-face
   ((t (:inherit ace-jump-face-foreground :height 3.0)))))

;;;; org-now
(after! org-now
  (setq org-now-location (quote ("~/Sync/org/now/now.org"))))

;;;; org-roam
(use-package! org-roam
  :commands (org-roam-insert org-roam-find-file org-roam-switch-to-buffer org-roam)
  :custom-face
  (org-roam-link ((t (:inherit org-link :foreground "#005200"))))
  :init
  (map! :leader
        :prefix "n"
        :desc "org-roam" "l" #'org-roam
        :desc "org-roam-insert" "i" #'org-roam-insert
        :desc "org-roam-switch-to-buffer" "b" #'org-roam-switch-to-buffer
        :desc "org-roam-find-file" "f" #'org-roam-find-file
        :desc "org-roam-show-graph" "g" #'org-roam-show-graph
        :desc "org-roam-insert" "i" #'org-roam-insert)

  (setq org-roam-directory "/home/alexander/Sync/org/notes/"
        org-roam-db-location "/home/alexander/Sync/org/notes/org-roam.db"))

;;;; company-org-roam
(use-package company-org-roam
  :when (featurep! :completion company)
  :config
  (set-company-backend! 'org-mode '(company-org-roam company-yasnippet company-dabbrev)))

(after! (org org-roam)
  (defun my/org-roam--backlinks-list (file)
    (if (org-roam--org-roam-file-p file)
        (--reduce-from
         (concat acc (format "- [[file:%s][%s]]\n"
                             (file-relative-name (car it) org-roam-directory)
                             (org-roam--get-title-or-slug (car it))))
         "" (org-roam-sql [:select [file-from]
                                   :from file-link                             :where (= file-to $s1)
                                   :and file-from :not :like $s2] file "%private%"))
      ""))
  (defun my/org-export-preprocessor (_backend)
    (let ((links (my/org-roam--backlinks-list (buffer-file-name))))
      (unless (string= links "")
        (save-excursion
          (goto-char (point-max))
          (insert (concat "\n* Backlinks\n" links))))))
  (add-hook 'org-export-before-processing-hook 'my/org-export-preprocessor))

;;;; Hyperbole
(eval-after-load "hyperbole"
  '(progn
     (define-key org-mode-map (kbd "<M-return>") nil)))

;;;; Notdeft

(defun notdeft-file-format (str)
  (when (string-match "^[^a-zA-Z0-9-]+" str)
    (setq str (replace-match "" t t str)))
  (when (string-match "[^a-zA-Z0-9-]+$" str)
    (setq str (replace-match "" t t str)))
  (while (string-match "[`'“”\"]" str)
    (setq str (replace-match "" t t str)))
  (while (string-match "[^a-zA-Z0-9-]+" str)
    (setq str (replace-match "-" t t str)))
  (setq str (downcase str))
  (and (not (string= "" str))
       (concat (format-time-string "%Y-%m-%d-%H%M") " " str)))

(setq notdeft-notename-function 'notdeft-file-format)


(defun as/notdeft-new-file-named ()
  (interactive)
  (let ((title (read-string "New note: ")))
    (notdeft-new-file-named nil title notdeft-template)
    (goto-char (point-min))
    (re-search-forward "^#\\+TITLE:.*$" nil t)
    (insert " " title)
    (re-search-forward "^#\\+DATE:.*$" nil t)
    (insert (format-time-string " [%Y-%m-%d-%H%M]"))
    (goto-char (point-min))
    (goto-line 4)
    ))


(after! notdeft
  (load "notdeft-example")
  (setq notdeft-xapian-program "/home/alexander/bin/Notdeft/notdeft-xapian")
  (setq notdeft-directories '("/home/alexander/Sync/org/notes"))
  (setq notdeft-time-format " %Y-%m-%d-%H%M")
  (setq notdeft-template
        "#+TITLE:
#+DATE:



* Related
* External Links
")

  )
;;;; pdf-tools
;;;; org
(use-package org
  :config
  (setq org-replace-disputed-keys t)

  (add-hook
   'org-mode-hook
   (lambda ()
     (visual-line-mode 1)))

  (setf org-blank-before-new-entry '((heading . nil) (plain-list-item . nil)))
  (setq org-ellipsis "⤵")
  (setq org-use-speed-commands t))

;;;;; org-todo-keywords
   (setq org-todo-keywords
	 (quote ((sequence "TODO(t)" "NEXT(n)" "|" "DONE(d)")
		 (sequence "WAITING(w)" "HOLD(h)" "DELEGATE(g)" "|" "CANCELLED(c)" "SCHEDULED(s)"))))

 (setq org-treat-S-cursor-todo-selection-as-state-change nil)

;;;;; org-todo-keyword-faces
   (setq org-todo-keyword-faces
        (quote (("TODO" :foreground "red" :weight bold)
	        ("NEXT" :foreground "#007cee" :weight bold)
	        ("DONE" :foreground "forest green" :weight bold)
	        ("WAITING" :foreground "orange" :weight bold)
          ("HOLD" :foreground "magenta" :weight bold)
	        ("DELEGATE" :foreground "purple" :weight bold)
	        ("CANCELLED" :foreground "red" :weight bold)
	        ("SCHEDULED" :foreground "forest green" :weight bold))))
;;;;; org-todo-state-tags-triggers
 (setq org-todo-state-tags-triggers
	 (quote (("CANCELLED" ("CANCELLED" . t))
		 ("WAITING" ("WAITING" . t))
		 ("HOLD" ("WAITING") ("HOLD" . t))
		 (done ("WAITING") ("HOLD"))
		 ("TODO" ("WAITING") ("CANCELLED") ("HOLD"))
		 ("NEXT" ("WAITING") ("CANCELLED") ("HOLD"))
		 ("DONE" ("WAITING") ("CANCELLED") ("HOLD")))))

 (setq org-use-fast-todo-selection t)

;;;;; org-refile
  (setq org-refile-targets '((nil :maxlevel . 9)
				                     (org-agenda-files :maxlevel . 9)))

(setq org-refile-use-outline-path 'file)
(setq org-outline-path-complete-in-steps nil)

(setq org-refile-allow-creating-parent-nodes (quote confirm))
(setq org-refile-target-verify-function 'as/verify-refile-target)

;;;;; org-babel
   (defadvice org-babel-execute-src-block (around load-language nil activate)
     "Load language if needed"
     (let ((language (org-element-property :language (org-element-at-point))))
       (unless (cdr (assoc (intern language) org-babel-load-languages))
	 (add-to-list 'org-babel-load-languages (cons (intern language) t))
	 (org-babel-do-load-languages 'org-babel-load-languages org-babel-load-languages))
       ad-do-it))

  (setq org-confirm-babel-evaluate nil)

   (setq org-src-fontify-natively t
	 org-src-tab-acts-natively t
	 org-src-preserve-indentation t
	 org-src-window-setup 'current-window)

   (setq org-confirm-babel-evaluate nil)
   (setq org-babel-results-keyword "results")

 (defun as/display-inline-images ()
     (condition-case nil
	 (org-display-inline-images)
       (error nil)))

   (add-hook 'org-babel-after-execute-hook 'as/display-inline-images 'append)

;;;;; org-tag-alist
(setq org-tag-alist (quote
                     (
                      ("NOTE" . ?n)
                      ("resilient" . ?r)
                      ("learning" . ?l)
                      ("work" . ?w)
                      ("personal" . ?p)
                      )))

   (setq org-fast-tag-selection-include-todo t)
(setq org-fast-tag-selection-single-key (quote expert))
;;;;; org-capture templates
(require 'org-protocol)
(require 'org-expiry)

  (defun org-journal-find-location ()
  (org-journal-new-entry t)
  (goto-char (point-min)))

(add-hook 'org-capture-mode-hook (lambda () (call-interactively 'org-store-link)))

(setq org-capture-templates
	    (quote (
              ("t" "Task" entry (file org-inbox-file) (function as/quick-capture))
              ("e" "Event" entry (file org-inbox-file ) "* %^{Event} %^g \n%^{When?}t\n")
		          ("n" "Note" entry (file org-inbox-file ) "* %^{Note} :NOTE: \n %T \n %?")

		          ("j" "Journal")

              ("j" "Journal")
		          ("jj" "Journal" entry (file+olp+datetree org-journal-file) "* Journal - %^{Title} %^g \n %T \n\n  %?")
		          ("jp" "Problem" entry (file+olp+datetree org-journal-file) "* Problem - %^{Domain} %^g \n %T \n\n *Problem:* %?\n\n *Insight:*\n\n *Tomorrow:*\n\n")
		          ("jc" "Code" entry    (file+olp+datetree org-journal-file) "* Code - %^{Title} %^g \n %T \n\n#+BEGIN_SRC\n%i\n#+END_SRC\n\n%?")
		          ("jf" "Focus Block" entry (file+olp+datetree org-journal-file) "* Focus - %^{Focus:} %^g \n %T \n\n %?")
		          ("jr" "Recovery Block" entry (file+olp+datetree org-journal-file) "* Recovery - %^{Recovery:} %^g \n %T \n\n %?")
		          ("ja" "Admin Block" entry (file+olp+datetree org-journal-file) "* Admin - %^{Admin:} %^g \n %T \n\n %?")
              ("ji" "Interrupt" entry (file+olp+datetree org-journal-file) "* Interrupt - %? \n %T :interrupt: \n\n")


              ("s" "Selection")
              ("sn" "Selection --> Note" entry (file org-inbox-file ) "* %^{Title} :NOTE: %^g \nSource: %u, [[%F][%f]]\n\n #+BEGIN_QUOTE\n%i\n#+END_QUOTE\n\n%?")
              ("st" "Selection --> Todo" entry (file org-inbox-file ) "* TODO %^{Title} %^g \nSource: %u, [[%F][%f]]\n #+BEGIN_QUOTE\n%i\n#+END_QUOTE\n\n%?")



              )))

(defun as/quick-capture-status ()
  "Create and return a TODO heading template"
  (interactive)
  (let* ((title (read-from-minibuffer "Capture: ")))
    (with-temp-buffer
      (org-mode)
      (org-insert-heading)
      (insert (concat title " "))
      (org-todo)
      (buffer-string))))

(defun as/quick-capture ()
  "Returns `org-capture' template string for new Hugo post.
See `org-capture-templates' for more information."
  (mapconcat #'identity
             `(
               ,(as/quick-capture-status)
               "%?")
             "\n"))


;;;; org-agenda
(require 'ox-org)

(setq org-time-stamp-rounding-minutes (quote (1 1)))

(setq org-default-priority ?D)
(setq org-agenda-follow-indirect t)

(setq org-agenda-window-setup 'current-window)
(setq org-agenda-dim-blocked-tasks t)
(setq org-agenda-compact-blocks t)

(setq org-agenda-restriction-lock-highlight-subtree nil)

(add-hook 'org-agenda-mode-hook
	        '(lambda () (hl-line-mode 1))
	        'append)

(setq org-agenda-tags-todo-honor-ignore-options t)
(setq org-agenda-inhibit-startup t)

(setq org-agenda-log-mode-items (quote (closed state clock)))
(setq org-agenda-text-search-extra-files (quote ("~/Sync/org/notes/")))
(setq org-agenda-show-all-dates t)
(setq org-agenda-start-on-weekday nil)

(setq org-agenda-tags-column -100
	    org-agenda-start-with-log-mode nil)

(setq org-agenda-entry-text-maxlines 20)
(setq org-agenda-entry-text-leaders " ")
(setq org-agenda-timegrid-use-ampm t)
(setq org-agenda-include-diary nil)
(setq org-agenda-insert-diary-extract-time t)
(setq org-log-done t)
(setq org-agenda-span 'day)
(setq org-agenda-start-day nil)
