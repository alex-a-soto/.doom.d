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
;;
(setq user-home-directory
  (expand-file-name "~/"))


(setq org-gtd-file "~/Projects/gtd.org")
(setq org-inbox-file "~/Inbox/inbox.org")


;; This determines the style of line numbers in effect. If set to `nil', line
;; numbers are disabled. For relative line numbers, set this to `relative'.

;;;; Display Line Numbers
(setq display-line-numbers-type t)
(setq global-display-line-numbers-mode nil)


;;;; Display time and battery
(display-time-mode 1)
(display-battery-mode 1)

;;;; Delete by moving to trash
(setq delete-by-moving-to-trash  t)

;;;; eww
;(setq browse-url-browser-function 'eww) ; emacs browser
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

(defun as/create-workspace ()
  (interactive)
  (let* ((name (read-from-minibuffer "Name:")))
    (+workspace-new name)
    (+workspace-switch name))
  (+workspace/display))


(map! :leader
      :prefix "w"
      :map winner-mode-map
      :desc "Toggle Window" "TAB" #'as/toggle-one-window)

(map! :leader
      :prefix "w"
      :map winner-mode-map
      :desc "Create Workspace" "c" #'as/create-workspace)

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
;;;; dired
(add-hook 'dired-mode-hook
          (lambda ()
            (define-key dired-mode-map
              (kbd "C-c C-x a")
              #'org-attach-dired-to-subtree)))

(add-hook 'dired-mode-hook
          (lambda ()
            (define-key dired-mode-map (kbd "C-c C-x c")
              (lambda ()
                (interactive)
                (let ((org-attach-method 'cp))
                  (call-interactively #'org-attach-dired-to-subtree))))))

;;;; Workspaces
(after! persp-mode
  (setq persp-emacsclient-init-frame-behaviour-override "scratch"))

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
  (setq company-idle-delay 0.2)
  (setq company-minimum-prefix-length 2)
  (setq company-tooltip-limit 20))

(custom-set-faces
 '(aw-leading-char-face
   ((t (:inherit ace-jump-face-foreground :height 3.0)))))

;;;; org-roam
(use-package! org-roam
  ;; :hook
  ;; (after-init . org-roam-mode)
  ;; :custom-face
  ;; (org-roam-link ((t (:inherit org-link :foreground "#005200"))))
  :init
  ;; (map! :leader
  ;;       :prefix "n"
  ;;       :desc "org-roam" "l" #'org-roam
  ;;       :desc "org-roam-insert" "i" #'org-roam-insert
  ;;       :desc "org-roam-switch-to-buffer" "b" #'org-roam-switch-to-buffer
  ;;       :desc "org-roam-find-file" "f" #'org-roam-find-file
  ;;       :desc "org-roam-show-graph" "g" #'org-roam-show-graph
  ;;       :desc "org-roam-insert" "i" #'org-roam-insert)

  (setq org-roam-directory "/home/alexander/Projects/LearnInPublic/"
        org-roam-db-location "/home/alexander/Projects/LearnInPublic/org-roam.db"
        org-roam-graph-exclude-matcher "private"))

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
;;; def
(setq deft-directory "/home/alexander/Inbox")

;;;; Notdeft
(map! :leader
      :prefix "n"
      :desc "Open NotDeft" "d" #'notdeft
      :desc "Create Note" "n" #'as/notdeft-new-file-named
      :desc "#LearningInPublic" "p" #'as/LearnInPublic?)


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
       (concat (format-time-string "%Y-%m-%d-%H%M") "-" str)))

(setq notdeft-notename-function 'notdeft-file-format)

(defun as/notdeft-new-file-named ()
  (interactive)
  (setq notdeft-directory "/home/alexander/Projects/LearnInPublic")
  (let ((title (read-string "New note: ")))
    (notdeft-new-file-named nil title notdeft-template)
    (goto-char (point-min))
    (re-search-forward "^#\\+TITLE:.*$" nil t)
    (insert " " title)
    (re-search-forward "^#\\+DATE:.*$" nil t)
    (insert (format-time-string " [%Y-%m-%d-%H%M]"))
    (goto-char (point-min))
    (goto-line 5)
    ))


(setq org-roam-capture-templates '(("d" "default" plain #'org-roam-capture--get-point "%?" :file-name "%<%Y%m%d%H%M%S>-${slug}" :head "#+TITLE: ${title}\n#+KEYWORDS:\n\n\n\n\n* Related\n* External Links" :unnarrowed t)))

(setq notdeft-template
      "#+TITLE:
#+DATE:
#+KEYWORDS:



* Related
* External Links
")

(after! notdeft
  (load "notdeft-example")
  (setq notdeft-xapian-program "/home/alexander/.bin/Notdeft/notdeft-xapian")
  (setq notdeft-directories '("/home/alexander/Inbox" "/home/alexander/Projects/LearnInPublic"))



  (setq notdeft-time-format " %Y-%m-%d-%H%M")

  (defun notdeft-new-file (pfx)
    "Create a new file quickly.
Create it with an automatically generated name, one based
on the `notdeft-filter-string' filter string if it is non-nil.
With a prefix argument PFX, offer a choice of NotDeft
directories, when there is more than one of them.
With two prefix arguments, also offer a choice of filename
extensions when `notdeft-secondary-extensions' is non-empty.
Return the filename of the created file."
    (interactive "P")
    (let ((data (and notdeft-filter-string))
          (notename
           (and notdeft-filter-string
                (notdeft-title-to-notename notdeft-filter-string))))
      (setq notdeft-template (concat
                              "#+TITLE:" data
                              "\n#+DATE:"
                              "\n#+KEYWORDS:\n\n\n\n"


                              "* Related\n"
                              "* External Links"
                              ))
      (notdeft-sub-new-file notdeft-template notename pfx)
      (goto-char (point-min))
      (re-search-forward "^#\\+TITLE:.*$" nil t)
      (re-search-forward "^#\\+DATE:.*$" nil t)
      (insert (format-time-string " [%Y-%m-%d-%H%M]"))
      (goto-char (point-min))
      (goto-line 5)

      )))


;;;; pdf-tools
;;;; org
(use-package! org
  :config
  (setq org-replace-disputed-keys t)
  (setq org-return-follows-link t)
  (setq org-catch-invisible-edits 'show)


  (add-hook
   'org-mode-hook
   (lambda ()
     (visual-line-mode 1)))

  (setf org-blank-before-new-entry '((heading . nil) (plain-list-item . nil)))
  (setq org-ellipsis "⤵")
  (setq org-use-speed-commands t))

;;;;; org-todo-keywords
(setq org-todo-keywords
	    (quote ((sequence "TODO(t)" "NEXT(n)" "IN-PROGRESS(i)" "|" "DONE(d)")
		          (sequence "WAITING(w)" "HOLD(h)" "DELEGATE(g)" "|" "CANCELLED(c)" "SCHEDULED(s)"))))

(setq org-treat-S-cursor-todo-selection-as-state-change nil)

(custom-declare-face '+org-todo-active '((t (:inherit (bold font-lock-constant-face org-todo)))) "")
(custom-declare-face '+org-todo-project '((t (:inherit (bold font-lock-doc-face org-todo)))) "")
(custom-declare-face '+org-todo-onhold '((t (:inherit (bold warning org-todo)))) "")

;;;;; org-todo-keyword-faces
(setq org-todo-keyword-faces
      (quote (("TODO" :foreground "red" :weight bold)
	            ("NEXT" :foreground "#007cee" :weight bold)
              ("IN-PROGRESS" :foreground "yellow" :weight bold)
	            ("DONE" :foreground "forest green" :weight bold)
	            ("WAITING" :foreground "orange" :weight bold)
              ("HOLD" :foreground "magenta" :weight bold)
	            ("DELEGATE" :foreground "purple" :weight bold)
	            ("CANCELLED" :foreground "red" :weight bold)
	            ("SCHEDULED" :foreground "forest green" :weight bold)

              (quote (("[-]"  . +org-todo-active)
                      ("STRT" . +org-todo-active)
                      ("[?]"  . +org-todo-onhold)
                      ("WAIT" . +org-todo-onhold))))))
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
(setq org-id-locations-file "/home/alexander/.doom.d/.orgids")

(setq org-refile-targets '((nil :maxlevel . 9)
				                   (org-agenda-files :maxlevel . 6)
                           ("~/Inbox/inbox.org" :maxlevel . 6)
                           ("~/Inbox/LGV20/mobile.org" :maxlevel . 6)
                           ))

(setq org-refile-use-outline-path 'file)
(setq org-outline-path-complete-in-steps nil)

(setq org-refile-allow-creating-parent-nodes (quote confirm))

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
                      ("sancocho" . ?s)
                      ("personal" . ?p)
                      )))

(setq org-fast-tag-selection-include-todo t)
(setq org-fast-tag-selection-single-key (quote expert))
;;;;; org-column
(setq org-columns-default-format
      "%1PRIORITY(P) %50ITEM(Task) %12ALLTAGS(Area) %50OUTCOME(Outcome) %6Effort(Effort){:} %6ENERGY(Energy) %5CLOCKSUM(Clock) %10DEADLINE(Deadline)")

;;;;; global-properties
(setq org-global-properties (quote (("Effort_ALL" . "0:05 0:10 0:15 0:25 0:45 1:00 1:30 2:00 3:00 4:00 5:00 6:00 0:00")
				                            ("STYLE_ALL" . "habit"))))
;;;;; org-capture templates
(require 'org-protocol)
(require 'org-expiry)

(defun org-journal-find-location ()
  (org-journal-new-entry t)
  (goto-char (point-min)))


(add-hook 'org-capture-mode-hook (lambda () (call-interactively 'org-store-link)))

(setq org-capture-templates
	    (quote
       (("t" "Task" entry (file org-inbox-file) (function as/quick-capture))
        ("p" "Project" entry (file org-inbox-file) (file "~/.doom.d/templates/new-project.org"))
        ("e" "Event" entry (file org-inbox-file) "* %^{Event} %^g \n%^{When?}t\n")
		    ("n" "Note" entry (file org-inbox-file ) "* %^{Note} :NOTE: \n\n %?")
        ("l" "Link" entry (file org-inbox-file ) "* %(org-cliplink-capture) \n %?" :immediate-finish t)
        ("s" "Selection --> Note" entry (file org-inbox-file ) "* %^{Title}  \nSource: %u, [[%F][%f]]\n\n #+BEGIN_QUOTE\n%i\n#+END_QUOTE\n\n %?")


        ("o" "Org-web tools")
		    ("ow" "Capture Website" entry (file+olp "~/Sync/org/resources/library.org" ) (function as/capture-website) :immediate-finish t)
        ("ox" "Capture local" entry (file+olp org-inbox-file ) (function org-web-tools-archive-attach) :immediate-finish t)
		    ("ot" "Capture text" entry (file org-inbox-file) "* %^{Title} %:description \n:PROPERTIES:\n:DATE_ADDED: %u\n:SOURCE_URL: [[%:link][%:description]]\n:END:\n\n %i%?" :immediate-finish t)


        ("j" "Journal")
		    ("jj" "Journal" entry (function org-journal-find-location) "** %(format-time-string org-journal-time-format) Journal - %^{Title} %^g \n %T \n\n %?")
		    ("jp" "Problem" entry (function org-journal-find-location) "** %(format-time-string org-journal-time-format) Problem - %^{Domain} %^g \n %T \n\n *Problem:* %?\n\n *Insight:*\n\n *Tomorrow:*\n\n")
		    ("jc" "Code" entry    (function org-journal-find-location) "** %(format-time-string org-journal-time-format) Code - %^{Title} %^g \n %T \n\n#+BEGIN_SRC\n%i\n#+END_SRC\n\n%?")

		    ("jf" "Focus Block" entry (function org-journal-find-location) "** %(format-time-string org-journal-time-format) Focus - %^{Focus:} %^g \n %T \n\n %?")
		    ("jr" "Recovery Block" entry (function org-journal-find-location) "** %(format-time-string org-journal-time-format) Recovery - %^{Recovery:} %^g \n %T \n\n %?")
		    ("ja" "Admin Block" entry (function org-journal-find-location) "** %(format-time-string org-journal-time-format) Admin - %^{Admin:} %^g \n %T \n\n %?")
        ("ji" "Interruption" entry (function org-journal-find-location) "** %(format-time-string org-journal-time-format) Interrupt - %? \n %T :interrupt: \n\n")

        ("m" "My Routines")
        ("mm" "Morning" entry (file+olp+datetree "/tmp/routines.org") (file "~/.doom.d/templates/morning-routine.org"))
        ("me" "Evening" entry (file+olp+datetree "/tmp/routines.org") (file "~/.doom.d/templates/evening-routine.org"))
        ("mn" "Night" entry (file+olp+datetree "/tmp/routines.org") (file "~/.doom.d/templates/night-routine.org"))

        ("r" "Review")
        ("rd" "Daily" entry (file+olp+datetree "/tmp/reviews.org") (file "~/.doom.d/templates/daily-review.org"))
        ("rw" "Weekly" entry (file+olp+datetree "/tmp/reviews.org") (file "~/.doom.d/templates/weekly-review.org"))
        ("rm" "Monthly" entry (file+olp+datetree "/tmp/reviews.org") (file "~/.doom.d/templates/monthly-review.org"))
        ("ra" "Annual" entry (file+olp+datetree "/tmp/reviews.org") (file "~/.doom.d/templates/annual-review.org")))))

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
  (mapconcat #'identity
             `(
               ,(as/quick-capture-status)
               "%?")
             "\n"))


;;;; org-agenda
(after! org-agenda
  (require 'ox-org)

  (setq org-agenda-hide-tags-regexp "noexport\\|ATTACH")

  (setq org-agenda-files (quote (
                                 "~/Projects/gtd.org"
                                 "~/Inbox/inbox.org"
                                 "~/Projects/rc-gcal.org"
                                 )))

  (setq org-agenda-text-search-extra-files (quote (
                                                   "~/Inbox/inbox.org"
                                                   "~/Inbox/LGV20/mobile.org"
                                                   )))


  (setq org-archive-location "/home/alexander/Archive/e5/1b5de9-8d7d-40e8-929c-a8a7f628db78/gtd_archive.org::datetree/")

  (setq org-time-stamp-rounding-minutes (quote (1 1)))

  (setq org-default-priority ?D)
  (setq org-agenda-follow-indirect t)

  (setq org-agenda-window-setup 'current-window)
  (setq org-agenda-dim-blocked-tasks nil)
  (setq org-agenda-compact-blocks t)

  (setq org-agenda-restriction-lock-highlight-subtree nil)

  (add-hook 'org-agenda-mode-hook
	          '(lambda () (hl-line-mode 1))
	          'append)

  (setq org-agenda-tags-todo-honor-ignore-options t)
  (setq org-agenda-inhibit-startup t)

  (setq org-agenda-log-mode-items (quote (closed state clock)))
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

  (defun my-org-agenda-recent-open-loops ()
    (interactive)
    (let ((org-agenda-start-with-log-mode t)
          (org-agenda-use-time-grid nil))
      (org-agenda-list nil (org-read-date nil nil "-2d") 4)))

  (defun my-org-agenda-longer-open-loops ()
    (interactive)
    (let ((org-agenda-start-with-log-mode t)
          (org-agenda-use-time-grid nil))
      (org-agenda-list 'file (org-read-date nil nil "-14d") 28)))


  (setq organization-task-id "0f1cf581-cd20-4acd-bf70-e2174635579c")


  (defun as/punch-in (arg)
    (interactive "p")
    (setq as/keep-clock-running t)
    (if (equal major-mode 'org-agenda-mode)
        (let* ((marker (org-get-at-bol 'org-hd-marker))
               (tags (org-with-point-at marker (org-get-tags-at))))
          (if (and (eq arg 4) tags)
              (org-agenda-clock-in '(16))
            (as/clock-in-organization-task-as-default)))
      (save-restriction
        (widen)
        (if (and (equal major-mode 'org-mode) (not (org-before-first-heading-p)) (eq arg 4))
            (org-clock-in '(16))
          (as/clock-in-organization-task-as-default)))))

  (defun as/clock-in-organization-task-as-default ()
    (interactive)
    (org-with-point-at (org-id-find organization-task-id 'marker)
      (org-clock-in '(16))))

  (defun as/punch-out ()
    (interactive)
    (setq as/keep-clock-running nil)
    (when (org-clock-is-active)
      (org-clock-out))
    (org-agenda-remove-restriction-lock))


;;;; org-super-agenda
  (org-super-agenda-mode t)
  (setq org-agenda-time-grid '((daily today require-timed) nil)
        org-agenda-skip-scheduled-if-done t
        org-agenda-skip-deadline-if-done t
        org-agenda-include-deadlines t
        org-agenda-include-diary t
        org-agenda-block-separator nil
        org-agenda-compact-blocks t
        org-agenda-start-with-log-mode t)


  (setq org-agenda-custom-commands
	      '(
          ("i" "Inbox" tags "REFILE"
           ((org-agenda-overriding-header "Inbox")
            (org-tags-match-list-sublevels t)))

          ("o" "Overview"
	         ((agenda "" ((org-agenda-span 1)
			                  (org-super-agenda-groups
			                   '((:name "Habits"
				                    :habit t
                            :order 2)

			                     (:name "Schedule"
				                    :time-grid t
				                    :scheduled t
				                    :order 1)
			                     (:discard (:anything t))))))

            (alltodo "" ((org-agenda-overriding-header "Stuck Project")
			                   (org-super-agenda-groups
			                    '((:name none
				                     :discard (:children "NEXT")
                             :discard (:children "IN-PROGRESS")
				                     :order 4)
			                      (:name none
				                     :discard (:children nil)
				                     :order 4)
			                      (:name none
				                     :children todo)))))


            (alltodo "" ((org-agenda-overriding-header "Active Project")
			                   (org-super-agenda-groups
			                    '((:name none
				                     :children "NEXT"
                             :children "IN-PROGRESS"
				                     :order 1)
			                      (:discard (:anything t))))))

	          (alltodo "" ((org-agenda-overriding-header "In-Progress")
			                   (org-super-agenda-groups
			                    '((:name none
				                     :discard (:not (:todo ("IN-PROGRESS")))
				                     :discard (:habit)
				                     :order 6)
			                      (:name none
				                     :todo t
				                     :face (:underline t))
			                      ))))

            (alltodo "" ((org-agenda-overriding-header "Next Task")
			                   (org-super-agenda-groups
			                    '((:name none
				                     :discard (:not (:todo "NEXT"))
				                     :discard (:habit)
				                     :order 5)
			                      (:name none
				                     :todo "NEXT"
				                     :face (:background "" :underline t))))))

	          (alltodo "" ((org-agenda-overriding-header "Project Task")
			                   (org-agenda-skip-function 'bh/skip-non-project-tasks)
			                   (org-super-agenda-groups
			                    '((:name none
                             :discard (:tag "HOLD")
				                     :todo t
				                     :order 5)))))

	          (alltodo "" ((org-agenda-overriding-header "Standalone Task")
			                   (org-agenda-skip-function 'bh/skip-project-tasks)
			                   (org-super-agenda-groups
			                    '((:name none
                             :discard (:tag "REFILE")
				                     :todo ("TODO" "WAITING" "HOLD")
				                     :order 7)
			                      (:discard (:anything t))))))
            ))
          ))

  (defun bh/is-project-p ()
    "Any task with a todo keyword subtask"
    (save-restriction
      (widen)
      (let ((has-subtask)
            (subtree-end (save-excursion (org-end-of-subtree t)))
            (is-a-task (member (nth 2 (org-heading-components)) org-todo-keywords-1)))
        (save-excursion
          (forward-line 1)
          (while (and (not has-subtask)
                      (< (point) subtree-end)
                      (re-search-forward "^\*+ " subtree-end t))
            (when (member (org-get-todo-state) org-todo-keywords-1)
              (setq has-subtask t))))
        (and is-a-task has-subtask))))

  (defun bh/find-project-task ()
    "Move point to the parent (project) task if any"
    (save-restriction
      (widen)
      (let ((parent-task (save-excursion (org-back-to-heading 'invisible-ok) (point))))
        (while (org-up-heading-safe)
          (when (member (nth 2 (org-heading-components)) org-todo-keywords-1)
            (setq parent-task (point))))
        (goto-char parent-task)
        parent-task)))

  (defun bh/skip-non-tasks ()
    "Show non-project tasks.
Skip project and sub-project tasks, habits, and project related tasks."
    (save-restriction
      (widen)
      (let ((next-headline (save-excursion (or (outline-next-heading) (point-max)))))
        (cond
         ((bh/is-task-p)
          nil)
         (t
          next-headline)))))

  (defun bh/skip-project-tasks ()
    "Show non-project tasks.
Skip project and sub-project tasks, habits, and project related tasks."
    (save-restriction
      (widen)
      (let* ((subtree-end (save-excursion (org-end-of-subtree t))))
        (cond
         ((bh/is-project-p)
          subtree-end)
         ((org-is-habit-p)
          subtree-end)
         ((bh/is-project-subtree-p)
          subtree-end)
         (t
          nil)))))

  (defun bh/is-task-p ()
    "Any task with a todo keyword and no subtask"
    (save-restriction
      (widen)
      (let ((has-subtask)
            (subtree-end (save-excursion (org-end-of-subtree t)))
            (is-a-task (member (nth 2 (org-heading-components)) org-todo-keywords-1)))
        (save-excursion
          (forward-line 1)
          (while (and (not has-subtask)
                      (< (point) subtree-end)
                      (re-search-forward "^\*+ " subtree-end t))
            (when (member (org-get-todo-state) org-todo-keywords-1)
              (setq has-subtask t))))
        (and is-a-task (not has-subtask)))))

  (defun bh/is-project-subtree-p ()
    "Any task with a todo keyword that is in a project subtree.
Callers of this function already widen the buffer view."
    (let ((task (save-excursion (org-back-to-heading 'invisible-ok)
                                (point))))
      (save-excursion
        (bh/find-project-task)
        (if (equal (point) task)
            nil
          t))))


  (defun bh/skip-non-project-tasks ()
    "Show project tasks.
Skip project and sub-project tasks, habits, and loose non-project tasks."
    (save-restriction
      (widen)
      (let* ((subtree-end (save-excursion (org-end-of-subtree t)))
             (next-headline (save-excursion (or (outline-next-heading) (point-max)))))
        (cond
         ((bh/is-project-p)
          next-headline)
         ((org-is-habit-p)
          subtree-end)
         ((and (bh/is-project-subtree-p)
               (member (org-get-todo-state) (list "NEXT" "IN-PROGRESS")))
          subtree-end)
         ((not (bh/is-project-subtree-p))
          subtree-end)
         (t
          nil)))))
  )

;;;; org-attach
(setq org-attach-id-dir  "~/Archive")
(setq org-attach-method 'mv)

(defun as/goto-inbox ()
  (interactive)
  (find-file "~/Inbox/inbox.org")
  (dired-other-window "~/Inbox")
  (dired-hide-details-mode)
  )

(defun as/add-to-inbox ()
  (interactive)
  (dired-mark nil)
  (dired-copy-filename-as-kill '(4))
  (other-window 1 nil)
  (goto-char (point-max))
  (newline-and-indent)
  (end-of-line)
  (call-interactively 'org-insert-heading-respect-content nil)
  (yank)
  (org-id-get-create)
  (other-window 1 nil)
  (call-interactively 'org-attach-dired-to-subtree)
  )

;;;; Mapping avy with which-key

(map! :leader
      :prefix "j"
      :desc "Jump to word" "w" #'avy-goto-word-0)

(map! :leader
      :prefix "j"
      :desc "Jump to char" "j" #'avy-goto-char)

(map! :leader
      :prefix "j"
      :desc "Goto-line" "l" #'avy-goto-line)

(setq ivy-sort-matches-functions-alist
      '((t)
        (ivy-switch-buffer . ivy-sort-function-buffer)
        (counsel-find-file . ivy--)))


(use-package! counsel
  :config
  (setq grep-command "rg -M 120 --with-filename --no-heading --line-number --color never %s")
  :bind
  ("C-s" . 'counsel-grep-or-swiper))

;;; dired-hide-dotfiles
(use-package! dired-hide-dotfiles
  :config
  (define-key dired-mode-map "." #'dired-hide-dotfiles-mode)
  (add-hook! 'dired-mode-hook 'as/dired-mode-hook)
  (defun as/dired-mode-hook ()
    (dired-hide-dotfiles-mode +1)))

;;; flycheck-ledger
(use-package! flycheck-ledger
  :demand t
  :after (flycheck ledger-mode)
  :init (progn
          (add-hook 'ledger-mode-hook #'flycheck-mode)
          (setq flycheck-ledger-zero-accounts '("Assets:Budget:Available"
                                                "Assets:Budget:Unbudgeted"
                                                "Assets:Expenses:InternalTransfer"))))

;;; elfeed
(after! elfeed

  (setf url-queue-timeout 30)

  (elfeed-org)

  (setq rmh-elfeed-org-files (list "~/Archive/08/6e648e-2baf-4775-a0bc-8153ae99e505/rss.org"))

  (setq-default elfeed-search-filter "@12-hours-ago +unread")




  (defun as/elfeed-load-db-and-open ()
    "Wrapper to load the elfeed db from disk before opening"
    (interactive)
    (elfeed-db-load)
    (elfeed)
    (elfeed-update)
    (setq elfeed-search-filter "@1-hours-ago +unread")
    (elfeed-search-update--force))

  ;;write to disk when quiting
  (defun as/elfeed-save-db-and-bury ()
    "Wrapper to save the elfeed db to disk before burying buffer"
    (interactive)
    (elfeed-db-save)
    (quit-window))

  (define-key elfeed-search-mode-map "q" 'as/elfeed-save-db-and-bury)
  (define-key elfeed-search-mode-map "h"
    (lambda ()
      (interactive)
      (elfeed-search-set-filter (default-value 'elfeed-search-filter))))

  (define-key elfeed-search-mode-map (kbd "j") #'next-line)
  (define-key elfeed-search-mode-map (kbd "k") #'previous-line)

  (defun as/elfeed-yank-capture ()
    (interactive)
    (elfeed-search-yank)
    (org-capture nil "l")
    (message "Entry saved to Inbox"))

  (define-key elfeed-search-mode-map "y" #'as/elfeed-yank-capture)
  )

;;; frame-font
(setq my-font-name "DejaVu Sans Mono")
(defcustom my-font-size 12 "My font size")

(defun set-frame-font-size (&optional font-size)
  "Change frame font size to FONT-SIZE.
If no FONT-SIZE provided, reset the size to its default variable."
  (let ((font-size
         (or font-size
             (car (get 'my-font-size 'standard-value)))))
    (customize-set-variable 'my-font-size font-size)
    (set-frame-font
     (format "%s %d" my-font-name font-size) nil t)))

(defun increase-frame-font ()
  "Increase frame font by one."
  (interactive)
  (set-frame-font-size (+ my-font-size 1)))

(defun decrease-frame-font ()
  "Decrease frame font by one."
  (interactive)
  (set-frame-font-size (- my-font-size 1)))

(defun reset-frame-font ()
  "Reset frame font to its default value."
  (interactive)
  (set-frame-font-size))

;;; modalka
(use-package modalka
  :config
  (modalka-global-mode 1)

  (defun normal-mode-modalka ()
	  (interactive)
	  (if (modalka-mode nil)
		    (modalka-mode 1)
	    (nil)))

  (defun insert-mode-modalka ()
	  (interactive)
	  (modalka-mode 0))

  (setq-default cursor-type '(bar . 2))
  (setq modalka-cursor-type 'box)

  (setq org-capture-mode-hook 'insert-mode-modalka)

  (defun modalka-select-major-mode (modalka-mode-map)
	  (let ((modalka-mode-command (cdr (assoc major-mode modalka-mode-map))))
	    (if modalka-mode-command (apply modalka-mode-command))))

  (defun modalka-mode-hydra ()
	  (interactive)
	  (modalka-select-major-mode modalka-major-mode-hydra-list))


  (custom-set-variables
   '(modalka-excluded-modes
	   (quote
	    (
	     ediff-mode
	     helpful-mode
	     dired-mode
	     magit-mode
	     magit-popup-mode
	     debugger-mode
	     ediff-mode
	     help-mode
	     git-rebase-mode
	     help-mode
	     org-agenda-mode
	     org-capture-mode
	     emms-playlist-mode
	     pdf-tools-modes
	     undo-tree-visualizer-mode
	     ))))

  (define-key modalka-mode-map [remap self-insert-command] 'ignore)
  (define-key global-map [escape] #'normal-mode-modalka)

  (general-def modalka-mode-map

    "<tab>" '+workspace/other
    "<tab>" '+workspace/other
    "<backtab>" 'crux-switch-to-previous-buffer
    "<return>" 'action-key
    "SPC" 'ignore
    "<backspace>" 'ignore

    "a"  'ignore
    "b"  'ivy-switch-buffer
    "c"  'org-capture
    "d"  'dired-jump
    "e"  'ignore
    "f"  'counsel-find-file
    "g"  'ignore
    "h"  'bury-buffer
    "i"  #'insert-mode-modalka
    "j"  'avy-goto-char
    "k"  'kill-buffer
    "l"  'ignore
    "m"  'ignore
    "n"  'org-tree-to-indirect-buffer
    "o"  'ignore
    "p"  'treefactor-throw
    "q"  'ignore
    "r"  'counsel-recentf
    "s"  'ignore
    "t"  '+treemacs/toggle
    "u"  'ignore
    "v"  'point-to-register
    "w"  '+workspace/switch-to
    "x"  'ignore
    "y"  'ignore
    "z"  'ignore


    "A"  'ignore
    "B"  'ignore
    "C"  'ignore
    "D"  'ignore
    "E"  'ignore
    "F"  'ignore
    "G"  'ignore
    "H"  'unbury-buffer
    "I"  'ignore
    "J"  'ignore
    "K"  'kill-this-buffer
    "L"  'ignore
    "M"  'ignore
    "N"  'ignore
    "O"  'ignore
    "P"  'ignore
    "Q"  'ignore
    "R"  'ignore
    "S"  'ignore
    "T"  'ignore
    "U"  'ignore
    "V"  'jump-to-register
    "W"  'ignore
    "X"  'ignore
    "Y"  'ignore
    "Z"  'ignore


    "0"  'delete-window
    "1"  'delete-other-windows
    "2"  'split-window-below
    "3"  'split-window-right
    "4"  'ignore
    "5"  'ignore
    "6"  'ignore
    "8"  'ignore
    "9"  'ignore

    "!"  'shell-command
    "@"  'hycontrol-windows-grid
    "#"  'ignore
    "$"  'ignore
    "%"  'ignore
    "^"  'ignore
    "&"  'ignore
    "*"  'ignore
    "("  'ignore
    ")"  'ignore
    "-"  'ignore
    "+"  'ignore
    "<"  'ignore
    ">"  'ignore
    "?"  'ignore
    ";"  'ignore
    "'"  'ignore
    "\\" 'ignore
    "["  'ignore
    "]"  'ignore
    ","  'winner-undo
    "."  'winner-redo
    "/"  'ignore
    "'"  'ignore
    "|"  'ignore
    "="  'ignore
    "+"  'ignore
    "-"  'ignore
    "_"  'ignore)
  )

;Hyperbole
;; (use-package! hyperbole
;;   :config
;;   (define-key org-mode-map (kbd "<M-return>") nil))
;;
;;; org-journal

(use-package! org-journal
  :bind
  ("C-c n j" . org-journal-new-entry)
  ("C-c n t" . org-journal-today)
  :custom
  (org-journal-enable-agenda-integration t)
  :config
  (setq org-journal-file-format "private-%Y-%m-%d.org"
        org-journal-dir "/home/alexander/Projects/LearnInPublic"
        org-journal-carryover-items nil)
  (defun org-journal-today ()
    (interactive)
    (org-journal-new-entry t)))


;;; Twittering-mode

;;; LearnInPublic
(defun as/LearnInPublic? ()
  (interactive)
  (if (yes-or-no-p (message "#LearnInPublic?"))
      (let ((msg (read-from-minibuffer "Message: "))
            (buff (file-name-base buffer-file-name)))
        (if (buffer-modified-p)
            (if (yes-or-no-p (message "Update repository?"))
                (as/commit msg buff))
          (if (yes-or-no-p (message "Post to Twitter?"))
              (as/plain-tweet msg buff)
            nil))
        nil)))

(defun as/commit (msg buff)
(interactive)
(save-buffer)
(magit-call-git "add" (concat buff ".org"))
(magit-call-git "commit" "-m" msg)
(magit-call-git "push" "origin")
(if (yes-or-no-p (message "Post to Twitter?"))
      (as/tweet msg buff)
    nil))

(defun as/tweet (msg url)
  (interactive)
  (let ((tags (read-from-minibuffer "Tags: ")))
    (setq tweet-template (concat
                          msg
                          "\n\n"
                          "https://github.com/alex-a-soto/LearnInPublic/blob/master/" url ".org\n\n"
                          "#LearnInPublic " tags
                          )))
(twittering-update-status tweet-template nil nil 'normal t))

(defun as/plain-tweet (msg url)
  (interactive)
  (let ((tags (read-from-minibuffer "Tags: ")))
    (setq tweet-template (concat
                          msg
                          "\n\n"
                          "#LearnInPublic " tags
                          )))
(twittering-update-status tweet-template nil nil 'normal t))

(setq browse-url-chrome-program "/usr/bin/google-chrome-stable")
(setq browse-url-browser-function 'browse-url-chrome)

;;; xournal
 (openwith-mode t)

(setq openwith-associations '(("\\.xopp\\'" "xournalpp" (file))))




;;; org-gcal
(load-file "~/.doom.d/rc-gcal.el")
