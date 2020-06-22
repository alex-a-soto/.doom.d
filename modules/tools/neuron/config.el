;;; tools/neuron/config.el -*- lexical-binding: t; -*-

(setq neuron-default-zettelkasten-directory (expand-file-name "~/2-Linked/7-Names/2-Flat/zettelkasten/"))

(defun search-zettelkasten ()
  "Search zettels by content."
  (interactive)
  (progn
    (+ivy-file-search :in neuron-zettelkasten :recursive nil :prompt "Search Zettelkasten: ")
    (neuron-mode)))

  (defun neuron-blog-post ()
  "Create or open today's daily notes."
  (interactive)
  (neuron-check-if-zettelkasten-exists)
  (let* ((today (current-time))
         (zid (format-time-string neuron-daily-note-id-format today))
         (title   (read-string "Title: "))
         (exists (alist-get 'path (neuron--query-zettel-from-id zid)))
         (path (or exists (neuron--run-command
                           (neuron--make-command "new" "--id-hash" title))))
         (buffer (and path (find-file-noselect path))))
    (when buffer
      (neuron--rebuild-cache)
      (pop-to-buffer-same-window buffer)
      (neuron-mode)
      (unless exists
        (dolist (tag neuron-blog-tag)
          (neuron-add-tag tag))))))

(use-package! neuron-mode
  :config
  (map! :leader
        (:prefix ("z" . "zettel")
          "z" #'neuron-new-zettel
          "e" #'neuron-edit-zettel
          "s" #'neuron-select-zettelkasten
          "w" #'neuron-rib-watch
          "g" #'neuron-rib-generate
          "o" #'neuron-open-zettel
          "O" #'neuron-open-index
          "r" #'neuron-refresh
          "l" #'neuron-insert-new-zettel
          "a" #'neuron-add-tag
          "i" #'neuron-insert-tag
          "q" #'neuron-query-tags
          "b" #'neuron-blog-post
          )
                 
          ;Alternatively, bind all rib commands in a separate prefix
          (:prefix ("r" . "rib")
            "w" #'neuron-rib-watch
            "g" #'neuron-rib-generate
            "s" #'neuron-rib-serve
            "o" #'neuron-rib-open-zettel
            "z" #'neuron-rib-open-z-index
            "k" #'neuron-rib-kill
            )
          )


  (map! :map neuron-mode-map
        :localleader
        ;; Override markdown-mode's default behavior
        "o" #'neuron-follow-thing-at-point

        ;; You can also remove the "z" prefix but
        ;; be careful not to override default
        ;; markdown-mode bindings.
        (:prefix ("z" . "zettel")
          "z" #'neuron-new-zettel
          "e" #'neuron-edit-zettel
          "t" #'neuron-insert-tag
          "T" #'neuron-query-tags
          "o" #'neuron-open-current-zettel
          "l" #'neuron-insert-zettel-link
          "L" #'neuron-insert-new-zettel
          "s" #'neuron-insert-static-link
          )
        )
              
  (map! :leader "sz" #'search-zettelkasten)

  (setq neuron-blog-tag '("timeline"))


  )
