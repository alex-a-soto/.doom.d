(require 'deadgrep)

(defun dg-current-directory (search-term directory)
  "Start a ripgrep search for SEARCH-TERM from DIRECTORY.
If called with a prefix argument, create the results buffer but
don't actually start the search."
  (interactive
   (list (deadgrep--read-search-term)
         (read-directory-name "Directory: "
                              default-directory)))
  (let ( (deadgrep-project-root-function (list 'lambda '() directory)) )
    (deadgrep search-term)
    ))

(defun dg (search-term directory)
  "Start a ripgrep search for SEARCH-TERM from DIRECTORY.
If called with a prefix argument, create the results buffer but
don't actually start the search."
  (interactive
   (list (deadgrep--read-search-term)
         (read-directory-name "Directory: "
                              (funcall deadgrep-project-root-function))))
  (let ( (deadgrep-project-root-function (list 'lambda '() directory)) )
    (deadgrep search-term)
    ))
