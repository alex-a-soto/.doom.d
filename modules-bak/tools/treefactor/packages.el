;; -*- no-byte-compile: t; -*-
;;; tools/neuron/packages.el

(package! treefactor
  :recipe (:type git :host github :repo "cyberthal/treefactor"))

;; For more reproducibility, pin the repo:
;; (package! neuron-mode :pin e1b3e12c71)
;; or run M-x doom/update-pinned-package-form to have the latest commit
