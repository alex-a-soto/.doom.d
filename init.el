;;; init.el -*- lexical-binding: t; -*-

(cond
 ((string-equal system-name "exobrain")
  (load-file (concat "~/.doom.d/init/" system-name ".el"))))

(cond
 ((string-equal system-name "SurfaceGo")
  (load-file (concat "~/.doom.d/init/" system-name ".el"))))
