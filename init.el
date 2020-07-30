;;; init.el -*- lexical-binding: t; -*-

(cond
 ((string-equal system-name "exomind")
  (load-file (concat "~/.doom.d/init/" system-name ".el"))))

(cond
 ((string-equal system-name "SurfaceGo")
  (load-file (concat "~/.doom.d/init/" system-name ".el"))))

(cond
 ((string-equal system-name "alpha")
  (load-file (concat "~/.doom.d/init/" system-name ".el"))))

