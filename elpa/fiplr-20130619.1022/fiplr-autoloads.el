;;; fiplr-autoloads.el --- automatically extracted autoloads
;;
;;; Code:


;;;### (autoloads (fiplr-reload-list fiplr-clear-cache fiplr-find-file)
;;;;;;  "fiplr" "fiplr.el" (20934 39338 0 0))
;;; Generated autoloads from fiplr.el

(autoload 'fiplr-find-file "fiplr" "\
Runs a completing prompt to find a file from the project.
The root of the project is the return value of `fiplr-root'.

\(fn)" t nil)

(autoload 'fiplr-clear-cache "fiplr" "\
Clears the internal caches used by fiplr so the project is searched again.

\(fn)" t nil)

(autoload 'fiplr-reload-list "fiplr" "\
Clear caches and reload the file listing.

\(fn)" t nil)

;;;***

;;;### (autoloads nil nil ("fiplr-pkg.el") (20934 39338 473962 0))

;;;***

(provide 'fiplr-autoloads)
;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; fiplr-autoloads.el ends here
