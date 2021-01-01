(module salmonella-html-report-cmd ()

(import scheme)

(cond-expand
  (chicken-4
   (import chicken)
   (use data-structures extras files ports posix srfi-1)
   (use salmonella-html-report salmonella-log-parser))
  (chicken-5
   (import (chicken base)
           (chicken file)
           (chicken format)
           (chicken pathname)
           (chicken port)
           (chicken process-context)
           (chicken string))
   (import salmonella-html-report salmonella-log-parser srfi-1)))

;; TODO
;; - maybe optimize `all-dependencies' (memoization?)

(include "version.scm")

;;; Misc
(define *verbose* #f)

(define (info msg)
  (when *verbose*
    (print "=== " msg)))


;;; Usage
(define (usage #!optional exit-code)
  (let* ((this-program (pathname-strip-directory (program-name)))
         (msg #<#EOF
Usage: #this-program [ <options> ] <salmonella log file> <out dir>

--verbose
  Verbose output.

--version
  Show version and exit.

--disable-graphs
  Disable generation of dependency graphs.

--css-uri=<uri>
  URI of the CSS file to be used in the generatated pages.

--graphics-format=<type>
  Format of the [reverse] dependency graph images.  The supported ones
  are those supported by dot (GraphViz).  The default format is SVG.

--compress-html
  Compress HTML files using gzip.

--html-compressor
  External program to use to compress HTML files.

--html-compressor-args
  Arguments to be passed to the external program to compress HTML files.

--compress-graphics
  Compress graphics files using gzip.

--graphics-compressor
  External program to use to compress graphics files.

--graphics-compressor-args
  Arguments to be passed to the external program to compress graphics files.

--keep-dot-files
  By default, #this-program will remove dot files (GraphViz) after converting
  them to graphics files.  This command line can be used to avoid removing
  them.
EOF
))
    (with-output-to-port
        (if (and exit-code (not (zero? exit-code)))
            (current-error-port)
            (current-output-port))
      (cut print msg))
    (when exit-code (exit exit-code))))


(let* ((args (command-line-arguments))
       (disable-graphs? (and (member "--disable-graphs" args) #t))
       (graphics-format (or (cmd-line-arg '--graphics-format args) "svg"))
       (css (cmd-line-arg '--css-uri args))
       (keep-dot-files? (and (member "--keep-dot-files" args) #t)))

  (when (or (member "--help" args)
            (member "-help" args)
            (member "-h" args))
    (usage 0))

  (when (member "--version" args)
    (print salmonella-html-report-version)
    (exit 0))

  (when (< (length args) 2)
    (usage 1))

  (when (member "--verbose" args)
    (set! *verbose* #t))

  (let ((out-dir (last args))
        (log-file (last (butlast args))))

    (unless (file-exists? log-file)
      (die "Could not find " log-file ". Aborting."))

    (when (file-exists? out-dir)
      (die out-dir " already exists. Aborting."))

    (when css
      (salmonella-page-css css))

    (compress-html?
     (and (member "--compress-html" args) #t))

    (html-compressor
     (or (cmd-line-arg '--html-compressor args)
         (html-compressor)))

    (html-compressor-args
     (or (cmd-line-arg '--html-compressor-args args)
         (html-compressor-args)))

    (compressed-html-extension
     (or (cmd-line-arg '--compressed-html-extension args)
         (compressed-html-extension)))

    (compress-graphics?
     (and (member "--compress-graphics" args) #t))

    (graphics-compressor
     (or (cmd-line-arg '--graphics-compressor args)
         (graphics-compressor)))

    (compressed-graphics-extension
     (or (cmd-line-arg '--compressed-graphics-extension args)
         (compressed-graphics-extension)))

    (graphics-compressor-args
     (or (cmd-line-arg '--graphics-compressor-args args)
         (graphics-compressor-args)))

    ;; Create directories
    (let ((installation-report-dir (make-pathname out-dir "install"))
          (test-report-dir (make-pathname out-dir "test"))
          (dep-graphs-dir (make-pathname out-dir "dep-graphs"))
          (rev-dep-graphs-dir (make-pathname out-dir "rev-dep-graphs"))
          (ranks-dir (make-pathname out-dir "ranks")))
      (create-directory out-dir 'with-parents)
      (create-directory installation-report-dir)
      (create-directory test-report-dir)
      (create-directory dep-graphs-dir)
      (create-directory rev-dep-graphs-dir)
      (create-directory ranks-dir)

      (let* ((log (read-log-file log-file))
             (eggs (sort-eggs (log-eggs log)))
             (circular-deps '()) ;; alist mapping eggs to circular-dependency objects
             (circular-rev-deps '()) ;; alist mapping eggs to circular-dependecy objects
             (eggs/deps '()) ;; alist mapping dependencies '((egg1 . egg2) ...)
             (eggs/rev-deps '()) ;; alist mapping reverse dependencies '((egg1 . egg2) ...)
             )

        ;; Generate dependencies data
        (info "Generating dependencies data")
        (for-each (lambda (egg)
                    (let ((deps (all-dependencies egg eggs log)))
                      (if (circular-dependency? deps)
                          (set! circular-deps
                                (cons (cons egg deps) circular-deps))
                          (set! eggs/deps
                                (cons (cons egg deps) eggs/deps)))))
                  eggs)
        (for-each (lambda (egg)
                    (let ((deps (all-dependencies egg eggs log 'reverse)))
                      (if (circular-dependency? deps)
                          (set! circular-rev-deps
                                (cons (cons egg deps) circular-rev-deps))
                          (set! eggs/rev-deps
                                (cons (cons egg deps) eggs/rev-deps)))))
                  eggs)

        ;; Generate the index page
        (info "Generating the index page")
        (sxml-log->html (make-index log eggs circular-deps)
                        (make-pathname out-dir "index.html"))

        ;; Generate the installation report for each egg
        (for-each (lambda (egg)
                    (info (conc "Generating installation report for " egg))
                    (sxml-log->html
                     (egg-installation-report-page egg log)
                     (make-pathname installation-report-dir
                                    (symbol->string egg)
                                    "html")))
                  eggs)

        ;; Generate the test report for each egg that has test and whose
        ;; installation is successful
        (for-each (lambda (egg)
                    (when (and (has-test? egg log)
                               (zero? (install-status egg log)))
                      (info (conc "Generating test report for " egg))
                      (sxml-log->html
                       (egg-test-report-page egg log)
                       (make-pathname test-report-dir
                                      (symbol->string egg)
                                      "html"))))
                  eggs)

        ;; Generate the dependencies graphs page for each egg
        (if (and (dot-installed?) (not disable-graphs?))
            (for-each (lambda (egg)
                        (info (conc "Generating reverse dependencies graph for " egg))
                        (unless (egg-has-circular-dependencies? egg circular-deps)
                          (egg-dependencies->dot egg log rev-dep-graphs-dir graphics-format keep-dot-files? reverse?: #t))
                        (sxml-log->html
                         (egg-reverse-dependencies-report egg
                                                          eggs/rev-deps
                                                          circular-rev-deps
                                                          graphics-format
                                                          log)
                         (make-pathname rev-dep-graphs-dir
                                        (symbol->string egg)
                                        "html"))

                        (info (conc "Generating dependencies graph for " egg))
                        (unless (egg-has-circular-dependencies? egg circular-deps)
                          (egg-dependencies->dot egg log dep-graphs-dir graphics-format keep-dot-files?))
                        (sxml-log->html
                         (egg-dependencies-report egg eggs/deps circular-deps graphics-format log)
                         (make-pathname dep-graphs-dir
                                        (symbol->string egg)
                                        "html")))
                      eggs)
            (fprintf (current-error-port)
                     "~a ~a\n"
                     "Warning: the external program `dot' has not been found."
                     "[Reverse] dependencies graphs are not going to be generated."))

        ;; Generate the ranks page
        (sxml-log->html (rank-installation-time log)
                        (make-pathname ranks-dir "installation-time" "html"))
        (sxml-log->html (rank-test-time log)
                        (make-pathname ranks-dir "test-time" "html"))
        (sxml-log->html (rank-dependencies log eggs/deps circular-deps disable-graphs?)
                        (make-pathname ranks-dir "deps" "html"))
        (sxml-log->html (rank-dependencies log eggs/rev-deps circular-rev-deps disable-graphs? 'reverse)
                        (make-pathname ranks-dir "rev-deps" "html"))
        ))))

) ;; end module
