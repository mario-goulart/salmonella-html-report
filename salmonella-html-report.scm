(use sxml-transforms regex posix salmonella salmonella-log-parser)

(define egg-doc-uri "http://wiki.call-cc.org/egg")
(define page-css "http://wiki.call-cc.org/chicken.css")


;;; Misc
(define (cmd-line-arg option args)
  ;; Returns the argument associated to the command line option OPTION
  ;; in ARGS or #f if OPTION is not found in ARGS or doesn't have any
  ;; argument.
  (let ((val (any (cut string-match (conc option "=(.*)") <>) args)))
    (and val (cadr val))))

(define (die . msg)
  (with-output-to-port (current-error-port)
    (lambda ()
      (print (apply conc msg))))
  (exit 1))

(define *verbose* #f)

(define (info msg)
  (when *verbose*
    (print "=== " msg)))

;;; SXML utils
(define (page-template content #!key title)
  `((literal
     "<!DOCTYPE html PUBLIC \"-//W3C//DTD XHTML 1.0 Strict//EN\""
     "\"http://www.w3.org/TR/xhtml1/DTD/xhtml1-strict.dtd\"><html xmlns=\"http://www.w3.org/1999/xhtml\">")
    (html (@ (xmlns "http://www.w3.org/1999/xhtml"))
          (head
           (meta (@ (charset "utf-8")))
           (title ,title)
           (link (@ (rel "stylesheet")
                    (href ,page-css)
                    (type "text/css"))))
          (body
           (div (@ (id "content"))
                ,content)))))


(define (zebra-table header rows)
  `(table
    (tr ,@(map (lambda (h) `(th ,h)) header))
    ,(let ((odd-row #f))
       (map (lambda (row)
              (set! odd-row (not odd-row))
              `(tr (@ (class ,(if odd-row "odd" "even")))
                   ,@(map (lambda (cell) `(td ,cell)) row)))
            rows))))


(define (link-egg-doc egg #!optional text)
  (let ((egg (symbol->string egg)))
    `(a (@ (href ,(make-pathname egg-doc-uri egg)))
        ,(or text egg))))

(define (link-egg-test egg log #!optional text)
  (let ((status (test-status egg log)))
    (if (and status (not (= status -1)))
        (let ((egg (symbol->string egg)))
          `(a (@ (href ,(make-pathname "test" egg "html")))
              ,(case status
                 ((0) "ok")
                 ((1) "fail")
                 (else (error 'link-egg-test
                              (conc "Unexpected test status: " status))))))
        "no test")))

(define (link-egg-install egg #!optional text)
  (let ((egg (symbol->string egg)))
    `(a (@ (href ,(make-pathname "install" egg "html")))
        ,(or text egg))))

(define (egg-summary-line egg log)
  (let ((str-egg (symbol->string egg))
        (broken-deps (broken-dependencies egg log)))
    `((a (@ (href ,(make-pathname "install" str-egg "html"))) ,egg)
      ,(egg-version egg log)
      ,(link-egg-doc egg "doc")
      (a (@ (href ,(make-pathname "dep-graphs" str-egg "png"))) "dependencies")
      (a (@ (href ,(make-pathname "rev-dep-graphs" str-egg "png")))
         "reverse dependencies")
      ,(if (null? broken-deps)
           ""
           (intersperse (map link-egg-install broken-deps) ", "))
      ,(link-egg-test egg log)
      )))


(define (sxml-log->html sxml output-file)
  (with-output-to-file output-file
    (lambda ()
      (let* ((rules `((literal *preorder* . ,(lambda (t b) b))
                      . ,universal-conversion-rules*)))
      (SRV:send-reply (pre-post-order* sxml rules))))))


;;; Index page
(define (make-index log eggs)
  (let* ((date (seconds->string (start-time log)))
         (title (string-append "Salmonella report")))
    (page-template
     `((h1 ,title)
       (p ,date)
       ,(zebra-table
         '("Egg" "Version" "Doc" "Dependencies" "Reverse dependencies" "Broken dependencies" "Test")
         (map (lambda (egg)
                (egg-summary-line egg log))
              eggs)))
     title: (string-append title " - " date))))


;;; Egg installation report page
(define (egg-installation-report egg log)
  (page-template
   `((h1 "Installation output for " ,egg)
     (p "Installation time: " ,(install-duration egg log) "s")
     (pre ,(install-message egg log)))))


;;; Egg test report page
(define (egg-test-report egg log)
  (page-template
   `((h1 "Test output for " ,egg)
     (p "Testing time: " ,(test-duration egg log) "s")
     (pre ,(test-message egg log)))))

;;; Dependencies
(define dot-installed?
  (handle-exceptions exn #f (system* "dot -V > /dev/null 2>&1")))

(unless dot-installed?
  (print "Warning: the external program `dot' has not been found. "
         "[Reverse] dependencies graphs are not going to be generated."))

(define (dot->png dot-file)
  (when dot-installed?
    (system (sprintf "dot -Tpng -o ~A ~A  2>&1"
                     (pathname-replace-extension dot-file "png")
                     dot-file))))

(define (link->dot link)
  (conc (egg->dot (car link)) " -> " (egg->dot (cdr link)) ";"))

(define (egg->dot name)
  (let ((name (->string name)))
    (string-append
     "_" ;; for eggnames starting with numbers (e.g., 9p) and the
         ;; digraph egg
     (irregex-replace/all "[-+]" name "_"))))

(define (label->dot label)
  (let ((egg (car label))
        (version (cadr label))
        (license (caddr label)))
    (conc (egg->dot egg)
          " [label=\"" egg
          (if version
              (conc " (" version ")")
              "")
          "\\n"
          (if license
              (conc " (" license ")")
              "")
          "\"]")))

(define (dot-graph labels links)
  (string-append
   "digraph eggs {\n"
   (string-intersperse (map label->dot labels) "\n")
   "\n"
   (string-intersperse (map link->dot links) "\n")
   "\n}"))

(define reverse-dependencies
  (let ((rev-deps '()))
    (lambda (egg log)
      (or (alist-ref egg rev-deps)
          (let ((deps
                 (filter
                  (lambda (e)
                    (memq egg (egg-dependencies e
                                                log)))
                  (log-eggs log))))
            (set! rev-deps (cons (cons egg deps) rev-deps))
            deps)))))

(define (egg-dependencies->dot egg log dep-graphs-dir #!key reverse?)
  (let ((links '())
        (labels '()))

    (define (add-link! from to)
      (unless (member (cons from to) links)
        (set! links (cons (cons from to) links))))

    (define (add-label! egg version license)
      (let ((egg/version/license (list egg version license)))
        (unless (member egg/version/license labels)
          (set! labels (cons egg/version/license labels)))))

    (define (egg-deps->dot! egg)
      (let ((deps (if reverse?
                      (reverse-dependencies egg log)
                      (egg-dependencies egg log))))
        (add-label! egg (egg-version egg log) (egg-license egg log))
        (for-each (lambda (dep)
                    (add-label! dep (egg-version dep log) (egg-license dep log))
                    (egg-deps->dot! dep)
                    (if reverse?
                        (add-link! dep egg)
                        (add-link! egg dep)))
                  deps)))

    (egg-deps->dot! egg)
    (let ((dot-file (make-pathname dep-graphs-dir (symbol->string egg) "dot")))
      (with-output-to-file dot-file
        (lambda ()
          (print (dot-graph labels links))))
      (dot->png dot-file))))


;;; Broken dependencies
(define (broken-dependencies egg log)
  (let ((deps (egg-dependencies egg log)))
    (filter (lambda (dep)
              (let ((status (install-status dep log)))
                (and status (not (zero? status)))))
            deps)))


;;; Usage
(define (usage #!optional exit-code)
  (let* ((this-program (pathname-strip-directory (program-name)))
         (msg #<#EOF
Usage: #this-program --log-file=<salmonella log file> --out-dir=<out dir>
EOF
))
    (with-output-to-port
        (if exit-code
            (current-error-port)
            (current-output-port))
      (cut print msg))
    (when exit-code (exit exit-code))))




(let* ((args (command-line-arguments))
       (log-file (or (cmd-line-arg '--log-file args) "salmonella.log"))
       (out-dir (or (cmd-line-arg '--out-dir args) "salmonella-html")))

  (when (member "--verbose" args)
    (set! *verbose* #t))

  (when (or (member "--help" args)
            (member "-h" args)
            (member "-help" args))
    (usage 0))

  (unless (file-exists? log-file)
    (die "Could not find " log-file ". Aborting."))

  (when (file-exists? out-dir)
    (die out-dir " already exists. Aborting."))

  ;; Create directories
  (let ((installation-report-dir (make-pathname out-dir "install"))
        (test-report-dir (make-pathname out-dir "test"))
        (dep-graphs-dir (make-pathname out-dir "dep-graphs"))
        (rev-dep-graphs-dir (make-pathname out-dir "rev-dep-graphs")))
    (create-directory out-dir 'with-parents)
    (create-directory installation-report-dir)
    (create-directory test-report-dir)
    (create-directory dep-graphs-dir)
    (create-directory rev-dep-graphs-dir)

    (let* ((log (read-log-file log-file))
           (eggs (sort (log-eggs log)
                       (lambda (e1 e2)
                         (string<? (symbol->string e1)
                                   (symbol->string e2))))))
      ;; Generate the index page
      (info "Generating the index page")
      (sxml-log->html (make-index log eggs) (make-pathname out-dir "index.html"))

      ;; Generate the installation report for each egg
      (for-each (lambda (egg)
                  (info (conc "Generating installation report for " egg))
                  (sxml-log->html
                   (egg-installation-report egg log)
                   (make-pathname installation-report-dir
                                  (symbol->string egg)
                                  "html")))
                eggs)

      ;; Generate the test report for each egg
      (for-each (lambda (egg)
                  (info (conc "Generating test report for " egg))
                  (sxml-log->html
                   (egg-test-report egg log)
                   (make-pathname test-report-dir
                                  (symbol->string egg)
                                  "html")))
                eggs)

      ;; Generate the dependencies graphs page for each egg
      (for-each (lambda (egg)
                  (info (conc "Generating reverse dependencies graph for " egg))
                  (egg-dependencies->dot egg log rev-dep-graphs-dir reverse?: #t)
                  (info (conc "Generating dependencies graph for " egg))
                  (egg-dependencies->dot egg log dep-graphs-dir))
                eggs)
      )))
