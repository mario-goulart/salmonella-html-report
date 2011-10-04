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
    ,(if header
         `(tr ,@(map (lambda (h) `(th ,h)) header))
         '())
    ,(let ((odd-row #f))
       (map (lambda (row)
              (set! odd-row (not odd-row))
              `(tr (@ (class ,(if odd-row "odd" "even")))
                   ,@(map (lambda (cell) `(td ,cell)) row)))
            rows))))


(define (link-egg-doc egg log #!optional text)
  (if (doc-exists? egg log)
      (let ((egg (symbol->string egg)))
        `(a (@ (href ,(make-pathname egg-doc-uri egg)))
            ,(or text egg)))
      "no doc"))

(define (link-egg-test egg log #!optional text)
  (let ((status (test-status egg log)))
    (if (and status (not (= status -1)))
        (let ((egg (symbol->string egg)))
          `(a (@ (href ,(make-pathname "test" egg "html")))
              ,(if (zero? status)
                   "ok"
                   "fail")))
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
      ,(link-egg-doc egg log "egg page")
      (a (@ (href ,(make-pathname "dep-graphs" str-egg "html"))) "dependencies")
      (a (@ (href ,(make-pathname "rev-dep-graphs" str-egg "html")))
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
(define (render-summary log)
  (let ((blank '(literal "&nbsp;")))
    `((h2 "Summary")
      (table
       (tr (th "Installation")
           (th "Tests")
           (th "Documentation")
           (th "Total"))
       (tr
        ;; Installation
        (td ,(zebra-table
              #f
              `(("Ok" ,(count-install-ok log))
                ("Failed" ,(count-install-fail log))
                (,blank ,blank))))

        ;; Tests
        (td ,(zebra-table
              #f
              `(("Ok" ,(count-test-ok log))
                ("Failed" ,(count-test-fail log))
                ("No test" ,(count-no-test log)))))

        ;; Documentation
        (td ,(zebra-table
              #f
              `(("Documented" ,(count-documented log))
                ("Undocumented" ,(count-undocumented log))
                (,blank ,blank))))

        ;; Total
        (td ,(zebra-table
              #f
              `(("Total number of eggs" ,(count-total-eggs log))
                (,blank ,blank) ;; no info about skipped eggs...
                (,blank ,blank))))
        )))))

(define (list-eggs eggs log #!optional failed?)
  (zebra-table
   '("Egg" "Version" "Doc" "Dependencies" "Reverse dependencies" "Broken dependencies" "Test")
   (map (lambda (egg)
          (egg-summary-line egg log))
        ((if failed? remove filter)
         (lambda (egg)
           (let ((status (install-status egg log)))
             (and status (zero? status))))
         eggs))))

(define (render-warnings log)
  (let ((warnings
         (append (filter-map
                  (lambda (entry)
                    (let ((action (report-action entry)))
                      (and (memq action '(check-dependencies
                                          check-category))
                           (list (report-egg entry)
                                 (report-message entry)))))
                  log)

                 (filter-map
                  (lambda (egg)
                    (let ((gpl-deps (gpl-dependencies egg log)))
                      (and gpl-deps
                           (list egg
                                 (conc egg "'s license is " (egg-license egg log)
                                       " but it has GPL eggs as dependencies: "
                                       (string-intersperse
                                        (map ->string gpl-deps)
                                        ", "))))))
                  (log-eggs log)))))
    (if (null? warnings)
        '()
        `((h2 "Warnings")
          ,(zebra-table '("Egg" "Warning") warnings)))))

(define (make-index log eggs)
  (let* ((date (seconds->string (start-time log)))
         (title (string-append "Salmonella report")))
    (page-template
     `((h1 ,title)
       (p ,date)
       ,(render-summary log)
       ,(render-warnings log)
       (h2 "Installation failed")
       ,(list-eggs eggs log 'failed)

       (h2 "Installation succeeded")
       ,(list-eggs eggs log))
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
     (pre ,(test-message egg log)))
   title: (conc "Test output for " egg)))

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

(define (link->dot egg link log reverse?)
  (let* ((orig (car link))
         (dest (cdr link))
         (direct-dep? (if reverse?
                          (direct-reverse-dependency? egg orig log)
                          (direct-dependency? egg dest log))))
    (conc (egg->dot (car link)) " -> " (egg->dot (cdr link))
          (if direct-dep?
              ""
              "[ color = gray ]")
          ";")))

(define (egg->dot name)
  (let ((name (->string name)))
    (string-append
     "_" ;; for eggnames starting with numbers (e.g., 9p) and the
         ;; digraph egg
     (irregex-replace/all "[-+]" name "_"))))

(define (label->dot label log)
  (let ((egg (car label))
        (version (cadr label))
        (license (caddr label)))
    (conc (egg->dot egg)
          " [label=\"" egg
          (if version
              (conc " (" version ")")
              "")
          (if license
              (conc "\\n(" license ")")
              "")
          "\""
          ;; If egg installation failed, paint the node red
          (let ((status (install-status egg log)))
            (if (and status (zero? status))
                ""
                ",color=red,style=filled"))
          ;; If egg test failed, paint the node border red
          (let ((status (test-status egg log)))
            (if (and status (not (= status -1)) (not (zero? status)))
                ",color=red"
                ""))
          "]")))

(define (dot-graph egg labels links log reverse?)
  (string-append
   "digraph eggs {\n"
   "node [fontsize=8]\n"
   (string-intersperse (map (lambda (label)
                              (label->dot label log))
                            labels)
                       "\n")
   "\n"
   (string-intersperse (map (lambda (link)
                              (link->dot egg link log reverse?))
                            links)
                       "\n")
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

(define (direct-reverse-dependency? egg1 egg2 log)
  (and (memq egg2 (reverse-dependencies egg1 log)) #t))

(define (direct-dependency? egg1 egg2 log)
  (and (memq egg2 (egg-dependencies egg1 log)) #t))

(define (all-dependencies egg log #!optional reverse?)
  (define (get-deps egg)
    (let ((deps (if reverse?
                    (reverse-dependencies egg log)
                    (egg-dependencies egg log))))
      (if (null? deps)
          '()
          (append deps (map get-deps deps)))))
  (delete-duplicates (flatten (get-deps egg))))

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
          (print (dot-graph egg labels links log reverse?))))
      (dot->png dot-file))))

(define (color-legend)
  `((h4 "Legend")
    (table (@ (style "width: 300px;"))
           (tr (td (div (@ (style ,(string-append "width: 15px;"
                                                  "height: 15px;"
                                                  "background-color: red;")))))
               (td "Egg whose installation failed"))
           (tr (td (div (@ (style ,(string-append "width: 12px;"
                                                  "height: 12px;"
                                                  "border-style: solid;"
                                                  "border-width: 2px;"
                                                  "border-color: red;")))))
               (td "Egg whose tests failed"))
           (tr (td (hr (@ (style ,(string-append "width: 15px;"
                                                 "background-color: black;"
                                                 "height: 3px;"
                                                 "border: 0px;"
                                                 )))))
               (td "Direct connection"))
           (tr (td (hr (@ (style ,(string-append "width: 15px;"
                                                 "background-color: gray;"
                                                 "height: 3px;"
                                                 "border: 0px;"
                                                 )))))
               (td "Indirect connection")))))

(define (egg-dependencies-report egg log)
  (page-template
   `((h1 "Dependencies for " ,egg)
     ,(let* ((total-deps (length (all-dependencies egg log)))
             (num-direct-deps (length (egg-dependencies egg log)))
             (num-indirect-deps (- total-deps num-direct-deps)))
        `(p ,(case total-deps
               ((0) (conc egg " has no dependencies."))
               ((1) (conc egg " has 1 dependency."))
               (else (conc egg " has " total-deps " dependencies "
                           "(" num-direct-deps " direct, "
                           num-indirect-deps " indirect)")))))
     (p (img (@ (src ,(make-pathname #f (symbol->string egg) "png"))
                (alt ,(conc "Dependencies graph for " egg)))))
     ,(color-legend))
   title: (conc "Dependencies for " egg)))

(define (egg-reverse-dependencies-report egg log)
  (page-template
   `((h1 "Reverse dependencies for " ,egg)
     ,(let* ((total-deps (length (all-dependencies egg log 'reverse)))
             (num-direct-deps (length (reverse-dependencies egg log)))
             (num-indirect-deps (- total-deps num-direct-deps)))
        `(p ,(case total-deps
               ((0) (conc "No egg depends on " egg "."))
               ((1) (conc "1 egg depends on " egg "."))
               (else (conc total-deps " eggs depend on " egg
                           " (" num-direct-deps " direct, "
                           num-indirect-deps " indirect)")))))
     (p (img (@ (src ,(make-pathname #f (symbol->string egg) "png"))
                (alt ,(conc "Reverse dependencies graph for " egg)))))
     ,(color-legend))
   title: (conc "Reverse dependencies for " egg)))

;;; Broken dependencies
(define (broken-dependencies egg log)
  (let ((deps (egg-dependencies egg log)))
    (filter (lambda (dep)
              (let ((status (install-status dep log)))
                (and status (not (zero? status)))))
            deps)))

;;; GPL infection
(define (gpl? license)
  (and license (or (string-prefix-ci? "GPL" license)
                   (string-prefix-ci? "AGPL" license))))

(define (gpl-dependencies egg log)
  (and (not (gpl? (egg-license egg log)))
       (let* ((deps (egg-dependencies egg log with-test-dependencies?: #t))
              (gpl-deps (filter-map (lambda (egg)
                                      (and (gpl? (egg-license egg log))
                                           egg))
                                    deps)))
         (and (not (null? gpl-deps)) gpl-deps))))


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

  (when (null? args)
    (usage 1))

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
           (eggs (sort-eggs (log-eggs log))))

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
      (when dot-installed?
        (for-each (lambda (egg)
                    (info (conc "Generating reverse dependencies graph for " egg))
                    (egg-dependencies->dot egg log rev-dep-graphs-dir reverse?: #t)
                    (sxml-log->html
                     (egg-reverse-dependencies-report egg log)
                     (make-pathname rev-dep-graphs-dir
                                    (symbol->string egg)
                                    "html"))

                    (info (conc "Generating dependencies graph for " egg))
                    (egg-dependencies->dot egg log dep-graphs-dir)
                    (sxml-log->html
                     (egg-dependencies-report egg log)
                     (make-pathname dep-graphs-dir
                                    (symbol->string egg)
                                    "html")))
                  eggs))
      )))
