(use sxml-transforms regex posix salmonella salmonella-log-parser)

;; TODO
;; - maybe optimize `all-dependencies' (memoization?)

(define egg-doc-uri "http://wiki.call-cc.org/egg")
(define *page-css* "http://wiki.call-cc.org/chicken.css")


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

(define (menu egg log active)
  (let ((locs `((summary        . "Report summary")
                (install        . "Installation report")
                (test           . "Test report")
                (dep-graphs     . "Dependencies graphs")
                (rev-dep-graphs . "Reverse dependencies graphs"))))
    `(ul (@ (id "salmonella-menu"))
         ,@(filter-map
            (lambda (item)
              (let ((path (car item))
                    (label (cdr item)))
                `(li ,(cond ((eq? active path)
                             label)
                            ((eq? path 'test)
                             (link-egg-test egg log
                                            text: "Test report"
                                            relative-path: ".."
                                            text-no-test: "Test report"))
                            (else
                             `(a (@ (href ,(if (eq? path 'summary)
                                               "../"
                                               (conc "../" path "/" egg ".html"))))
                                 ,label))))))
            locs))))

;;; SXML utils
(define (page-template content #!key title)
  `((literal
     "<!DOCTYPE html PUBLIC \"-//W3C//DTD XHTML 1.0 Strict//EN\""
     "\"http://www.w3.org/TR/xhtml1/DTD/xhtml1-strict.dtd\"><html xmlns=\"http://www.w3.org/1999/xhtml\">")
    (html (@ (xmlns "http://www.w3.org/1999/xhtml"))
          (head
           (meta (@ (charset "utf-8")))
           (meta (@ (name "generator") (content "salmonella-html-report")))
           (title ,title)
           (link (@ (rel "stylesheet")
                    (href ,*page-css*)
                    (type "text/css")))
           (style (@ (type "text/css"))
             "#salmonella-menu li { display: inline; list-style-type: none; padding-right: 20px; }"))
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


(define (link-egg-doc egg log #!optional text omit-text-if-no-doc?)
  (if (doc-exists? egg log)
      (let ((egg (symbol->string egg)))
        `(a (@ (href ,(make-pathname egg-doc-uri egg)))
            ,(or text egg)))
      (if omit-text-if-no-doc?
          ""
          egg)))

(define (link-egg-test egg log #!key text relative-path text-no-test)
  (let ((status (test-status egg log)))
    (if (and status (not (= status -1)))
        (let ((egg (symbol->string egg)))
          `(a (@ (href ,(make-pathname
                         (if relative-path
                             (list relative-path "test")
                             "test")
                         egg
                         "html")))
              ,(if (zero? status)
                   (or text "ok")
                   (or text "fail"))))
        (or text-no-test ""))))

(define (link-egg-install egg #!optional text)
  (let ((egg (symbol->string egg)))
    `(a (@ (href ,(make-pathname "install" egg "html")))
        ,(or text egg))))

(define (egg-summary-line egg log #!optional failed?)
  (let ((str-egg (symbol->string egg))
        (broken-deps (broken-dependencies egg log)))
    (append
     `((a (@ (href ,(make-pathname "install" str-egg "html"))) ,egg)
       ,(egg-version egg log)
       ,(link-egg-doc egg log "egg page" 'omit-text-if-no-doc)
       (a (@ (href ,(make-pathname "dep-graphs" str-egg "html"))) "dependencies")
       (a (@ (href ,(make-pathname "rev-dep-graphs" str-egg "html")))
          "reverse dependencies"))
     (if failed?
         (if (null? broken-deps)
             '()
             (list (intersperse (map link-egg-install broken-deps) ", ")))
         '())
     `(,(link-egg-test egg log))
      )))


(define sxml-log->html
  (let ((rules `((literal *preorder* . ,(lambda (t b) b))
                 . ,universal-conversion-rules*)))
    (lambda (sxml output-file)
      (with-output-to-file output-file
        (lambda ()
          (SRV:send-reply (pre-post-order* sxml rules)))))))


;;; Index page
(define (render-summary log)
  (let ((blank '(literal "&nbsp;")))
    `((h2 (@ (id "summary")) "Summary")
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
              `(("Total number of eggs" ,(count-total-eggs log with-skipped?: #t))
                ("Not skipped" ,(count-total-eggs log with-skipped?: #f))
                ("Skipped" ,(length (log-skipped-eggs log))))))
        )))))


(define (list-eggs/installation-failed eggs log)
  (zebra-table
    '("Egg" "Version" "Doc" "Dependencies" "Reverse dependencies" "Broken dependencies")
    (map (lambda (egg)
           (egg-summary-line egg log #t))
         (remove (lambda (egg)
                   (let ((status (install-status egg log)))
                     (and status (zero? status))))
                 eggs))))


(define (list-eggs/installation-succeeded eggs log)
  (define (list-eggs eggs)
    (zebra-table
     '("Egg" "Version" "Doc" "Dependencies" "Reverse dependencies" "Tests")
     (map (lambda (egg)
            (egg-summary-line egg log #f))
          eggs)))
  `((h3 (@ (id "installation-succeeded-test-failed")) "Tests failed")
    ,(list-eggs (filter (lambda (egg)
                          (let ((status (install-status egg log))
                                (test-status (test-status egg log)))
                            (and status
                                 (zero? status)
                                 (> test-status 0))))
                        eggs))
    (h3 (@ (id "installation-succeeded-test-succeeded")) "Tests succeeded")
    ,(list-eggs (filter (lambda (egg)
                          (let ((status (install-status egg log))
                                (test-status (test-status egg log)))
                            (and status
                                 (zero? status)
                                 (zero? test-status))))
                        eggs))
    (h3 (@ (id "installation-succeeded-no-test")) "No tests")
    ,(list-eggs (filter (lambda (egg)
                          (let ((status (install-status egg log))
                                (test-status (test-status egg log)))
                            (and status
                                 (zero? status)
                                 (eq? test-status -1))))
                        eggs))))


(define (render-warnings log)
  (let ((warnings
         (append (filter-map
                  (lambda (entry)
                    (let ((action (report-action entry)))
                      (and (memq action '(check-dependencies
                                          check-category
                                          check-license
                                          check-author))
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
                  (log-eggs log))

                 (map (lambda (egg/circ)
                        (let ((egg (car egg/circ))
                              (circular-dep (cdr egg/circ)))
                          (list egg
                                (conc egg " (or some egg it depends on) contains circular dependencies"))))
                      *circular-dependencies*))))
    (if (null? warnings)
        '()
        `((h2 (@ (id "warnings")) "Warnings")
          ,(zebra-table '("Egg" "Warning") warnings)))))

(define (make-index log eggs)
  (let* ((date (seconds->string (start-time log)))
         (title (string-append "Salmonella report"))
         (skipped-eggs (log-skipped-eggs log)))
    (page-template
     `((h1 ,title)
       (p ,date)

       ;; TOC
       (h2 "Table of contents")
       (ul ,@(append
              '((li (a (@ (href "#summary")) "Summary"))
                (li (a (@ (href "#warnings")) "Warnings"))
                (li (a (@ (href "#installation-failed")) "Installation failed"))
                (li `((a (@ (href "#installation-succeeded")) "Installation succeeded")
                      (ul (li (a (@ (href "#installation-succeeded-test-failed")) "Test failed"))
                          (li (a (@ (href "#installation-succeeded-test-succeeded")) "Test succeeded"))
                          (li (a (@ (href "#installation-succeeded-no-test")) "No test"))))))
              (if (null? skipped-eggs)
                  '()
                  '((li (a (@ (href "#skipped-eggs")) "Skipped eggs"))))
              '((li (a (@ (href "#ranks")) "Ranks"))
                (li (a (@ (href "#environment-information")) "Environment information"))
                (li (a (@ (href "#total-run-time")) "Total run time")))))

       ,(render-summary log)
       ,(render-warnings log)
       (h2 (@ (id "installation-failed")) "Installation failed")
       ,(list-eggs/installation-failed eggs log)

       (h2 (@ (id "installation-succeeded")) "Installation succeeded")
       ,(list-eggs/installation-succeeded eggs log)

       ,(if (null? skipped-eggs)
            '()
            `((h2 (@ (id "skipped-eggs")) "Skipped eggs")
              ,(zebra-table #f (map list skipped-eggs))))

       ,(ranks-report)

       (h2 (@ (id "environment-information")) "Environment information")
       (pre ,(salmonella-info log))

       (h2 (@ (id "total-run-time")) "Total run time")
       ,(prettify-time (inexact->exact (total-time log)))

       (div (@ (style "text-align: center; font-size: small;"))
            "Generated by "
            (a (@ (href "http://wiki.call-cc.org/egg/salmonella-html-report"))
               "salmonella-html-report")))
     title: (string-append title " - " date))))


;;; Egg installation report page
(define (egg-installation-report egg log)
  (page-template
   `((h1 "Installation output for " ,(link-egg-doc egg log) " "
         ,(let ((status (install-status egg log)))
            (if (and status (zero? status))
                "[ok]"
                "[fail]")))
     ,(menu egg log 'install)
     ,(cond ((not (zero? (fetch-status egg log)))
             `(pre ,(fetch-message egg log)))
            ((not (meta-data egg log))
             '(p "Error reading .meta file"))
            (else
             `((p "Installation time: "
                  ,(prettify-time
                    (inexact->exact (install-duration egg log))))
               (pre ,(install-message egg log))))))
   title: (conc egg ": Installation report")))


;;; Egg test report page
(define (egg-test-report egg log)
  (page-template
   `((h1 "Test output for " ,(link-egg-doc egg log) " ["
         ,(if (zero? (test-status egg log))
              "ok"
              "fail")
         "]")
     ,(menu egg log 'test)
     (p "Testing time: "
        ,(prettify-time (inexact->exact (test-duration egg log))))
     (pre ,(test-message egg log)))
   title: (conc egg ": Test output")))

;;; Dependencies
(define dot-installed?
  (handle-exceptions exn #f (system* "dot -V > /dev/null 2>&1")))

(unless dot-installed?
  (print "Warning: the external program `dot' has not been found. "
         "[Reverse] dependencies graphs are not going to be generated."))

(define (dot->image dot-file graphics-format)
  (when dot-installed?
    (system (sprintf "dot -T~a -o ~A ~A  2>&1"
                     graphics-format
                     (pathname-replace-extension dot-file graphics-format)
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

(define (all-dependencies root-egg log #!optional reverse?)
  ;; Returns a list of [reverse] dependencies for the given egg or a
  ;; circular-dependency object if a circular dependency is detected.
  ;; This procedure is just a helper, not intented to be used by
  ;; regular code.  Use `all-egg-dependencies' instead.
  (let ((visited '()))
    (define (get-deps egg)
      (let ((deps (if reverse?
                      (reverse-dependencies egg log)
                      (egg-dependencies egg log))))
        (cond ((null? deps)
               '())
              ((find (lambda (dep)
                       (member (cons dep egg) visited))
                     deps)
               => (lambda (circular)
                    (list (make-circular-dependency egg circular))))
              (else (for-each (lambda (dep)
                                (set! visited (cons (cons egg dep) visited)))
                              deps)
                    (append deps (map get-deps deps))))))
    (let ((deps (flatten (get-deps root-egg))))
      (cond ((find circular-dependency? deps) => identity)
            (else (delete-duplicates deps))))))

(define-record circular-dependency egg1 egg2)

(define *eggs/dependencies* '()) ;; alist mapping dependencies '((egg1 . egg2) ...)
(define *eggs/reverse-dependencies* '()) ;; alist mapping reverse dependencies '((egg1 . egg2) ...)
(define *circular-dependencies* '()) ;; alist mapping eggs to circular-dependecy objects
(define *circular-reverse-dependencies* '()) ;; alist mapping eggs to circular-dependecy objects

(define (all-egg-dependencies egg reverse?)
  (or (if reverse?
          (alist-ref egg *eggs/reverse-dependencies*)
          (alist-ref egg *eggs/dependencies*))
      (if reverse?
          (alist-ref egg *circular-reverse-dependencies*)
          (alist-ref egg *circular-dependencies*))))

(define (egg-has-circular-dependencies? egg reverse?)
  (and
   (if reverse?
       (alist-ref egg *circular-reverse-dependencies*)
       (alist-ref egg *circular-dependencies*))
   #t))

(define (egg-dependencies->dot egg log dep-graphs-dir graphics-format #!key reverse?)
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
      (dot->image dot-file graphics-format))))

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

(define (deps-report egg graphics-format log reverse?)
  (page-template
   `(,(if reverse?
          `(h1 "Reverse dependencies for " ,(link-egg-doc egg log))
          `(h1 "Dependencies for " ,(link-egg-doc egg log)))
     ,(menu egg log (if reverse? 'rev-dep-graphs 'dep-graphs))
     ,(let ((all-deps (all-egg-dependencies egg reverse?)))
        (if (circular-dependency? all-deps)
            `((p "This egg (or some egg "
                 ,(if reverse?
                      "that depends on it"
                      "it depends on")
                 ") contains circular dependencies.")
              ,(let ((egg1 (circular-dependency-egg1 all-deps))
                     (egg2 (circular-dependency-egg2 all-deps)))
                 `(p (code ,egg1) " depends on " (code ,egg2) " and "
                     (code ,egg2) " depends on " (code ,egg1) ".")))
            (let* ((total-deps (length all-deps))
                   (num-direct-deps
                    (length (if reverse?
                                (reverse-dependencies egg log)
                                (egg-dependencies egg log))))
                   (num-indirect-deps (- total-deps num-direct-deps)))
              `((p ,(case total-deps
                      ((0) (if reverse?
                               (conc "No egg depends on " egg ".")
                               (conc egg " has no dependencies.")))
                      ((1) (if reverse?
                               (conc "1 egg depends on " egg ".")
                               (conc egg " has 1 dependency.")))
                      (else (if reverse?
                                (conc total-deps " eggs depend on " egg
                                      " (" num-direct-deps " direct, "
                                      num-indirect-deps " indirect)")
                                (conc egg " has " total-deps " dependencies "
                                      "(" num-direct-deps " direct, "
                                      num-indirect-deps " indirect)")))))
                (p (img (@ (src ,(make-pathname #f (symbol->string egg) graphics-format))
                           (alt ,(conc (if reverse? "Reverse dependencies" "Dependencies")
                                       " graph for " egg)))))
                ,(color-legend))))))
   title: (conc egg ": "
                (if reverse?
                    "Reverse dependencies"
                    "Dependencies"))))



(define (egg-dependencies-report egg graphics-format log)
  (deps-report egg graphics-format log #f))

(define (egg-reverse-dependencies-report egg graphics-format log)
  (deps-report egg graphics-format log #t))

;;; Broken dependencies
(define (broken-dependencies egg log)
  (let ((deps (egg-dependencies egg log)))
    (filter (lambda (dep)
              (let ((status (install-status dep log)))
                (and status (not (zero? status)))))
            deps)))

;;; GPL infection
(define (gpl? license)
  (let ((license (->string license)))
    (and license (or (string-prefix-ci? "GPL" license)
                     (string-prefix-ci? "AGPL" license)))))

(define (gpl-dependencies egg log)
  (and (not (gpl? (egg-license egg log)))
       (let* ((deps (egg-dependencies egg log with-test-dependencies?: #t))
              (gpl-deps (filter-map (lambda (egg)
                                      (and (gpl? (egg-license egg log))
                                           egg))
                                    deps)))
         (and (not (null? gpl-deps)) gpl-deps))))


;;; Ranks
(define (ranks-report)
  `((h2 (@ (id "ranks")) "Ranks")
    (ul
     (li (a (@ (href "ranks/installation-time.html")) "Installation time"))
     (li (a (@ (href "ranks/test-time.html")) "Test time"))
     (li (a (@ (href "ranks/deps.html")) "Dependencies"))
     (li (a (@ (href "ranks/rev-deps.html")) "Reverse dependencies")))))

(define (rank-duration action log)
  (map (lambda (egg/duration)
         (list (car egg/duration) (prettify-time (cdr egg/duration))))
       (sort (filter-map
              (lambda (entry)
                (let ((entry-action (report-action entry)))
                  (and (eq? action entry-action)
                       (let ((egg (symbol->string (report-egg entry))))
                         (cons `(a (@ (href ,(make-pathname
                                              (list ".."
                                                    (symbol->string action))
                                              egg
                                              "html")))
                                   ,egg)
                               (report-duration entry))))))
              log)
             (lambda (a b)
               (> (cdr a) (cdr b))))))


(define (rank-page-nav)
  '(p (a (@ (href "..")) "Report summary")))


(define (rank-installation-time log)
  (page-template
   `((h1 "Installation time rank")
     ,(rank-page-nav)
     ,(zebra-table
       '("Egg" "Instalation time")
       (rank-duration 'install log)))
   title: "Installation time rank"))


(define (rank-test-time log)
  (page-template
   `((h1 "Test time rank")
     ,(rank-page-nav)
     ,(zebra-table
       `("Egg" "Test time")
       (rank-duration 'test log)))
   title: "Test time rank"))


(define (rank-dependencies log graphs-disabled? #!optional reverse?)
  (define (link-dep egg)
    (if graphs-disabled?
        egg
        `(a (@ (href ,(make-pathname (list ".."
                                           (if reverse?
                                               "rev-dep-graphs"
                                               "dep-graphs"))
                                     (symbol->string egg)
                                     "html")))
            ,egg)))
  (page-template
   `((h1 ,(if reverse?
              "Reverse dependencies rank"
              "Dependencies rank"))
     ,(rank-page-nav)
     ,(zebra-table
       `("Egg" ,(if reverse?
                    "Number of reverse dependencies"
                    "Number of dependencies")
         "Percentage of the total number of eggs")
       (let* ((all-eggs (log-eggs log))
              (num-eggs (length all-eggs)))
         (sort
          (map (lambda (egg)
                 (let* ((all-deps (all-egg-dependencies egg reverse?))
                        (num-deps (length all-deps)))
                   (list (link-dep egg)
                         num-deps
                         (conc (inexact->exact (round (* (/ num-deps num-eggs) 100))) "%"))))
               (remove (lambda (egg)
                         (egg-has-circular-dependencies? egg reverse?))
                       all-eggs))
          (lambda (a b)
            (> (cadr a) (cadr b))))))

     ,(if (null? (if reverse?
                     *circular-reverse-dependencies*
                     *circular-dependencies*))
          '()
          `((h2 "Eggs with circular dependencies"
                ,(if reverse?
                     " in their reverse depedencies"
                     ""))
            (ul ,@(map (lambda (egg)
                         `(li ,(link-dep egg)))
                       (map car (if reverse?
                                    *circular-reverse-dependencies*
                                    *circular-dependencies*)))))))
   title: (if reverse?
              "Reverse dependencies rank"
              "Dependencies rank")))


;;; Usage
(define (usage #!optional exit-code)
  (let* ((this-program (pathname-strip-directory (program-name)))
         (msg #<#EOF
Usage: #this-program [ <options> ] <salmonella log file> <out dir>

--verbose
  Verbose output

--disable-graphs
  Disable generation of dependency graphs

--css-uri=<uri>
  URI of the CSS file to be used in the generatated pages

--graphics-format=<type>
  Format of the [reverse] dependency graph images.  The supported ones
  are those supported by dot (GraphViz).
EOF
))
    (with-output-to-port
        (if exit-code
            (current-error-port)
            (current-output-port))
      (cut print msg))
    (when exit-code (exit exit-code))))




(let* ((args (command-line-arguments))
       (disable-graphs? (and (member "--disable-graphs" args) #t))
       (graphics-format (or (cmd-line-arg '--graphics-format args) "svg"))
       (css (cmd-line-arg '--css-uri args)))

  (when (< (length args) 2)
    (usage 1))

  (when (member "--verbose" args)
    (set! *verbose* #t))

  (when (or (member "--help" args)
            (member "-h" args)
            (member "-help" args))
    (usage 0))

  (let ((out-dir (last args))
        (log-file (last (butlast args))))

    (unless (file-exists? log-file)
      (die "Could not find " log-file ". Aborting."))

    (when (file-exists? out-dir)
      (die out-dir " already exists. Aborting."))

    (when css (set! *page-css* css))

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
             (eggs (sort-eggs (log-eggs log))))


        ;; Generate dependencies data
        (info "Generating dependencies data")
        (for-each (lambda (egg)
                    (let ((deps (all-dependencies egg log)))
                      (if (circular-dependency? deps)
                          (set! *circular-dependencies*
                                (cons (cons egg deps) *circular-dependencies*))
                          (set! *eggs/dependencies*
                                (cons (cons egg deps) *eggs/dependencies*)))))
                  eggs)
        (for-each (lambda (egg)
                    (let ((deps (all-dependencies egg log 'reverse)))
                      (if (circular-dependency? deps)
                          (set! *circular-reverse-dependencies*
                                (cons (cons egg deps) *circular-reverse-dependencies*))
                          (set! *eggs/reverse-dependencies*
                                (cons (cons egg deps) *eggs/reverse-dependencies*)))))
                  eggs)

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

        ;; Generate the test report for each egg that has test and whose
        ;; installation is successful
        (for-each (lambda (egg)
                    (when (and (has-test? egg log)
                               (zero? (install-status egg log)))
                      (info (conc "Generating test report for " egg))
                      (sxml-log->html
                       (egg-test-report egg log)
                       (make-pathname test-report-dir
                                      (symbol->string egg)
                                      "html"))))
                  eggs)

        ;; Generate the dependencies graphs page for each egg
        (when (and dot-installed? (not disable-graphs?))
          (for-each (lambda (egg)
                      (info (conc "Generating reverse dependencies graph for " egg))
                      (unless (egg-has-circular-dependencies? egg 'reverse)
                        (egg-dependencies->dot egg log rev-dep-graphs-dir graphics-format reverse?: #t))
                      (sxml-log->html
                       (egg-reverse-dependencies-report egg graphics-format log)
                       (make-pathname rev-dep-graphs-dir
                                      (symbol->string egg)
                                      "html"))

                      (info (conc "Generating dependencies graph for " egg))
                      (unless (egg-has-circular-dependencies? egg #f)
                        (egg-dependencies->dot egg log dep-graphs-dir graphics-format))
                      (sxml-log->html
                       (egg-dependencies-report egg graphics-format log)
                       (make-pathname dep-graphs-dir
                                      (symbol->string egg)
                                      "html")))
                    eggs))

        ;; Generate the ranks page
        (sxml-log->html (rank-installation-time log)
                        (make-pathname ranks-dir "installation-time" "html"))
        (sxml-log->html (rank-test-time log)
                        (make-pathname ranks-dir "test-time" "html"))
        (sxml-log->html (rank-dependencies log disable-graphs?)
                        (make-pathname ranks-dir "deps" "html"))
        (sxml-log->html (rank-dependencies log disable-graphs? 'reverse)
                        (make-pathname ranks-dir "rev-deps" "html"))
        ))))
