
%run guile

%use (with-cli define-cli:show-help) "./euphrates/define-cli.scm"
%use (system-environment-get) "./euphrates/system-environment.scm"
%use (make-directories) "./euphrates/make-directories.scm"
%use (read-all-port) "./euphrates/read-all-port.scm"
%use (dprintln) "./euphrates/dprintln.scm"
%use (current-program-path/p) "./euphrates/current-program-path-p.scm"
%use (file-or-directory-exists?) "./euphrates/file-or-directory-exists-q.scm"
%use (write-string-file) "./euphrates/write-string-file.scm"
%use (append-string-file) "./euphrates/append-string-file.scm"
%use (list-zip) "./euphrates/list-zip.scm"
%use (system-re) "./euphrates/system-re.scm"
%use (string-strip) "./euphrates/string-strip.scm"
%use (list-intersperse) "./euphrates/list-intersperse.scm"

(define (main)

  (define (fatal fmt . args)
    (parameterize ((current-output-port (current-error-port)))
      (apply dprintln (cons fmt args)))
    (exit 1))

  (define (get-date)
    (string-strip
     (car
      (system-re "date --utc '+%Y-%m-%d %H:%M:%S+0000'"))))

  (define ROOT_VAR_NAME "TEGFS_ROOT")

  (define (cd-to-root root)
    (unless root
      (fatal "Cannot cd into root because ~s env variable is not defined" ROOT_VAR_NAME))

    (unless (file-or-directory-exists? root)
      (make-directories root))

    (chdir root))

  (define (init-registry-file <registry-file>)
    (unless <registry-file>
      (fatal "Parameter <registry-file> is required, but it is not set"))

    (unless (file-or-directory-exists? <registry-file>)
      (write-string-file
       <registry-file>
       "\n# This file was automatically created by tegfs-add\n\n\n")))

  (parameterize ((current-program-path/p "tegfs-add"))
    (with-cli
     (MAIN
      MAIN : OPT*
      /      --help
      OPT : --title <title>
      /     --tag <tag...>
      /     --key <key...> <value...>
      /     --registry-file <registry-file>
      /     --root <root>
      /     --date <date>
      )

     :default (<root> (system-environment-get ROOT_VAR_NAME))

     (when --help
       (define-cli:show-help))

     (cd-to-root <root>)

     (init-registry-file <registry-file>)

     (define input
       (read-all-port (current-input-port)))

     (define date
       (or <date> (get-date)))

     (define key-value-pairs
       (cons (cons "date" (string-append "[" date "]"))
             (list-zip (or <key...> '()) (or <value...> '()))))

     (define tags (or <tag...> '()))

     (append-string-file
      <registry-file>
      (with-output-to-string
        (lambda ()
          (newline)

          (display "* ")
          (when <title>
            (display <title>))
          (unless (null? tags)
            (display ":")
            (display
             (apply string-append (list-intersperse ":" tags)))
            (display ":"))
          (newline)

          (for-each
           (lambda (pair)
             (display "  ")
             (display (car pair))
             (display ": ")
             (display (cdr pair))
             (newline))
           key-value-pairs)

          (unless (string-null? input)
            (display "  #+BEGIN_SRC text") (newline)
            (display input)
            (unless (string-suffix? "\n" input)
              (newline))
            (display "  #+END_SRC text") (newline))
          (newline))))

     ))

  (display "Added!\n"))

(main)
