
%run guile

%use (with-cli define-cli:show-help) "./euphrates/define-cli.scm"
%use (append-posix-path) "./euphrates/append-posix-path.scm"
%use (system-environment-get) "./euphrates/system-environment.scm"
%use (random-choice) "./euphrates/random-choice.scm"
%use (alphanum/alphabet) "./euphrates/alphanum-alphabet.scm"
%use (make-directories) "./euphrates/make-directories.scm"
%use (read-all-port) "./euphrates/read-all-port.scm"
%use (dprintln) "./euphrates/dprintln.scm"
%use (current-program-path/p) "./euphrates/current-program-path-p.scm"
%use (file-or-directory-exists?) "./euphrates/file-or-directory-exists-q.scm"
%use (write-string-file) "./euphrates/write-string-file.scm"
%use (append-string-file) "./euphrates/append-string-file.scm"

%use (debug) "./euphrates/debug.scm"

(define (main)

  (define (fatal fmt . args)
    (parameterize ((current-output-port (current-error-port)))
      (apply dprintln (cons fmt args)))
    (exit 1))

  (parameterize ((current-program-path/p "tegfs-add"))
    (with-cli
     (MAIN
      MAIN : OPT*
      OPT : KEY
      /     TITLE
      /     TAG
      /     REGISTRY
      /     ROOT
      KEY : --key <key...> <value...>
      TITLE : --title <title>
      TAG : --tag <tag...>
      REGISTRY : --registry-file <registry-file>
      ROOT : --root <root>
      )

     (define ROOT_VAR_NAME "TEGFS_ROOT")

     (define root
       (or <root>
           (system-environment-get ROOT_VAR_NAME)))

     (unless root
       (fatal "Cannot cd into root because ~s env variable is not defined" ROOT_VAR_NAME))

     (unless <registry-file>
       (fatal "Parameter <registry-file> is required, but it is not set"))

     (unless (file-or-directory-exists? root)
       (make-directories root))

     (chdir root)

     (unless (file-or-directory-exists? <registry-file>)
       (write-string-file
        <registry-file>
        "# This file was automatically created by tegfs-add\n\n\n"))

     (debug "HI")

     (define input
       (read-all-port (current-input-port)))

     (debug "input: ~s" input)))

  (dprintln "All done"))

(main)
