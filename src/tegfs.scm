
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

%use (tegfs-add) "./add.scm"
%use (fatal) "./fatal.scm"

(define (main)
  (define ROOT_VAR_NAME "TEGFS_ROOT")

  (define (cd-to-root root)
    (unless root
      (fatal "Cannot cd into root because $~a env variable is not defined" ROOT_VAR_NAME))

    (unless (file-or-directory-exists? root)
      (make-directories root))

    (chdir root))

  (parameterize ((current-program-path/p "tegfs"))
    (with-cli
     (MAIN
      MAIN : ROOT* FUNC
      /      --help
      FUNC : add ADDOPT+
      /      status
      ADDOPT : --title <title>
      /        --tag <tag...>
      /        --key <key...> <value...>
      /        --registry-file <registry-file>
      /        --date <date>
      ROOT : --root <root>
      )

     :default (<root> (system-environment-get ROOT_VAR_NAME))

     (when --help
       (define-cli:show-help))

     (cd-to-root <root>)

     (cond
      (add (tegfs-add
            <title> <tag...> <key...> <value...>
            <registry-file> <date>))
      (status
       (display "NOT IMPLEMENTED YET") (newline)
       (exit 1))
      (else
       (display "Impossible") (newline)
       (exit 1))))))

(main)
