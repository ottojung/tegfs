
%run guile

%use (with-cli define-cli:show-help) "./euphrates/define-cli.scm"
%use (append-posix-path) "./euphrates/append-posix-path.scm"
%use (system-environment-get) "./euphrates/system-environment.scm"
%use (random-choice) "./euphrates/random-choice.scm"
%use (alphanum/alphabet) "./euphrates/alphanum-alphabet.scm"
%use (make-directories) "./euphrates/make-directories.scm"
%use (read-all-port) "./euphrates/read-all-port.scm"

%use (debug) "./euphrates/debug.scm"

;; (with-cli
;;  (MAIN
;;   MAIN : OPT* <file...>+
;;   /     --help
;;   OPT : --force
;;   /     --recursive
;;   /     -fr
;;   /     -rf
;;   /     -v
;;   )

(debug "READ: ~s" (read-all-port (current-input-port)))

