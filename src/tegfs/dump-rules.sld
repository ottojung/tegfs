
(define-library
  (tegfs dump-rules)
  (export dump-rules)
  (import
    (only (euphrates append-posix-path)
          append-posix-path))
  (import
    (only (euphrates file-or-directory-exists-q)
          file-or-directory-exists?))
  (import
    (only (euphrates read-string-file)
          read-string-file))
  (import
    (only (euphrates write-string-file)
          write-string-file))
  (import
    (only (tegfs dump-rules-from-text)
          dump-rules-from-text))
  (import (only (tegfs get-root) get-root))
  (import
    (only (tegfs rules-filename) rules-filename))
  (import (only (scheme base) begin define if let))
  (cond-expand
    (guile (import (only (guile) include-from-path))
           (begin
             (include-from-path "tegfs/dump-rules.scm")))
    (else (include "dump-rules.scm"))))
