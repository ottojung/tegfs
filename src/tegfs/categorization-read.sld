
(define-library
  (tegfs categorization-read)
  (export categorization-read)
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
    (only (tegfs categorization-filename)
          categorization-filename))
  (import (only (tegfs root-p) root/p))
  (import (only (scheme base) begin define unless))
  (cond-expand
    (guile (import (only (guile) include-from-path))
           (begin
             (include-from-path
               "tegfs/categorization-read.scm")))
    (else (include "categorization-read.scm"))))
