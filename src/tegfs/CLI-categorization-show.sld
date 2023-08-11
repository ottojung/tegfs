
(define-library
  (tegfs CLI-categorization-show)
  (export CLI::categorization-show)
  (import
    (only (tegfs categorization-read)
          categorization-read))
  (import
    (only (scheme base) begin define newline))
  (import (only (scheme write) display))
  (cond-expand
    (guile (import (only (guile) include-from-path))
           (begin
             (include-from-path
               "tegfs/CLI-categorization-show.scm")))
    (else (include "CLI-categorization-show.scm"))))
