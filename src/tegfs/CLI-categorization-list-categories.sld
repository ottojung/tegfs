
(define-library
  (tegfs CLI-categorization-list-categories)
  (export CLI::categorization-list-categories)
  (import
    (only (euphrates lines-to-string) lines->string))
  (import (only (euphrates tilda-a) ~a))
  (import
    (only (tegfs categorization-get-all-tags)
          categorization-get-all-tags))
  (import
    (only (tegfs categorization-read)
          categorization-read))
  (import
    (only (scheme base) begin define map newline))
  (import (only (scheme write) display))
  (cond-expand
    (guile (import (only (guile) include-from-path))
           (begin
             (include-from-path
               "tegfs/CLI-categorization-list-categories.scm")))
    (else (include
            "CLI-categorization-list-categories.scm"))))
