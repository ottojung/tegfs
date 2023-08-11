
(define-library
  (tegfs tegfs-query-noopen)
  (export tegfs-query/noopen)
  (import
    (only (tegfs entries-iterate) entries-iterate))
  (import
    (only (tegfs tegfs-query-noopen-notall-profun)
          tegfs-query/noopen/notall/profun))
  (import
    (only (scheme base)
          and
          begin
          define
          if
          not
          null?))
  (cond-expand
    (guile (import (only (guile) include-from-path))
           (begin
             (include-from-path
               "tegfs/tegfs-query-noopen.scm")))
    (else (include "tegfs-query-noopen.scm"))))
