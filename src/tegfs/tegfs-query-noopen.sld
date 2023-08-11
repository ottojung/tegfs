
(define-library
  (tegfs tegfs-query-noopen)
  (export tegfs-query/noopen)
  (import
    (only (euphrates profun-database)
          profun-database-extend))
  (import
    (only (euphrates profun-standard-handler)
          profun-standard-handler))
  (import
    (only (euphrates profun)
          profun-create-falsy-database
          profun-eval-query/boolean))
  (import
    (only (tegfs dump-rules-list) dump-rules/list))
  (import
    (only (tegfs entries-iterate) entries-iterate))
  (import
    (only (tegfs generify-dumped-term)
          generify-dumped-term))
  (import
    (only (tegfs inference-to-profun-rule)
          inference->profun-rule))
  (import
    (only (tegfs prolog-query-parse)
          prolog-query-parse))
  (import
    (only (tegfs prolog) translate-entry-tags))
  (import
    (only (scheme base)
          and
          begin
          cons
          define
          define-values
          if
          let
          list
          map
          not
          null?
          quasiquote))
  (cond-expand
    (guile (import (only (guile) include-from-path))
           (begin
             (include-from-path
               "tegfs/tegfs-query-noopen.scm")))
    (else (include "tegfs-query-noopen.scm"))))
