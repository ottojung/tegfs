;;;; Copyright (C) 2022  Otto Jung
;;;;
;;;; This program is free software: you can redistribute it and/or modify
;;;; it under the terms of the GNU General Public License as published by
;;;; the Free Software Foundation; version 3 of the License.
;;;;
;;;; This program is distributed in the hope that it will be useful,
;;;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;;; GNU General Public License for more details.
;;;;
;;;; You should have received a copy of the GNU General Public License
;;;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

%run guile

%var tegfs-prolog/parse
%var tegfs-prolog

%use (make-temporary-filename) "./euphrates/make-temporary-filename.scm"
%use (system-fmt) "./euphrates/system-fmt.scm"
%use (append-posix-path) "./euphrates/append-posix-path.scm"
%use (dprintln) "./euphrates/dprintln.scm"
%use (file-or-directory-exists?) "./euphrates/file-or-directory-exists-q.scm"
%use (write-string-file) "./euphrates/write-string-file.scm"
%use (read-string-file) "./euphrates/read-string-file.scm"
%use (comp) "./euphrates/comp.scm"
%use (fn) "./euphrates/fn.scm"
%use (curry-if) "./euphrates/curry-if.scm"
%use (string->lines) "./euphrates/string-to-lines.scm"
%use (string-strip) "./euphrates/string-strip.scm"
%use (string-split/simple) "./euphrates/string-split-simple.scm"
%use (list-split-on) "./euphrates/list-split-on.scm"
%use (define-tuple) "./euphrates/define-tuple.scm"
%use (read-list) "./euphrates/read-list.scm"
%use (lines->string) "./euphrates/lines-to-string.scm"
%use (list-map/flatten) "./euphrates/list-map-flatten.scm"
%use (curry-if) "./euphrates/curry-if.scm"
%use (CFG-CLI->CFG-AST) "./euphrates/parse-cfg-cli.scm"
%use (list-deduplicate) "./euphrates/list-deduplicate.scm"
%use (raisu) "./euphrates/raisu.scm"
%use (file-delete) "./euphrates/file-delete.scm"
%use (list-and-map) "./euphrates/list-and-map.scm"
%use (list-find-first) "./euphrates/list-find-first.scm"
%use (compose-under) "./euphrates/compose-under.scm"
%use (read-string-line) "./euphrates/read-string-line.scm"
%use (open-file-port) "./euphrates/open-file-port.scm"
%use (list-span-while) "./euphrates/list-span-while.scm"

%use (categorization-filename) "./categorization-filename.scm"
%use (root/p) "./root-p.scm"
%use (tegfs-edit-tags) "./edit-tags.scm"

%use (debugv) "./euphrates/debugv.scm"

(define (tegfs-prolog/parse)
  (tegfs-prolog "build/testroot/jaj-wwwad.tegfs.reg.lisp")
  (dprintln "done(prolog)."))

(define (yield-for-prolog thing)
  (define type (car thing))
  (case type
    ((t)
     (display "t(")
     (display (cadr thing))
     (for-each
      (lambda (arg)
        (display ", v")
        (display arg))
      (cddr thing))
     (display ").")
     (newline))
    ((p)
     (display "p(")
     (display (cadr thing))
     (for-each
      (lambda (arg)
        (display ", ")
        (when (symbol? arg)
          (display "v"))
        (write arg))
      (cddr thing))
     (display ").")
     (newline))))

;;
;; Example translation:
;;
;; input tags:
;;    educational video from^4chan
;;      vs=X,Y woman=X man=Y black=Y
;;      with=X,B=Y,C
;;      big=B beautiful=B
;;      big=C concentraition=C
;;
;; output:
;;
;; t(1, educational).
;; t(1, video).
;; t(1, from, 4chan).
;; t(1, vs, vX, vY).
;; t(1, woman, vX).
;; t(1, man, vY).
;; t(1, black, vY).
;; t(1, with, vX, vB).
;; t(1, big, vB).
;; t(1, beautiful, vB).
;; t(1, with, vY, vC).
;; t(1, big, vC).
;; t(1, concentraition, vC).
;;
;; p(1, id, "id").
;; p(1, target, "target").
;;
(define (tegfs-prolog registry-path)
  (define input-port (open-file-port registry-path "r"))
  (define output-path (string-append (make-temporary-filename) ".pl"))
  (define output-port (open-file-port output-path "w"))
  (define counter
    (let ((cnt 0))
      (lambda _ (set! cnt (+ 1 cnt)) cnt)))

  (parameterize ((current-output-port output-port))
    (display ":-style_check(-discontiguous).")
    (newline)
    (for-each (translate-entry yield-for-prolog counter)
              (read-list input-port)))

  (close-port output-port)

  (system-fmt "prolog ~a" output-path))

(define (handle-the-split yield cnt equal-split)
  (define pred-name (list->string (car equal-split)))

  (define variable-lists (cdr equal-split))
  (for-each
   (lambda (var-list)
     (define variables
       (map list->string
            (list-split-on
             (comp (equal? #\,))
             var-list)))

     (yield `(t ,cnt ,pred-name ,@variables)))
   variable-lists))

(define (translate-tag yield cnt)
  (lambda (tag)
    (define str (symbol->string tag))
    (define chars (string->list str))

    (define-values (span-^-pre span-^-post0)
      (list-span-while
       (negate (comp (equal? #\^)))
       chars))
    (define span-^-post
      ((curry-if (negate null?) cdr) span-^-post0))
    (define span-^? (not (null? span-^-post)))

    (define-values (span-_-pre0 span-_-post0)
      (list-span-while
       (negate (comp (equal? #\_)))
       (reverse chars)))

    (define span-_-pre (reverse span-_-pre0))
    (define span-_-post
      (reverse ((curry-if (negate null?) cdr) span-_-post0)))
    (define span-_? (not (null? span-_-post)))

    (define equal-split
      (list-split-on (comp (equal? #\=)) chars))
    (define equal-split?
      (not (null? (cdr equal-split))))

    (define sum
      (+ (if span-^? 1 0)
         (if span-_? 1 0)
         (if equal-split? 1 0)))

    (unless (>= 1 sum)
      (raisu 'used-too-much-properties tag sum))

    (cond
     (span-^?
      (yield
       `(t ,cnt ,(list->string span-^-pre)
           ,(list->string span-^-post))))
     (span-_?
      (yield
       `(t ,cnt ,(list->string span-_-pre)
           ,(list->string span-_-post))))
     (equal-split?
      (handle-the-split yield cnt equal-split))
     (else
      (yield `(t ,cnt ,tag))))))

(define (translate-property yield cnt)
  (lambda (property)
    (define name (car property))
    (define value (cdr property))
    (unless (equal? 'tags name)
      (yield `(p ,cnt ,name ,value)))))

(define (translate-entry yield counter)
  (lambda (entry)
    (define cnt (counter))

    (define id
      (cdr (or (assoc 'id entry)
               (raisu 'could-not-get-an-id entry))))

    (define tags
      (cdr (or (assoc 'tags entry)
               (raisu 'could-not-get-tags entry))))

    (for-each (translate-tag yield cnt) tags)
    (for-each (translate-property yield cnt) entry)

    ))





