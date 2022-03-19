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
%use (list-ref-or) "./euphrates/list-ref-or.scm"
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
%use (alphanum/alphabet/index) "./euphrates/alphanum-alphabet.scm"
%use (~a) "./euphrates/tilda-a.scm"

%use (categorization-filename) "./categorization-filename.scm"
%use (root/p) "./root-p.scm"
%use (tegfs-edit-tags) "./edit-tags.scm"
%use (get-registry-files) "./get-registry-files.scm"

%use (debugv) "./euphrates/debugv.scm"

(define (tegfs-prolog/parse)
  (tegfs-prolog)
  (dprintln "done(prolog)."))

(define (prolog-var-needs-quoting? var/chars)
  (define first (alphanum/alphabet/index (car var/chars)))
  (not
   (or (equal? '(#\~) var/chars)
       (and first
            (> first 9)
            (< first 36)
            (list-and-map
             (compose-under
              or
              (comp (equal? #\_))
              alphanum/alphabet/index)
             (cdr var/chars))))))

(define (yield-for-prolog thing)
  (define type (car thing))

  (define (print arg)
    (cond
     ((symbol? arg)
      (let* ((str (symbol->string arg))
             (chars (string->list str)))
        (if (prolog-var-needs-quoting? chars)
            (begin
              (display "'")
              (display str)
              (display "'"))
            (display str))))
     ((string? arg) (write arg))
     ((integer? arg) (write arg))
     (else (raisu 'uknown-type arg))))

  (define (comma-print lst)
    (let loop ((lst lst))
      (unless (null? lst)
        (print (car lst))
        (unless (null? (cdr lst))
          (display ", "))
        (loop (cdr lst)))))

  (case type
    ((t)
     (display "t(")
     (print (cadr thing))
     (display ", ")
     (unless (null? (cdr (cddr thing)))
       (display "["))
     (comma-print (cddr thing))
     (unless (null? (cdr (cddr thing)))
       (display "]"))
     (display ").")
     (newline))
    ((p)
     (display "p(")
     (comma-print (cdr thing))
     (display ").")
     (newline))))

;;
;; Example translation:
;;
;; input tags:
;;    educational video from^4chan
;;      vs=X,Y,$ woman=X man=Y black=Y
;;      with=B,X=C,Y
;;      big=B beautiful=B
;;      big=C concentraition=C
;;      from^futurama,X
;;
;; desugared:
;;    educational=$ video=$ from=2,$ 4chan=2
;;      vs=X,Y,$ woman=X man=Y black=Y
;;      with=B,X with=C,Y
;;      big=B beautiful=B
;;      big=C concentraition=C
;;      from=3,X futurama=3
;;
;; output:
;;
;; t(educational, 1).
;; t(video, 1).
;; t(from, [2, 1]).
;; t('4chan', 2).
;; t(vs, [v1X, v1Y, 1]).
;; t(woman, v1X).
;; t(man, v1Y).
;; t(black, v1Y).
;; t(with, [v1B, v1X]).
;; t(big, v1B).
;; t(beautiful, v1B).
;; t(with, [v1Y, v1C]).
;; t(big, v1C).
;; t(concentraition, v1C).
;; t(from, [3, v1X]).
;; t(futurama, 3).
;;
;; p(id, 1, "id").
;; p(target, 1, "target").
;;
(define (tegfs-prolog)
  (define output-path (string-append (make-temporary-filename) ".pl"))
  (define output-port (open-file-port output-path "w"))

  ;; (parameterize ((current-output-port output-port))
  (parameterize ((current-output-port (current-output-port))) ;; DEBUG
    (display ":-style_check(-discontiguous).") (newline)
    (translate-registries yield-for-prolog)
    (display "what(X, Y) :- t(Y, X) ; t(K, Z), member(X, Z), Y = [K | Z].") (newline)
    )

  (exit 1) ;; DEBUG

  (close-port output-port)

  (system-fmt "prolog ~a" output-path))

(define (translate-registries yield)
  (define counter
    (let ((cnt 0))
      (lambda _ (set! cnt (+ 1 cnt)) cnt)))

  (for-each
   (lambda (registry-path0)
     (define registry-path (append-posix-path (root/p) registry-path0))
     (define input-port (open-file-port registry-path "r"))
     (for-each (translate-entry yield counter registry-path)
               (read-list input-port)))
   (get-registry-files)))

(define (handle-the-split yield cnt equal-split)
  (define pred-name (string->symbol (list->string (car equal-split))))

  (define (handle-variable name)
    (if (equal? "$" name)
        cnt
        (string->symbol (string-append "v" (number->string cnt) name))))

  (define variable-list (cadr equal-split))
  (define variables
    (map (comp list->string handle-variable)
         (list-split-on
          (comp (equal? #\,))
          variable-list)))

  (yield `(t ,pred-name ,@variables)))

(define (translate-tag yield counter cnt)
  (lambda (chars)
    (define equal-split (list-split-on (comp (equal? #\=)) chars))
    (handle-the-split yield cnt equal-split)))

;; returns list of characters!
(define (desugar-tag counter)
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
      (raisu 'used-conflicting-tag-syntaxes tag sum))

    (define (handle-prefix-suffix pre post)
      (let* ((arguments (list-split-on (comp (equal? #\,)) post))
             (do (unless (member (length arguments) '(1 2))
                   (raisu 'bad-number-of-arguments tag arguments)))
             (first-arg (car arguments))
             (second-arg (list-ref-or arguments 1 '(#\$)))
             (num (counter))
             (var (string->list (number->string num)))
             (first (append pre '(#\=) var '(#\,) second-arg))
             (second (append first-arg '(#\=) var)))
        (list first second)))

    (cond
     (span-^? (handle-prefix-suffix span-^-pre span-^-post))
     (span-_? (handle-prefix-suffix span-_-pre span-_-post))
     (equal-split? (map (comp (append (car equal-split) '(#\=))) (cdr equal-split)))
     (else (list (append chars '(#\=) '(#\$)))))))

(define (translate-property yield cnt registry-path)
  (lambda (property)
    (define name (car property))
    (define value (cdr property))
    (yield `(p ,name ,cnt ,(~a value)))))

(define (translate-entry yield counter registry-path)
  (lambda (entry)
    (define cnt (counter))

    (define id
      (cdr (or (assoc 'id entry)
               (raisu 'could-not-get-an-id entry))))

    (define tags
      (cdr (or (assoc 'tags entry)
               (raisu 'could-not-get-tags entry))))

    (define desugared (apply append (map (desugar-tag counter) tags)))

    (for-each (translate-tag yield counter cnt) desugared)
    (for-each (translate-property yield cnt registry-path) entry)

    ))





