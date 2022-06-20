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
%var tegfs-dump-prolog

%use (make-temporary-filename) "./euphrates/make-temporary-filename.scm"
%use (system-fmt) "./euphrates/system-fmt.scm"
%use (append-posix-path) "./euphrates/append-posix-path.scm"
%use (dprintln) "./euphrates/dprintln.scm"
%use (file-or-directory-exists?) "./euphrates/file-or-directory-exists-q.scm"
%use (write-string-file) "./euphrates/write-string-file.scm"
%use (read-string-file) "./euphrates/read-string-file.scm"
%use (comp) "./euphrates/comp.scm"
%use (fn) "./euphrates/fn.scm"
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
%use (list-deduplicate/reverse) "./euphrates/list-deduplicate.scm"
%use (make-hashset hashset-add! hashset->list) "./euphrates/ihashset.scm"

%use (categorization-filename) "./categorization-filename.scm"
%use (tags-this-variable/string) "./tags-this-variable.scm"
%use (get-registry-files) "./get-registry-files.scm"
%use (parse-tag) "./parse-tag.scm"
%use (print-tag-as-prolog-term) "./tag-to-prolog-term.scm"
%use (entries-for-each) "./entries-for-each.scm"
%use (id-name) "./id-name.scm"
%use (dump-rules) "./dump-rules.scm"

(define (tegfs-prolog/parse)
  (tegfs-prolog)
  (dprintln "done(prolog)."))

(define (yield-for-prolog thing)
  (print-tag-as-prolog-term thing)
  (display ".\n"))

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
;; i(1, "id").
;; v(X, Y) :- false.
;;
(define (tegfs-dump-prolog)
  (display ":-style_check(-discontiguous).\n")
  (let ((ret
         (translate-registries yield-for-prolog)))
    (dump-rules)
    (display "v(_, _) :- false.\n")
    (display "t('%diff', [X, Y]) :- X \\= Y.\n")
    (display "t('%any', _).\n")
    (display "vandthis(This, X) :- X = This ; v(This, X).\n")
    ;; (display "what(X, Y) :- t(Y, X) ; t(K, Z), member(X, Z), Y = [K | Z].") (newline)
    ret))

(define (tegfs-dump-prolog-file)
  (define output-path (string-append (make-temporary-filename) ".pl"))
  (define output-port (open-file-port output-path "w"))

  (parameterize ((current-output-port output-port))
    (tegfs-dump-prolog))

  (close-port output-port)

  output-path)

(define (tegfs-prolog)
  (define output-path (tegfs-dump-prolog-file))
  (system-fmt "prolog ~a" output-path))

(define (translate-registries yield)
  (define counter
    (let ((cnt 0))
      (lambda _ (set! cnt (+ 1 cnt)) cnt)))

  (define tags-set (make-hashset))
  (define yield*
    (lambda (thing)
      (case (car thing)
        ((t) (hashset-add! tags-set (car (cdr thing)))))
      (yield thing)))

  (entries-for-each (translate-entry yield* counter))

  (for-each
   (lambda (tag)
     (yield `(tag ,tag)))
   (hashset->list tags-set))

  (< 1 (counter)))

(define (alpha-convert-variable cnt)
  (lambda (name)
    (if (equal? name tags-this-variable/string)
        cnt
        (string->symbol (string-append "v" (number->string cnt) name)))))

(define (translate-parsed-tag yield cnt)
  (define convert (alpha-convert-variable cnt))
  (lambda (t)
    (define converted (map convert (cdr t)))
    (define arg (if (null? (cdr converted))
                    (car converted)
                    (list->vector converted)))
    (yield `(t ,(car t) ,arg))))

(define (translate-variable-binding yield cnt)
  (define convert (alpha-convert-variable cnt))
  (lambda (name)
    (unless (equal? (~a name) tags-this-variable/string)
      (yield `(v ,cnt ,(convert name))))))

(define (translate-entry yield counter)
  (lambda (entry)
    (define cnt (counter))

    (define id
      (cdr (or (assoc id-name entry)
               (raisu 'could-not-get-an-id entry))))

    (define tags
      (cdr (or (assoc 'tags entry)
               (cons 'tags '()))))

    (define parser (parse-tag counter))
    (define parsed-tags (apply append (map parser tags)))
    (define variables
      (list-deduplicate/reverse
       (apply append (map cdr parsed-tags))))

    (for-each (translate-parsed-tag yield cnt) parsed-tags)
    (for-each (translate-variable-binding yield cnt) variables)
    (yield `(i ,cnt ,id))

    ))





