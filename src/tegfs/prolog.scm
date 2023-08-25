;;;; Copyright (C) 2022, 2023  Otto Jung
;;;; This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU Affero General Public License for more details. You should have received a copy of the GNU Affero General Public License along with this program.  If not, see <https://www.gnu.org/licenses/>.

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
    (dump-rules print-prolog-inference)
    (display prolog-additional-static-predicates)
    ret))

(define (tegfs-dump-prolog-file)
  (define output-path (string-append (make-temporary-filename/local) ".pl"))
  (define output-port (open-file-port output-path "w"))

  (parameterize ((current-output-port output-port))
    (tegfs-dump-prolog))

  (close-port output-port)

  output-path)

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

  (entries-for-each (translate-entry-tags* #t yield* counter))

  (for-each
   (lambda (tag)
     (yield `(tag ,tag)))
   (hashset->list tags-set))

  (< 1 (counter)))

(define (alpha-convert-variable cnt)
  (define prefix (string-append "v" (number->string cnt)))
  (lambda (name)
    (if (equal? name tags-this-variable/string)
        cnt
        (string-append prefix (~a name)))))

(define (translate-parsed-tag yield cnt)
  (define convert (alpha-convert-variable cnt))
  (lambda (t)
    (define converted (map convert (cdr t)))
    (yield `(t ,(car t) ,@converted))))

(define (translate-variable-binding yield cnt)
  (define convert (alpha-convert-variable cnt))
  (lambda (name)
    (unless (equal? (~a name) tags-this-variable/string)
      (yield `(v ,cnt ,(convert name))))))

(define (translate-entry-tags entry)
  (define counter (lambda _ 0))
  (define ret (stack-make))
  (define (yield thing)
    (when (equal? 't (car thing))
      (stack-push! ret (cdr thing))))
  ((translate-entry-tags* #f yield counter) entry)
  (stack->list ret))

(define (translate-entry-tags* with-variables? yield counter)
  (define (add-special-sorted-status-tag tags)
    (if (null? tags)
        (cons '%unsorted tags)
        tags))
  (define (add-special-locality-tag target tags)
    (if target
        (if (a-weblink? target)
            (cons '%remote tags)
            (cons '%local tags))
        (cons '%notarget tags)))

  (lambda (entry)
    (define cnt (counter))

    (define id
      (assq-or keyword-id entry (raisu 'could-not-get-an-id entry)))
    (define tags0
      (assq-or keyword-tags entry '()))
    (define target
      (entry-get-target entry))
    (define tags
      (appcomp tags0
               add-special-sorted-status-tag
               (add-special-locality-tag target)))

    (define parser (make-tag-parser counter))
    (define parsed-tags (apply append (map parser tags)))

    (for-each (translate-parsed-tag yield cnt) parsed-tags)
    (when with-variables?
      (let ()
        (define variables
          (list-deduplicate/reverse
           (apply append (map cdr parsed-tags))))
        (for-each (translate-variable-binding yield cnt) variables)))
    (yield `(i ,cnt ,id))

    ))





