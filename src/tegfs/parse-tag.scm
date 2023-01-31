;;;; Copyright (C) 2022  Otto Jung
;;;;
;;;; This program is free software: you can redistribute it and/or modify
;;;; it under the terms of the GNU Affero General Public License as published
;;;; by the Free Software Foundation, either version 3 of the License, or
;;;; (at your option) any later version.
;;;;
;;;; This program is distributed in the hope that it will be useful,
;;;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;;; GNU Affero General Public License for more details.
;;;;
;;;; You should have received a copy of the GNU Affero General Public License
;;;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

(cond-expand
 (guile
  (define-module (tegfs parse-tag)
    :export (parse-tag parse-tag-structure)
    :use-module ((euphrates comp) :select (comp))
    :use-module ((euphrates list-intersperse) :select (list-intersperse))
    :use-module ((euphrates list-split-on) :select (list-split-on))
    :use-module ((euphrates tilda-a) :select (~a))
    :use-module ((tegfs tag-structure-sep1) :select (tag-structure-sep1))
    :use-module ((tegfs tag-structure-sep2) :select (tag-structure-sep2))
    :use-module ((tegfs tags-this-variable) :select (tags-this-variable/char))
    )))



(define (parse-tag-structure/chars counter)
  (lambda (tag)
    (define str (~a tag))
    (define chars (string->list str))

    (define equal-split
      (list-split-on (comp (equal? tag-structure-sep1)) chars))
    (define equal-split?
      (not (null? (cdr equal-split))))

    (cond
     (equal-split?
      (let* ((head (car equal-split)))
        (map (lambda (split)
               (cons 'equality
                     (cons head (list-split-on (comp (equal? tag-structure-sep2)) split))))
             (cdr equal-split))))
     (else `((single ,chars))))))

;;
;; example input:
;;   with=X,Y,Z=A,B
;;
;; output:
;;   ((equality with X Y Z) (equality with A B))
;;
(define (parse-tag-structure counter)
  (define parser (parse-tag-structure/chars counter))
  (lambda (tag)
    (define structure/chars (parser tag))
    (map (lambda (x)
           (cons (car x)
                 (map (compose string->symbol list->string) (cdr x))))
         structure/chars)))

(define (desugar-tag/chars counter)
  (define parser (parse-tag-structure/chars counter))
  (lambda (tag)
    (define structure/chars (parser tag))
    (map
     (lambda (s)
       (case (car s)
         ((equality)
          (let* ((equal-split (cdr s)))
            (apply append `(,(car equal-split) (,tag-structure-sep1)
                            ,@(list-intersperse `(,tag-structure-sep2) (cdr equal-split))))))
         ((single) (append (cadr s) `(,tag-structure-sep1) `(,tags-this-variable/char)))))
     structure/chars)))

(define (parse-tag counter)
  (lambda (tag)
    (define desugared ((desugar-tag/chars counter) tag))
    (map
     (lambda (chars)
       (define equal-split (list-split-on (comp (equal? tag-structure-sep1)) chars))
       (define pred-name (string->symbol (list->string (car equal-split))))

       (define variable-list (cadr equal-split))
       (define variables
         (map list->string
              (list-split-on
               (comp (equal? tag-structure-sep2))
               variable-list)))

       (cons pred-name variables))
     desugared)))

