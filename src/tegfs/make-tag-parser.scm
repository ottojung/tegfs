;;;; Copyright (C) 2022, 2023  Otto Jung
;;;; This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU Affero General Public License for more details. You should have received a copy of the GNU Affero General Public License along with this program.  If not, see <https://www.gnu.org/licenses/>.

(define tag-grammar
  `( tag = word arg*
     arg = equal idset
     idset = variable comma idset / variable
     word = wordc+
     wordc = alnum / "%" / "-"
     variable = letter alnum*  / ,tags-this-variable/string
     alnum = letter / digit
     letter = (re alpha)
     digit = (re numeric)
     comma = "," / "+"
     equal = "=" / ":"
     ))

(define backend-parser
  (lalr-parser/simple
   `(:grammar ,tag-grammar
     :join (variable word)
     :inline (variable word arg* idset)
     :skip (equal comma))))

(define (tag->tree tag)
  (define (errorp . args) #f)
  (backend-parser errorp (~a tag)))

(define (make-tag-structure-parser/chars counter)
  (lambda (tag)
    (define str (~a tag))
    (when (string-null? str)
      (raisu 'type-error "Empty tag is not a tag"))

    (define chars (string->list str))

    (define equal-split
      (list-split-on (comp (equal? tag-structure-sep1)) chars))

    (when (null? (car equal-split))
      (raisu 'type-error (stringf "Tag cannot begin with ~s" (~a tag-structure-sep1))))

    (define equal-split?
      (not (null? (cdr equal-split))))

    (when (list-or-map null? (cdr equal-split))
      (raisu 'type-error "Tag arguments cannot be empty"))

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
;;   ((equality with "X" "Y" "Z") (equality with "A" "B"))
;;
(define (make-tag-structure-parser counter)
  (define parser (make-tag-structure-parser/chars counter))
  (lambda (tag)
    (define structure/chars (parser tag))
    (map (lambda (x)
           (define op (car x))
           (define args (cdr x))
           (cons op (cons (string->symbol (list->string (car args)))
                          (map list->string (cdr args)))))
         structure/chars)))

(define tags-this-variable/chars
  (string->list tags-this-variable/string))

(define (desugar-tag/chars counter)
  (define parser (make-tag-structure-parser/chars counter))
  (lambda (tag)
    (define structure/chars (parser tag))
    (map
     (lambda (s)
       (case (car s)
         ((equality)
          (let* ((equal-split (cdr s)))
            (apply append `(,(car equal-split) (,tag-structure-sep1)
                            ,@(list-intersperse `(,tag-structure-sep2) (cdr equal-split))))))
         ((single) (append (cadr s) `(,tag-structure-sep1) tags-this-variable/chars))))
     structure/chars)))

(define (make-tag-parser/old counter)
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

(define (make-tag-parser counter)
  (lambda (tag)
    (define single-tree (tag->tree tag))

    (unless single-tree
      (raisu* :from "make-tag-parser"
              :type 'type-error
              :message "Input tag is malformed"
              :args (list tag)))

    (define pred
      (string->symbol
       (cadr single-tree)))

    (define args0
      (map cdr (cddr single-tree)))

    (define args
      (if (null? args0)
          (list (list tags-this-variable/string))
          args0))

    (define tags
      (cartesian-product
       (list pred) args))

    tags))
