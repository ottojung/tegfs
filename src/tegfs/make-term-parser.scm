;;;; Copyright (C) 2023  Otto Jung
;;;; This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU Affero General Public License for more details. You should have received a copy of the GNU Affero General Public License along with this program.  If not, see <https://www.gnu.org/licenses/>.

(define (make-term-parser counter)
  (lambda (tag)
    (define single-tree
      (tag->parse-tree tag))

    (unless single-tree
      (raisu* :from "make-tag-parser"
              :type 'type-error
              :message "Input tag is malformed"
              :args (list tag)))

    (define tag-body (cadr single-tree))

    (define (get-pred tag-body)
      (define pred-p (cadr tag-body))
      (define pred-type (car pred-p))

      (string->symbol
       (cond
        ((equal? pred-type 'normal-word)
         (cadr pred-p))
        ((equal? pred-type 'quoted-word)
         (un~s (cadr pred-p)))
        (else (raisu 'impossible-word-type pred-p tag-body)))))

    (cond
     ((equal? 'flat-tag (car tag-body))
      (let ()
        (define pred (get-pred tag-body))

        (define args0
          (map cdr (cddr tag-body)))

        (define args1
          (map (lambda (vars)
                 (map (lambda (var)
                        (cond
                         ((equal? 'normal-variable (car var))
                          (or (string->number (cadr var))
                              (string->symbol (cadr var))))
                         ((equal? 'quoted-variable (car var))
                          (if (equal? (cadr var) tags-this-variable/string)
                              (cadr var)
                              (let ((x (un~s (cadr var))))
                                (if (string? x)
                                    (or (string->number x) x)
                                    x))))
                         (else (raisu 'impossible-var-type var))))
                      vars))
               args0))

        (define args
          (if (null? args1)
              (list (list tags-this-variable/string))
              args1))

        (define tags
          (cartesian-product
           (list pred) args))

        tags))

     ((equal? 'sexp-tag (car tag-body))
      (let ()
        (define pred (get-pred tag-body))

        (define args0
          (cddr tag-body))

        (define args1
          (map (lambda (var)
                 (cond
                  ((equal? 'normal-variable (car var))
                   (or (string->number (cadr var))
                       (string->symbol (cadr var))))
                  ((equal? 'quoted-variable (car var))
                   (if (equal? (cadr var) tags-this-variable/string)
                       (cadr var)
                       (let ((x (un~s (cadr var))))
                         (if (string? x)
                             (or (string->number x) x)
                             x))))
                  (else (raisu 'impossible-var-type var))))
               args0))

        (define args
          (if (null? args1)
              (list tags-this-variable/string)
              args1))

        (define tags
          (list (cons pred args)))

        tags))

     (else
      (raisu 'impossible-tag-type tag-body)))))
