;;;; Copyright (C) 2022, 2023  Otto Jung
;;;; This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU Affero General Public License for more details. You should have received a copy of the GNU Affero General Public License along with this program.  If not, see <https://www.gnu.org/licenses/>.

(define (parse-inference yield args)
  (define split (list-split-on (comp (equal? "=>")) args))
  (define len (length split))

  (unless (equal? 2 len)
    (fatal "Inference ~s should have had precisely one => arrow, but it has ~s of them" args (- len 1)))

  (define-tuple (antecedents consequents) split)
  (define-values (RHS-parts RHS-variables)
    (prolog-query-parse antecedents))
  (for-each (lambda (c)
              (define consequent (car (prolog-query-parse (list c))))
              (yield (cons 'inference (cons consequent RHS-parts))))
            consequents))

(define (parse-synonyms yield args)
  (define len (length args))
  (unless (< 1 len)
    (fatal "Found an empty synonym"))

  (define main (car args))
  (define rest (cdr args))
  (for-each
   (lambda (synonym)
     (yield (list 'inference (string->symbol synonym) (string->symbol main)))
     (yield (list 'inference (string->symbol main) (string->symbol synonym))))
   rest))

(define (parse-symmetric yield args)
  (define len (length args))
  (unless (equal? 1 len)
    (fatal "Symmetric rule ~s should have had exactly one argument, but it has ~s" args len))

  (define name (car args))
  (define parsed ((make-term-parser tags-this-variable/string) name))
  (define parsed-length (length parsed))
  (unless (equal? 1 parsed-length)
    (fatal "Parsed symmetric relation ~s should be simple (without args), but it has ~s arguments"
           args (- parsed-length 1)))
  (yield
   (list 'inference
         (list (string->symbol name) 'Y 'X)
         (list (string->symbol name) 'X 'Y) (make-prolog-cut-symbol))))

(define (parse-rule yield)
  (lambda (words)
    (define command (string->symbol (car words)))
    (define args (cdr words))
    (case command
      ((rule) (parse-inference yield args))
      ((synonyms) (parse-synonyms yield args))
      ((symmetric) (parse-symmetric yield args))
      (else (raisu 'unknown-command words command)))))

(define (dump-rules-from-text yield text)
  (define lines (string->lines text))
  (for-each
   (lambda (line)
     (define-values (uncommented semi comment)
       (string-split-3 #\; line))
     (define words (string->words uncommented))
     (unless (null? words)
       ((parse-rule yield) words)))
   lines))
