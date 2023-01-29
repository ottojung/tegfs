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

%run guile

%var prolog-query-parse

%use (comp) "./euphrates/comp.scm"
%use (curry-if) "./euphrates/curry-if.scm"
%use (fn-cons) "./euphrates/fn-cons.scm"
%use (list-deduplicate/reverse) "./euphrates/list-deduplicate.scm"
%use (~a) "./euphrates/tilda-a.scm"
%use (parse-tag) "./parse-tag.scm"
%use (make-prolog-var) "./prolog-var.scm"
%use (tags-this-variable/string) "./tags-this-variable.scm"

(define (prolog-query-parse <query...>)
  (define (tovar x)
    (make-prolog-var
     ((curry-if (comp (equal? tags-this-variable/string)) (const 'This)) x)))

  (define parser (parse-tag tags-this-variable/string))
  (define (maybe-parse tag)
    (if (or (string? tag) (symbol? tag) (number? tag))
        (parser tag)
        (list tag)))

  (define parsed-query-0
    (apply
     append
     (map maybe-parse <query...>)))
  (define (convert-arguments args)
    (define mapped (map tovar args))
    (list
     (if (null? (cdr mapped))
         (car mapped)
         (list->vector mapped))))

  (define parsed-query-1 (map (curry-if pair? (fn-cons identity convert-arguments)) parsed-query-0))
  (define parsed-query (map (curry-if pair? (comp (cons 't))) parsed-query-1))
  (define this-strings `(,tags-this-variable/string "This"))
  (define (get-args term)
    (if (pair? term) (cdr term) '()))

  (define variables
    (filter (lambda (v) (not (member (~a v) this-strings)))
            (list-deduplicate/reverse
             (apply append (map get-args parsed-query-0)))))
  (values parsed-query variables))
