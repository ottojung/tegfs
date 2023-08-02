;;;; Copyright (C) 2022, 2023  Otto Jung
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
  (define-module (tegfs categorization-complete-selection-cont)
    :export (categorization-complete-selection/cont)
    :use-module ((euphrates comp) :select (comp))
    :use-module ((euphrates const) :select (const))
    :use-module ((euphrates curry-if) :select (curry-if))
    :use-module ((euphrates fn) :select (fn))
    :use-module ((euphrates identity) :select (identity))
    :use-module ((euphrates list-and-map) :select (list-and-map))
    :use-module ((euphrates list-deduplicate) :select (list-deduplicate list-deduplicate/reverse))
    :use-module ((euphrates list-find-first) :select (list-find-first))
    :use-module ((euphrates list-get-duplicates) :select (list-get-duplicates))
    :use-module ((euphrates list-singleton-q) :select (list-singleton?))
    :use-module ((euphrates multiset) :select (list->multiset multiset->list multiset-filter))
    :use-module ((euphrates negate) :select (negate))
    :use-module ((euphrates raisu) :select (raisu))
    :use-module ((euphrates tilda-a) :select (~a))
    :use-module ((tegfs categorization-starred-symbol-huh) :select (categorization-starred-symbol?))
    :use-module ((tegfs categorization-to-prolog-full) :select (categorization->prolog/full))
    :use-module ((tegfs categorization-translate-choices) :select (categorization-translate-choices))
    :use-module ((tegfs parse-tag) :select (parse-tag))
    :use-module ((tegfs profun-compute-ground) :select (profun-compute-ground))
    :use-module ((tegfs unparse-tag) :select (unparse-tag))
    :use-module ((tegfs unstar-symbol) :select (unstar-symbol))
    )))

(define (type-symbol? x)
  (define s (~a x))
  (or (string-prefix? "<" s)
      (string-suffix? ">" s)))

;; transitive over type-symbols, but not a general transitive closure
(define (get-parents ast/flatten tag/starred)
  (let loop ((tag/starred tag/starred) (buf '()))
    (when (member tag/starred buf)
      (raisu 'cycle-detected! tag/starred buf))

    (apply
     append
     (filter
      identity
      (map
       (lambda (production)
         (and (member tag/starred (cdr production))
              (let ((immediate (car production)))
                (if (type-symbol? immediate)
                    (loop immediate (cons tag/starred buf))
                    (list immediate)))))
       ast/flatten)))))

;; transitive over symbols that have single parent
(define (get-parents/transitive/reflexive ast/flatten ast/flatten/unstarred closed tag/starred)
  (define (get initial)
    (let loop ((tag/starred initial) (ast ast/flatten) (buf '()))
      (when (member tag/starred buf)
        (raisu 'cycle-detected! tag/starred buf))

      (let* ((immediate-parents
              (filter
               identity
               (map
                (lambda (production)
                  (and (member tag/starred (cdr production))
                       (car production)))
                ast)))
             (parent/starred
              (if (list-singleton? immediate-parents)
                  (car immediate-parents)
                  (list-find-first (fn member % closed) #f immediate-parents))))
        (if parent/starred
            (let ((parent (unstar-symbol parent/starred)))
              (cons parent (loop parent ast/flatten/unstarred (cons tag/starred buf))))
            '()))))

  (define ret
    ((curry-if null? (const (get (unstar-symbol tag/starred))))
     (get tag/starred)))
  (cons tag/starred ret))

(define (categorization-complete-selection/cont ast/flatten all-tags starred)
  (define-values (translated-tree ambiguous-branches)
    (categorization->prolog/full ast/flatten))

  (define parser (parse-tag 0))
  (define translated-choices
    (categorization-translate-choices parser ast/flatten starred))

  (define rules
    (append translated-choices
            translated-tree
            `(((%any X)))))

  (define result
    (profun-compute-ground rules all-tags))

  (define result/for-humans
    (map unparse-tag result))

  (define selected (map unstar-symbol starred))

  (define result/final
    (list-deduplicate/reverse
     (filter (negate type-symbol?)
             (append selected result/for-humans))))

  (define ambiguous
    (filter
     (lambda (x)
       (and (member (car x) selected)
            (not (type-symbol? x))
            (list-and-map
             (lambda (specializer)
               (not (member specializer result/final)))
             (cdr x))))
     ambiguous-branches))

  (define duplicates
    (map car (list-get-duplicates selected)))

  (append
   (list (cons 'ok result/final))
   (if (null? ambiguous) '()
       (list (cons 'ambiguous ambiguous)))
   (if (null? duplicates) '()
       (list (cons 'duplicates duplicates)))
   ))

(define (categorization-complete-selection/cont/old ast/flatten all-tags starred)
  (define starred/unstarred
    (map unstar-symbol starred))

  (define starred-productions
    (filter categorization-starred-symbol? (map car ast/flatten)))

  (define starred-non-productions
    (apply
     append
     (map (lambda (production)
            (filter categorization-starred-symbol? (cdr production)))
          ast/flatten)))

  (define ast/flatten/unstarred
    (map (lambda (production)
           (cons
            (unstar-symbol (car production))
            (list-deduplicate
             (map unstar-symbol (cdr production)))))
         ast/flatten))

  (define main-production
    (car (car ast/flatten)))

  (define (closure closed tag/starred)
    (get-parents/transitive/reflexive ast/flatten ast/flatten/unstarred closed tag/starred))

  (define transitive-closures
    (map (comp (closure '())) starred))

  (define closed
    (list-deduplicate
     (map unstar-symbol
          (apply append
                 (filter
                  (comp (member main-production))
                  transitive-closures)))))

  (define (ambiguous? tag/starred)
    (define cl (closure closed tag/starred))
    (not (member main-production cl)))

  (define ambiguous
    (filter ambiguous? starred))

  (define ok-tags
    (filter
     (negate type-symbol?)
     (filter
      (negate (comp (equal? main-production)))
      (list-deduplicate
       (map unstar-symbol (apply append transitive-closures))))))

  (define ambiguous-tags
    (and (not (null? ambiguous))
         (map
          (lambda (amb)
            (define unstarred (unstar-symbol amb))
            (cons unstarred
                  (map unstar-symbol (get-parents ast/flatten/unstarred unstarred))))
          ambiguous)))

  (define duplicates
    (let* ((S (list->multiset starred/unstarred))
           (S* (multiset-filter
                S (lambda (key value) (> value 1))))
           (L (multiset->list S*)))
      (and (not (null? L))
           L)))

  (append
   `((ok . ,ok-tags))
   (if ambiguous-tags
       (list (cons 'ambiguous ambiguous-tags))
       (list))
   (if duplicates
       (list (cons 'duplicates duplicates))
       (list))))
