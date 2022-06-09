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

%var parse-tag
%var parse-tag-structure

%use (curry-if) "./euphrates/curry-if.scm"
%use (list-span-while) "./euphrates/list-span-while.scm"
%use (list-split-on) "./euphrates/list-split-on.scm"
%use (list-ref-or) "./euphrates/list-ref-or.scm"
%use (comp) "./euphrates/comp.scm"
%use (raisu) "./euphrates/raisu.scm"
%use (list-intersperse) "./euphrates/list-intersperse.scm"
%use (~a) "./euphrates/tilda-a.scm"

%use (tags-this-variable/char) "./tags-this-variable.scm"
%use (tag-structure-sep1) "./tag-structure-sep1.scm"
%use (tag-structure-sep2) "./tag-structure-sep2.scm"

%use (debugv) "./euphrates/debugv.scm"

(define (parse-tag-structure/chars counter)
  (lambda (tag)
    (define str (~a tag))
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
      (list-split-on (comp (equal? tag-structure-sep1)) chars))
    (define equal-split?
      (not (null? (cdr equal-split))))

    (define sum
      (+ (if span-^? 1 0)
         (if span-_? 1 0)
         (if equal-split? 1 0)))

    (unless (>= 1 sum)
      (raisu 'used-conflicting-tag-syntaxes tag sum))

    (cond
     (span-^? `((prefix-suffix ,span-^-pre ,span-^-post)))
     (span-_? `((prefix-suffix ,span-_-pre ,span-_-post)))
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

    (define (handle-prefix-suffix pre post)
      (let* ((arguments (list-split-on (comp (equal? tag-structure-sep2)) post))
             (do (unless (member (length arguments) '(1 2))
                   (raisu 'bad-number-of-arguments tag arguments)))
             (first-arg (car arguments))
             (second-arg (list-ref-or arguments 1 `(,tags-this-variable/char)))
             (num (counter))
             (var (string->list (~a num)))
             (first (append pre `(,tag-structure-sep1) var `(,tag-structure-sep2) second-arg))
             (second (append first-arg `(,tag-structure-sep1) var)))
        (list first second)))

    (apply
     append
     (map
      (lambda (s)
        (case (car s)
          ((prefix-suffix) (apply handle-prefix-suffix (cdr s)))
          ((equality)
           (let* ((equal-split (cdr s)))
             (list
              (apply append `(,(car equal-split) (,tag-structure-sep1)
                              ,@(list-intersperse `(,tag-structure-sep2) (cdr equal-split)))))))
          ((single) (list (append (cadr s) `(,tag-structure-sep1) `(,tags-this-variable/char))))))
      structure/chars))))

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

