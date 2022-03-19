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

%use (curry-if) "./euphrates/curry-if.scm"
%use (list-span-while) "./euphrates/list-span-while.scm"
%use (list-split-on) "./euphrates/list-split-on.scm"
%use (list-ref-or) "./euphrates/list-ref-or.scm"
%use (comp) "./euphrates/comp.scm"
%use (raisu) "./euphrates/raisu.scm"

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

(define (parse-tag counter entry-cnt)
  (lambda (tag)
    (define desugared ((desugar-tag counter) tag))
    (map
     (lambda (chars)
       (define equal-split (list-split-on (comp (equal? #\=)) chars))
       (define pred-name (string->symbol (list->string (car equal-split))))

       (define (handle-variable name)
         (if (equal? "$" name)
             entry-cnt
             (string->symbol (string-append "v" (number->string entry-cnt) name))))

       (define variable-list (cadr equal-split))
       (define variables
         (map (comp list->string handle-variable)
              (list-split-on
               (comp (equal? #\,))
               variable-list)))

       (cons pred-name variables))
     desugared)))

