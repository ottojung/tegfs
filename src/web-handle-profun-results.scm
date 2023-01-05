;;;; Copyright (C) 2023  Otto Jung
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

%var web-handle-profun-results
%var web-handle-profun-results/or

%use (comp) "./euphrates/comp.scm"
%use (raisu) "./euphrates/raisu.scm"
%use (~s) "./euphrates/tilda-s.scm"
%use (words->string) "./euphrates/words-to-string.scm"
%use (web-bad-request) "./web-bad-request.scm"

(define (web-handle-profun-results/2 results fun fail-fun)
  (define head
    (if (null? results)
        (begin
          (fail-fun (cons 'its results))
          (raisu 'unexpected-null-from-backend-7126363))
        (car results)))

  (cond
   ((and (= 2 (length head))
         (equal? 'equals (car head)))
    (let ((equals (cadr head)))
      (fun equals)))

   ((and (= 3 (length head))
         (equal? '= (car head)))
    (let ((equals (list (map (comp (apply (lambda (EQ A B) (cons A B)))) results))))
      (fun equals)))

   ((equal? '((true)) results) (fun))
   ((equal? '((false)) results)
    (fail-fun (cons 'its results))
    (raisu 'unexpected-false-from-backend-812731632))
   (else
    (raisu 'unexpected-its-from-backend-61253123543 results))))

(define (web-handle-profun-results results fun)
  (web-handle-profun-results/or results fun identity))

(define (web-handle-profun-results/or results fun fail-fun)
  (case (car results)
    ((its)
     (web-handle-profun-results/2 (cdr results) fun fail-fun))
    ((error)
     (fail-fun results)
     ;; TODO: handle authorization errors differently
     (web-bad-request
      "Error: ~a"
      (words->string (map ~s (cadr results)))))
    (else
     (fail-fun results)
     (raisu 'unexpected-results-from-backend-87156243510 results))))
