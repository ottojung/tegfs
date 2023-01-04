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

%var web-iterate-profun-results

%use (fn-alist) "./euphrates/fn-alist.scm"
%use (raisu) "./euphrates/raisu.scm"
%use (~s) "./euphrates/tilda-s.scm"
%use (words->string) "./euphrates/words-to-string.scm"
%use (web-bad-request) "./web-bad-request.scm"

(define (web-iterate-profun-results/2 res fun)
  (cond
   ((and (= 2 (length res))
         (equal? 'equals (cadr res)))
    (let ((equals (cadr (cadr res))))
      (for-each fun equals)))
   ((equal? '(true) res) (fun))
   ((equal? '(false) res) (raisu 'unexpected-false-from-backend-812731632))
   (else
    (raisu 'unexpected-its-from-backend-61253123543))))

(define (web-iterate-profun-results/fun results fun)
  (case (car result)
    ((its)
     (web-iterate-profun-results/2 (cadr result) fun))
    ((error)
     (web-bad-request
      "Error: ~a"
      (words->string (map ~s (cadr result)))))
    (else
     (raisu 'unexpected-result-from-backend-87156243510))))

(define-syntax web-iterate-profun-results
  (syntax-rules ()
    ((_ results (name . names) . bodies)
     (let ((fun (fn-alist (name . names) . bodies)))
       (web-iterate-profun-results/fun results fun)))))
