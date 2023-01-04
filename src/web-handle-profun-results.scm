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

%use (raisu) "./euphrates/raisu.scm"
%use (~s) "./euphrates/tilda-s.scm"
%use (words->string) "./euphrates/words-to-string.scm"
%use (web-bad-request) "./web-bad-request.scm"

(define (web-handle-profun-results/2 res fun)
  (cond
   ((and (= 2 (length res))
         (equal? 'equals (cadr res)))
    (let ((equals (cadr (cadr res))))
      (fun equals)))
   ((equal? '(true) res) (fun))
   ((equal? '(false) res) (raisu 'unexpected-false-from-backend-812731632))
   (else
    (raisu 'unexpected-its-from-backend-61253123543))))

(define (web-handle-profun-results results fun)
  (case (car results)
    ((its)
     (web-handle-profun-results/2 (cadr results) fun))
    ((error)
     (web-bad-request
      "Error: ~a"
      (words->string (map ~s (cadr results)))))
    (else
     (raisu 'unexpected-results-from-backend-87156243510))))
