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

%var web::iterate-profun-results
%var web::iterate-profun-results/or

%use (fn-alist) "./euphrates/fn-alist.scm"
%use (web::handle-profun-results/default-fail-fun web::handle-profun-results/or) "./web-handle-profun-results.scm"

(define-syntax web::iterate-profun-results
  (syntax-rules ()
    ((_ results (name . names) . bodies)
     (web::iterate-profun-results/or
      web::handle-profun-results/default-fail-fun
      results
      (name . names)
      . bodies))))

(define-syntax web::iterate-profun-results/or
  (syntax-rules ()
    ((_ fail-fun results (name . names) . bodies)
     (let* ((fun (fn-alist (name . names) . bodies))
            (fun*
             (lambda (equals)
               (cond
                ((null? equals) 'false)
                ((null? (cdr equals))
                 (fun (car equals)))
                (else
                 (for-each fun equals))))))
       (web::handle-profun-results/or results fun* fail-fun)))))
