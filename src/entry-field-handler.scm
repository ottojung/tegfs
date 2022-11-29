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

%var entry-field-handler

%use (assq-or) "./euphrates/assq-or.scm"
%use (bool->profun-result) "./euphrates/bool-to-profun-result.scm"
%use (list-and-map) "./euphrates/list-and-map.scm"
%use (make-profun-error) "./euphrates/profun-error.scm"
%use (profun-op-function) "./euphrates/profun-op-function.scm"
%use (profun-reject) "./euphrates/profun-reject.scm"

(define entry-field-handler
  (profun-op-function
   2 (lambda (entry field)
       (bool->profun-result
        (if (and (list? entry)
                 (list-and-map pair? entry))
            (let ((field* (cond
                           ((symbol? field) field)
                           ((string? field) (string->symbol field))
                           (else #f))))
              (if field*
                  (assq-or field* entry (profun-reject))
                  (make-profun-error 'not-a-valid-field field)))
            (make-profun-error 'not-a-valid-entry entry))))))
