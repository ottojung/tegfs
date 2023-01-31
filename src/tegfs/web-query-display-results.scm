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

(cond-expand
 (guile
  (define-module (tegfs web-query-display-results)
    :export (web::query-display-results)
    :use-module ((euphrates fn-alist) :select (fn-alist))
    :use-module ((tegfs web-display-entry) :select (web::display-entry))
    )))



(define (web::query-display-results equals)
  (display "<div class='cards'>")
  (for-each
   (fn-alist
    (E F PL)
    (define entry E)
    (define maybe-full-senderid F)
    (define preview-link PL)
    (web::display-entry entry maybe-full-senderid preview-link))
   equals)
  (display "</div>"))
