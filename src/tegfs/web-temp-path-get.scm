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
  (define-module (tegfs web-temp-path-get)
    :export (web::temp-path-get)
    :use-module ((euphrates assq-or) :select (assq-or))
    :use-module ((tegfs web-iterate-profun-results) :select (web::iterate-profun-results))
    :use-module ((tegfs webcore-ask) :select (webcore::ask))
    )))



(define (web::temp-path-get server-operator-key tempid/0)
  (define tempid
    (if (string-prefix? "/" tempid/0)
        (substring tempid/0 1)
        tempid/0))

  (define results
    (webcore::ask
     `(whats
       (key ,server-operator-key)
       (get-tempentry ,tempid E))))

  (web::iterate-profun-results
   :results results
   (E)
   (and E (assq-or 'destination E #f))))
