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
  (define-module (tegfs web-get-full-link)
    :export (web::get-full-link)
    :use-module ((euphrates raisu) :select (raisu))
    :use-module ((tegfs a-weblink-q) :select (a-weblink?))
    )))



(define (web::get-full-link entry target maybe-senderid)
  (cond
   (maybe-senderid (string-append "full?vid=" maybe-senderid))
   ((not target) #f)
   ((a-weblink? target) target)
   (else
    (raisu 'not-enough-information-to-get-full-link entry))))
