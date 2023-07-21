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
  (define-module (tegfs web-get-preview-link)
    :export (web::get-preview-link)
    :use-module ((euphrates raisu) :select (raisu))
    :use-module ((tegfs keyword-config-xdgopen-fileserver) :select (keyword-config-xdgopen-fileserver))
    :use-module ((tegfs web-current-fileserver-p) :select (web::current-fileserver/p))
    )))

(define (web::get-preview-link linkpath)
  (define fileserver (web::current-fileserver/p))
  (cond
   ((equal? fileserver keyword-config-xdgopen-fileserver)
    (string-append "file?path=" linkpath))
   ((string? fileserver)
    (string-append fileserver linkpath))
   (else
    (raisu 'unexpected-value-of-fileserver fileserver))))
