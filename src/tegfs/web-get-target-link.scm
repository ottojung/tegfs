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
  (define-module (tegfs web-get-target-link)
    :export (web::get-target-link)
    :use-module ((euphrates raisu) :select (raisu))
    :use-module ((tegfs a-weblink-q) :select (a-weblink?))
    :use-module ((tegfs keyword-config-xdgopen-fileserver) :select (keyword-config-xdgopen-fileserver))
    :use-module ((tegfs web-current-fileserver-p) :select (web::current-fileserver/p))
    )))

(define (web::get-target-link linkpath)
  (define fileserver (web::current-fileserver/p))
  (cond
   ((a-weblink? linkpath)
    linkpath)
   ((string-prefix? "/directory" linkpath)
    linkpath)
   (else
    (cond
     ((string? fileserver)
      (string-append fileserver linkpath))
     ((equal? fileserver keyword-config-xdgopen-fileserver)
      (string-append "open?path=" linkpath))
     (else
      (raisu 'unexpected-value-of-fileserver fileserver))))))
