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
  (define-module (tegfs web-server-handle-temp-path)
    :export (web::server-handle-temp-path)
    :use-module ((tegfs web-not-found) :select (web::not-found))
    :use-module ((tegfs web-redirect) :select (web::redirect))
    :use-module ((tegfs web-temp-path-get) :select (web::temp-path-get))
    :use-module ((tegfs web-temp-path) :select (web::temp-path-destination))
    )))



(define (web::server-handle-temp-path callctx tempid)
  (define tpath (web::temp-path-get tempid))
  (define destination (and tpath (web::temp-path-destination tpath)))
  (if destination
      (web::redirect callctx destination #f)
      (web::not-found)))
