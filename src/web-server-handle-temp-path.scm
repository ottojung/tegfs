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

%var web::server-handle-temp-path

%use (web-not-found) "./web-not-found.scm"
%use (web::redirect) "./web-redirect.scm"
%use (web::temp-path-get) "./web-temp-path-get.scm"
%use (web::temp-path-destination) "./web-temp-path.scm"

(define (web::server-handle-temp-path callctx tempid)
  (define path (web::temp-path-get tempid))
  (define destination (and path (web::temp-path-destination path)))
  (if destination
      (web::redirect callctx destination #f)
      (web-not-found)))
