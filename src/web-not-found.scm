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

%var web::not-found

%use (web::callcontext/p) "./web-callcontext-p.scm"
%use (callcontext-url) "./web-callcontext.scm"
%use (web::make-html-response) "./web-make-html-response.scm"

%for (COMPILER "guile")

(use-modules (web request)
             (web uri))

%end

(define (web::not-found)
  (define url (callcontext-url (web::callcontext/p)))
  (web::make-html-response
   (string-append "Resource not found: " url)
   #:status 404))
