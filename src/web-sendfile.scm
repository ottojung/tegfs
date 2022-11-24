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

%var web-sendfile

%use (web-basic-headers) "./web-basic-headers.scm"

%for (COMPILER "guile")

(use-modules (web response))
(use-modules (ice-9 binary-ports))

%end

(define (web-sendfile return! content-type filepath)
  (define port (open-file filepath "rb"))
  (define bv (get-bytevector-all port))
  (close-port port)
  (return!
   (build-response
    #:code 200
    #:headers
    (append web-basic-headers
            `((content-type . (,content-type))
              (Cache-Control . "no-cache"))))
   bv))
