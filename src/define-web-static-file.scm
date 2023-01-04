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

%run guile

%var define-web-static-file


%use (raisu) "./euphrates/raisu.scm"
%use (web-basic-headers) "./web-basic-headers.scm"

%for (COMPILER "guile")

(use-modules (web response))
(use-modules (ice-9 iconv))
(use-modules (rnrs bytevectors))

%end

(define (web-respond-with-a-file type bv)
  (values
   (build-response
    #:code 200
    #:headers
    (append web-basic-headers
            `((content-type . ,type)
              (Cache-Control . "max-age=3600, public, private"))))
   bv))

(define-syntax define-web-static-file
  (syntax-rules ()
    ((_ name type content)
     (define name
       (let ()
         (define bv
           (cond
            ((bytevector? content) content)
            ((string? content) (string->utf8 content))
            (else (raisu 'unknown-content-type content))))
         (lambda _ (web-respond-with-a-file type bv)))))))