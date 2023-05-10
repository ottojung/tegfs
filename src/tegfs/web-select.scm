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
  (define-module (tegfs web-select)
    :export (web::select)
    :use-module ((euphrates debugv) :select (debugv))
    :use-module ((euphrates hashmap) :select (hashmap->alist))
    :use-module ((tegfs web-body-not-found) :select (web::body-not-found))
    :use-module ((tegfs web-callcontext-p) :select (web::callcontext/p))
    :use-module ((tegfs web-callcontext) :select (callcontext-body))
    :use-module ((tegfs web-make-html-response) :select (web::make-html-response))
    :use-module ((tegfs web-parse-multipart) :select (parse-multipart-as-hashmap))
    )))



(define (web::select)
  (define callctx (web::callcontext/p))
  (define body/bytes (callcontext-body callctx))

  (cond
   ((not body/bytes)
    (web::body-not-found))
   (else
    (let ()
      (define body/hash
        (parse-multipart-as-hashmap body/bytes))

      (debugv (map car (hashmap->alist body/hash)))

      (web::make-html-response "OKAY YOUR GOOD")))))
