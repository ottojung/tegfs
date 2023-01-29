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
  (define-module (tegfs web-hashmap-to-query)
    :export (web::hashmap->query)
    :use-module ((euphrates hashmap) :select (hashmap-foreach))
    :use-module ((euphrates tilda-a) :select (~a))
    :use-module ((euphrates uri-encode) :select (uri-encode)))))



(define (web::hashmap->query qH)
  (define ret "")
  (hashmap-foreach
   (lambda (key val)
     (define s (string-append (~a key) "=" (uri-encode (~a val))))
     (unless (string-null? ret)
       (set! ret (string-append ret "&")))
     (set! ret (string-append ret s)))
   qH)
  ret)
