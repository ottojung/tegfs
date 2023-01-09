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

%var web::body->hashmap

%use (web::query->hashmap) "./web-query-to-hashmap.scm"

%for (COMPILER "guile")
(use-modules (ice-9 iconv))
%end

(define (web::body->hashmap body/bytes)
  (define body
    (and body/bytes
         (bytevector->string body/bytes "utf-8")))

  (and body
       (web::query->hashmap body)))
