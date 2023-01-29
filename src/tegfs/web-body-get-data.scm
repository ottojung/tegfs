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

%var web::body::get-data
%var web::body::get-data/decode

%use (assq-or) "./euphrates/assq-or.scm"
%use (hashmap-ref) "./euphrates/hashmap.scm"

%for (COMPILER "guile")
(use-modules (ice-9 iconv))
%end

(define (web::body::get-data body/hash a-key)
  (define alist
    (hashmap-ref body/hash a-key '()))
  (assq-or 'data alist #f))

(define (web::body::get-data/decode body/hash a-key)
  (define data (web::body::get-data body/hash a-key))
  (and data (bytevector->string data "utf-8")))
