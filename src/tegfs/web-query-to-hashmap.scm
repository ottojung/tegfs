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

%var web::query->hashmap

%use (alist->hashmap) "./euphrates/hashmap.scm"
%use (string-split-3) "./euphrates/string-split-3.scm"
%use (string-split/simple) "./euphrates/string-split-simple.scm"
%use (web::try-uri-decode) "./web-try-uri-decode.scm"

(define (web::query->hashmap query)
  (define split (string-split/simple query #\&))
  (define key-values
    (map (lambda (sp)
           (define-values (key eq val) (string-split-3 #\= sp))
           (cons (string->symbol key) (web::try-uri-decode val)))
         split))
  (alist->hashmap key-values))
