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
  (define-module (tegfs web-query-to-hashmap)
    :export (web::query->hashmap)
    :use-module ((euphrates hashmap) :select (alist->hashmap))
    :use-module ((euphrates string-split-3) :select (string-split-3))
    :use-module ((euphrates string-split-simple) :select (string-split/simple))
    :use-module ((tegfs web-try-uri-decode) :select (web::try-uri-decode)))))



(define (web::query->hashmap query)
  (define split (string-split/simple query #\&))
  (define key-values
    (map (lambda (sp)
           (define-values (key eq val) (string-split-3 #\= sp))
           (cons (string->symbol key) (web::try-uri-decode val)))
         split))
  (alist->hashmap key-values))
