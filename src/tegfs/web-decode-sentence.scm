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
  (define-module (tegfs web-decode-sentence)
    :export (web::decode-sentence)
    :use-module ((euphrates string-to-words) :select (string->words))
    :use-module ((euphrates un-tilda-s) :select (un~s))
    :use-module ((tegfs prolog-query-parse) :select (prolog-query-parse))
    :use-module ((tegfs prolog-var) :select (prolog-var-name prolog-var?))
    )))

(define (generify-query query)
  (define (place-variables clause)
    (map
     (lambda (obj)
       (if (prolog-var? obj)
           (let ((name (prolog-var-name obj)))
             (cond
              ((symbol? name) name)
              (else (un~s name))))
           obj))
     clause))
  (map place-variables (map cdr query)))

(define (web::decode-sentence sentence/encoded)
  (generify-query
   (prolog-query-parse
    (string->words sentence/encoded))))
