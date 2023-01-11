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

;; Returns a tree that represents the tags structure
%var categorization-parse-tags

%use (curry-if) "./euphrates/curry-if.scm"
%use (list-deduplicate) "./euphrates/list-deduplicate.scm"
%use (list-map/flatten) "./euphrates/list-map-flatten.scm"
%use (CFG-CLI->CFG-AST) "./euphrates/parse-cfg-cli.scm"
%use (read-list) "./euphrates/read-list.scm"
%use (categorization-split) "./categorization-split.scm"

(define (categorization-parse-tags categorization-text)
  (define-values (cfg-part rules-part)
    (categorization-split categorization-text))

  (define words
    (with-input-from-string cfg-part
      (lambda _
        (read-list (current-input-port)))))

  (define words-flat
    (list-map/flatten (curry-if pair? identity list) words))

  (define ast
    (CFG-CLI->CFG-AST words-flat))

  (define ast/flatten
    (map (lambda (production)
           (cons (car production)
                 (list-deduplicate
                  (apply append (cdr production)))))
         ast))

  ast/flatten)
