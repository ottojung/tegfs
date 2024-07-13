;;;; Copyright (C) 2023, 2024  Otto Jung
;;;; This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU Affero General Public License for more details. You should have received a copy of the GNU Affero General Public License along with this program.  If not, see <https://www.gnu.org/licenses/>.

(define (tag->parse-tree tag)
  (define (errorp . args) #f)
  (define s
    (cond
     ((string? tag) tag)
     ((symbol? tag) (symbol->string tag))
     ((list? tag) (~s tag))
     (else (raisu* :from "tag->parse-tree"
                   :type 'type-error
                   :message "Tag to parse tree transformer expected either a tag in the form of a string, a symbol or a list, but got something else"
                   :args (list tag)))))

  (parselynn:simple:run/with-error-handler
   tag->parse-tree:parser:implementation
   errorp s))
