;;;; Copyright (C) 2023  Otto Jung
;;;; This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU Affero General Public License for more details. You should have received a copy of the GNU Affero General Public License along with this program.  If not, see <https://www.gnu.org/licenses/>.

(define backend-parser
  (lalr-parser/simple
   `(:grammar ,tag-grammar
     :join (normal-variable quoted-variable word)
     :inline (variable word arg* separated-arg separated-arg* idset)
     :skip (equal comma lbracket rbracket space space+ space*))))

(define (tag->parse-tree tag)
  (define (errorp . args) #f)
  (backend-parser errorp (~a tag)))
