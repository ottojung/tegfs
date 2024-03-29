;;;; Copyright (C) 2023  Otto Jung
;;;; This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU Affero General Public License for more details. You should have received a copy of the GNU Affero General Public License along with this program.  If not, see <https://www.gnu.org/licenses/>.

(define tag-grammar
  `( tag = flat-tag / sexp-tag

     ;; flat tags
     flat-tag = word arg*
     arg = equal idset
     idset = variable comma idset / variable

     ;; sexp tags
     sexp-tag = lbracket space* word separated-arg* space* rbracket
     separated-arg = space+ variable

     ;; words
     word = normal-word / quoted-word
     normal-word = wordc+
     quoted-word = quoted
     wordc = alnum / "%" / "-" / "<" / ">"

     ;; variables
     variable = normal-variable / quoted-variable
     quoted-variable = quoted / ,tags-this-variable/string
     normal-variable = letter alnum* / digit+

     ;; tokens
     lbracket = "("
     rbracket = ")"
     comma = "," / "+"
     equal = "=" / ":"
     quoted = (re string)
     alnum = letter / digit
     letter = (re (or alpha "_"))
     digit = (re numeric)
     newline = (re newline)
     space = (re whitespace)
     other = (re any)
     ))
