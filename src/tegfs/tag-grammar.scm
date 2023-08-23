;;;; Copyright (C) 2023  Otto Jung
;;;; This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU Affero General Public License for more details. You should have received a copy of the GNU Affero General Public License along with this program.  If not, see <https://www.gnu.org/licenses/>.

(define tag-grammar
  `( tag = flat-tag / sexp-tag
     sexp-tag = lbracket space* word separated-arg* space* rbracket
     flat-tag = word arg*
     separated-arg = space+ variable
     arg = equal idset
     idset = variable comma idset / variable
     word = wordc+ / quoted
     wordc = alnum / "%" / "-"
     variable = normal-variable / quoted-variable
     quoted-variable = quoted / ,tags-this-variable/string
     normal-variable = letter alnum*

     ;; tokens
     lbracket = "("
     rbracket = ")"
     comma = "," / "+"
     equal = "=" / ":"
     quoted = (re string)
     alnum = letter / digit
     letter = (re alpha)
     digit = (re numeric)
     space = (re " ")
     other = (re any)
     ))
