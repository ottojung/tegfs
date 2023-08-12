;;;; Copyright (C) 2022, 2023  Otto Jung
;;;; This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU Affero General Public License for more details. You should have received a copy of the GNU Affero General Public License along with this program.  If not, see <https://www.gnu.org/licenses/>.

(define (CLI::read-enumeration name option-list/0)
  (define option-list (map ~a option-list/0))
  (define hint/inner
    (apply string-append (list-intersperse "/" option-list)))
  (define hint
    (stringf "~a (~a)\n " name hint/inner))
  (define option-list/down
    (map string-downcase option-list))

  (let loop ()
    (define answer (string-downcase (CLI::read-answer-string hint)))
    (if (member answer option-list/down)
        (string->symbol answer)
        (begin
          (dprintln "\nPlease choose one of the following: ~a" hint/inner)
          (loop)))))
