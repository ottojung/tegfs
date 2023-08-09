;;;; Copyright (C) 2022, 2023  Otto Jung
;;;; This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU Affero General Public License for more details. You should have received a copy of the GNU Affero General Public License along with this program.  If not, see <https://www.gnu.org/licenses/>.

(define (CLI::read-answer-string question)
  (let loop ()
    (define _2
      (begin
        (display " ")
        (display question)))

    ;; (define state (state/p))
    (define answer (read-string-line))
    (when (eof-object? answer)
      (fatal "Cancelled by user"))

    (define num (string->number answer))
    (if num
        (let ((name+setter (CLI::string-index->key num)))
          (if name+setter
              (parameterize ((CLI::swiched-field?/p #t))
                ((cdr name+setter) 'recalculate))
              (begin
                (dprintln "Bad index ~s, must be one of the listed items" num)
                (loop))))
        answer)))
