;;;; Copyright (C) 2023  Otto Jung
;;;; This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU Affero General Public License for more details. You should have received a copy of the GNU Affero General Public License along with this program.  If not, see <https://www.gnu.org/licenses/>.

(define (CLI::string-index->key i0)
  (define setters (alist-initialize!:current-setters))
  (define i (- i0 1))
  (if (< i 0) #f
      (let loop ((setters setters) (i i))
        (if (null? setters) #f
            (let ((x (car setters)))
              (if (= 0 i) x
                  (loop (cdr setters) (- i 1))))))))
