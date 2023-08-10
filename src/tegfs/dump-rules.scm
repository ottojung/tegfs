;;;; Copyright (C) 2022, 2023  Otto Jung
;;;; This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU Affero General Public License for more details. You should have received a copy of the GNU Affero General Public License along with this program.  If not, see <https://www.gnu.org/licenses/>.

(define (dump-rules yield)
  (define rules-file (append-posix-path (get-root) rules-filename))
  (define rules-text
    (if (file-or-directory-exists? rules-file)
        (read-string-file rules-file)
        (let ((ret ";; This file is for logical rules for tags\n"))
          (write-string-file rules-file ret)
          ret)))
  (dump-rules-from-text yield rules-text))
