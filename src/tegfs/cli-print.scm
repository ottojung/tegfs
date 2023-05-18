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
  (define-module (tegfs cli-print)
    :export (CLI::print)
    :use-module ((tegfs entry-target-fullpath) :select (entry-target-fullpath))
    :use-module ((tegfs fatal) :select (fatal))
    :use-module ((tegfs get) :select (tegfs-get))
    )
  (use-modules (ice-9 binary-ports))
  (use-modules (scheme base))
  ))

(define (print-file-contents filename)
  (define p (current-output-port))
  (define input (open-file filename "r"))
  (define buffer (make-bytevector 4096))
  (let loop ()
    (let ((bytes-read (get-bytevector-n! input buffer 0 4096)))
      (unless (or (eof-object? bytes-read) (zero? bytes-read))
        (display (utf8->string buffer))
        (loop))))
  (close-input-port input)
  (force-output p))

(define (CLI::print <entry-id>)
  (define entry (tegfs-get <entry-id>))
  (unless entry
    (fatal "Entry with id ~s not found" <entry-id>))

  (let ()
    (define fullpath (entry-target-fullpath entry))
    (when fullpath
      (print-file-contents fullpath))))
