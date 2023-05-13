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
  (define-module (tegfs entry-card-name)
    :export (make-entry-card-name entry-card-name-id)
    :use-module ((euphrates string-split-3) :select (string-split-3))
    )))

(define (make-entry-card-name maybe-full-senderid id)
  (string-append (if maybe-full-senderid "s" "i")
                 ":"
                 (or maybe-full-senderid id)))

(define (entry-card-name-id name)
  (define-values (pref sep id)
    (string-split-3 ":" name))

  (and (not (string-null? sep)) id))
