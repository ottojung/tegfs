;;;; Copyright (C) 2022  Otto Jung
;;;;
;;;; This program is free software: you can redistribute it and/or modify
;;;; it under the terms of the GNU General Public License as published by
;;;; the Free Software Foundation; version 3 of the License.
;;;;
;;;; This program is distributed in the hope that it will be useful,
;;;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;;; GNU General Public License for more details.
;;;;
;;;; You should have received a copy of the GNU General Public License
;;;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

%run guile

%var core-query

%use (tegfs-query/diropen) "./tegfs-query-diropen.scm"

(define (core-query query/split)
  (define perm (web-get-permissions))
  (define (for-each-fn entry)
    (define target-fullpath
      (entry-target-fullpath entry))

    (when (has-access-for-entry-full? perm entry)
      (monadic-do target-fullpath '(say target-fullpath)))

    (when (has-access-for-entry-details? perm entry)
      (monadic-do entry '(say entry)))

    

    )

  (tegfs-query/diropen query/split for-each-fn))
