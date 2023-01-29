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

%run guile

%var entry-get-mimetype

%use (assq-or) "./euphrates/assq-or.scm"
%use (raisu) "./euphrates/raisu.scm"
%use (keyword-mimetype) "./keyword-mimetype.scm"

(define (entry-get-mimetype entry)
  (unless (pair? entry)
    (raisu 'entry-is-not-a-list entry))

  (let ()
    (define t (assq-or keyword-mimetype entry #f))
    (and (string? t) t)))
