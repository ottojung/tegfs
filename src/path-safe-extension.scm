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

%var path-safe-extension

%use (alphanum/alphabet/index) "./euphrates/alphanum-alphabet.scm"
%use (comp) "./euphrates/comp.scm"
%use (compose-under) "./euphrates/compose-under.scm"
%use (list-and-map) "./euphrates/list-and-map.scm"
%use (path-extension) "./euphrates/path-extension.scm"

(define (path-safe-extension path)
  (define max-size 7)
  (define ext (path-extension path))

  (if (and (>= max-size (string-length ext))
           (list-and-map (compose-under or (comp (equal? #\.)) alphanum/alphabet/index)
                         (string->list ext)))
      ext
      ""))
