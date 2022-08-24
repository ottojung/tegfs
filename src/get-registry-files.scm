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

%var get-registry-files

%use (string-split/simple) "./euphrates/string-split-simple.scm"
%use (get-config) "./get-config.scm"
%use (get-root) "./get-root.scm"

(define (get-registry-files)
  (define root (get-root))
  (define config (get-config))
  (define registries/string
    (cadr (or (assoc 'registries config) '(registries ""))))
  (define registries (string-split/simple registries/string #\:))
  registries)
