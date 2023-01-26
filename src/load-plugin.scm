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

%var load-plugin

%use (eval-in-current-namespace) "./euphrates/eval-in-current-namespace.scm"
%use (read-string-file) "./euphrates/read-string-file.scm"
%use (plugin-ctr) "./plugin.scm"

(define (load-text/fn text)
  (eval-in-current-namespace text))

(define (load-plugin filepath)
  (define text (read-string-file filepath))
  (define fun (load-text/fn text))
  (plugin-ctr fun))
