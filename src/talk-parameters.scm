;;;; Copyright (C) 2022  Otto Jung
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

%var query-split/p
%var query-permissions/p
%var query-filemap/2/p
%var query-diropen?/p
%var query-dirpreview?/p

%use (make-profun-parameter) "./euphrates/profun-op-parameter.scm"

(define query-split/p
  (make-profun-parameter))
(define query-permissions/p
  (make-profun-parameter))
(define query-filemap/2/p
  (make-profun-parameter))
(define query-diropen?/p
  (make-profun-parameter))
(define query-dirpreview?/p
  (make-profun-parameter))
