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

%var tegfs-query/diropen

%use (keyword-diropen) "./keyword-diropen.scm"
%use (tegfs-query/open) "./tegfs-query-open.scm"

(define (tegfs-query/diropen <query-format> for-each-fn)
  (define opening-properties
    (list keyword-diropen))
  (tegfs-query/open
   opening-properties <query-format> for-each-fn))
