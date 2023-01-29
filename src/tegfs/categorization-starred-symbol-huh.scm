;;;; Copyright (C) 2022, 2023  Otto Jung
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

%var categorization-starred-symbol?

%use (comp) "./euphrates/comp.scm"
%use (compose-under) "./euphrates/compose-under.scm"
%use (list-last) "./euphrates/list-last.scm"
%use (list-or-map) "./euphrates/list-or-map.scm"
%use (tag-structure-sep1) "./tag-structure-sep1.scm"

(define categorization-starred-symbol?
  (comp symbol->string string->list
        ((compose-under
          and
          (comp (list-or-map (lambda (c) (member c `(,#\* ,tag-structure-sep1)))))
          (comp list-last (equal? tag-structure-sep1) not)))))
