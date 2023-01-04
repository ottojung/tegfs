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

%var core::make-server-handler

%use (profun-handler-extend) "./euphrates/profun-handler.scm"
%use (instantiate-profun-parameter) "./euphrates/profun-op-parameter.scm"
%use (profun-op-value) "./euphrates/profun-op-value.scm"
%use (profun-standard-handler) "./euphrates/profun-standard-handler.scm"
%use (add-entry) "./add-entry.scm"
%use (core::add-entry) "./core-add-entry.scm"
%use (core-entry-handler) "./core-entry-handler.scm"
%use (entry-field-handler) "./entry-field-handler.scm"
%use (query-diropen?/p query-dirpreview?/p query-split/p) "./talk-parameters.scm"

(define (core::make-server-handler)
  (profun-handler-extend
   profun-standard-handler

   (entry core-entry-handler)
   (entry-field entry-field-handler)
   (add-entry core::add-entry)

   (query (instantiate-profun-parameter query-split/p))
   (diropen? (instantiate-profun-parameter query-diropen?/p))
   (dirpreview? (instantiate-profun-parameter query-dirpreview?/p))

   (value (profun-op-value '() '()))

   ))
