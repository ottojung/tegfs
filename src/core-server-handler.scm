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
%use (core::categorization) "./core-categorization.scm"
%use (core::entry-field) "./core-entry-field.scm"
%use (core::entry) "./core-entry.scm"
%use (core::diropen?/p core::dirpreview?/p core::query/p) "./core-paremeters.scm"
%use (core::set-categorization) "./core-set-categorization.scm"

(define (core::make-server-handler)
  (profun-handler-extend
   profun-standard-handler

   (entry core::entry)
   (entry-field core::entry-field)
   (add-entry core::add-entry)
   (categorization core::categorization)
   (set-categorization core::set-categorization)

   (query (instantiate-profun-parameter core::query/p))
   (diropen? (instantiate-profun-parameter core::diropen?/p))
   (dirpreview? (instantiate-profun-parameter core::dirpreview?/p))

   (value (profun-op-value '() '()))

   ))
