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

%var tegfs-server-handler

%use (profun-handler-extend) "./euphrates/profun-handler.scm"
%use (instantiate-profun-parameter) "./euphrates/profun-op-parameter.scm"
%use (profun-op-value) "./euphrates/profun-op-value.scm"
%use (profun-standard-handler) "./euphrates/profun-standard-handler.scm"
%use (entry-field-handler) "./entry-field-handler.scm"
%use (query-diropen?/p query-dirpreview?/p query-filemap/2/p query-permissions/p query-split/p) "./talk-parameters.scm"
%use (query-entry-handler) "./tegfs-query.scm"
%use (web-share-entry-preview-handler) "./web-share-entry-preview-handler.scm"

(define tegfs-server-handler
  (profun-handler-extend
   profun-standard-handler

   (value (profun-op-value '() '()))

   (entry query-entry-handler)
   (entry-field entry-field-handler)

   (query (instantiate-profun-parameter query-split/p))
   (permissions (instantiate-profun-parameter query-permissions/p))
   (filemap/2 (instantiate-profun-parameter query-filemap/2/p))
   (diropen? (instantiate-profun-parameter query-diropen?/p))
   (dirpreview? (instantiate-profun-parameter query-dirpreview?/p))

   (entry-preview web-share-entry-preview-handler)

   ))
