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

%var web-make-server-handler

%use (profun-handler-extend) "./euphrates/profun-handler.scm"
%use (tegfs-make-server-handler) "./tegfs-server-handler.scm"
%use (web-link-shared-handler) "./web-link-shared-handler.scm"
%use (web-share-entry-full-handler) "./web-share-entry-full-handler.scm"
%use (web-share-entry-preview-handler) "./web-share-entry-preview-handler.scm"

(define (web-make-server-handler web-context)
  (profun-handler-extend
   (tegfs-make-server-handler)

   (share-preview (web-share-entry-preview-handler web-context))
   (share-full (web-share-entry-full-handler web-context))

   (link-shared (web-link-shared-handler web-context))

   ))
