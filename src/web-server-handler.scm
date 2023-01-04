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

%var web-make-server-handler

%use (profun-handler-extend) "./euphrates/profun-handler.scm"
%use (add-entry) "./add-entry.scm"
%use (core::make-server-handler) "./core-server-handler.scm"
%use (login-handler) "./login-handler.scm"
%use (make-temporary-permissions-handler) "./make-temporary-permissions-handler.scm"
%use (shared-entry-contains-handler) "./shared-entry-contains-handler.scm"
%use (tegfs-key-handler) "./tegfs-key-handler.scm"
%use (web-collectgarbage-handler) "./web-collectgarbage-handler.scm"
%use (web-link-shared-handler) "./web-link-shared-handler.scm"
%use (web-senderid->entry-handler) "./web-senderid-to-entry-handler.scm"
%use (web-share-entry-full-handler) "./web-share-entry-full-handler.scm"
%use (web-share-entry-handler) "./web-share-entry-handler.scm"
%use (web-share-entry-preview-handler) "./web-share-entry-preview-handler.scm"
%use (webcore::add-entry) "./webcore-add-entry.scm"
%use (webcore::entry) "./webcore-entry-handler.scm"

(define (web-make-server-handler web-context)
  (profun-handler-extend
   (core::make-server-handler)

   (login (login-handler web-context))
   (key (tegfs-key-handler web-context))
   (entry (webcore::entry web-context)) ;; OVERRIDES
   (add-entry webcore::add-entry) ;; OVERRIDES
   (share-preview (web-share-entry-preview-handler web-context))
   (share-full (web-share-entry-full-handler web-context))
   (share-entry (web-share-entry-handler web-context))
   (senderid->entry (web-senderid->entry-handler web-context))
   (link-shared (web-link-shared-handler web-context))
   (shared-entry-contains (shared-entry-contains-handler web-context))
   (make-temporary-permissions (make-temporary-permissions-handler web-context))
   (collectgarbage (web-collectgarbage-handler web-context))

   ))
