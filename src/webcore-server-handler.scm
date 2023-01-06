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

%var webcore::make-server-handler

%use (profun-handler-extend) "./euphrates/profun-handler.scm"
%use (add-entry) "./add-entry.scm"
%use (core::make-server-handler) "./core-server-handler.scm"
%use (webcore::login) "./webcore-login.scm"
%use (webcore::make-temporary-permissions) "./webcore-make-temporary-permissions.scm"
%use (webcore::shared-entry-contains) "./webcore-shared-entry-contains.scm"
%use (webcore::key) "./webcore-key.scm"
%use (webcore::collectgarbage) "./webcore-collectgarbage.scm"
%use (webcore::link-shared) "./webcore-link-shared.scm"
%use (webcore::senderid->entry) "./webcore-senderid-to-entry.scm"
%use (webcore::share-entry-full) "./webcore-share-entry-full.scm"
%use (webcore::share-entry) "./webcore-share-entry.scm"
%use (webcore::share-entry-preview) "./webcore-share-entry-preview.scm"
%use (webcore::add-entry) "./webcore-add-entry.scm"
%use (webcore::entry) "./webcore-entry.scm"

(define (webcore::make-server-handler web::context)
  (profun-handler-extend
   (core::make-server-handler)

   (login (webcore::login web::context))
   (key (webcore::key web::context))
   (entry (webcore::entry web::context)) ;; OVERRIDES
   (add-entry webcore::add-entry) ;; OVERRIDES
   (share-preview (webcore::share-entry-preview web::context))
   (share-full (webcore::share-entry-full web::context))
   (share-entry (webcore::share-entry web::context))
   (senderid->entry (webcore::senderid->entry web::context))
   (link-shared (webcore::link-shared web::context))
   (shared-entry-contains (webcore::shared-entry-contains web::context))
   (make-temporary-permissions (webcore::make-temporary-permissions web::context))
   (collectgarbage (webcore::collectgarbage web::context))

   ))
