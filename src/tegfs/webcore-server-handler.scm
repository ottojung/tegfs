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

(cond-expand
 (guile
  (define-module (tegfs webcore-server-handler)
    :export (webcore::make-server-handler)
    :use-module ((euphrates profun-handler) :select (profun-handler-extend))
    :use-module ((tegfs add-entry) :select (add-entry))
    :use-module ((tegfs add-file-entry) :select (add-file-entry))
    :use-module ((tegfs add-tempentry) :select (add-tempentry))
    :use-module ((tegfs core-server-handler) :select (core::make-server-handler))
    :use-module ((tegfs permission) :select (permission-token))
    :use-module ((tegfs update-entry) :select (update-entry))
    :use-module ((tegfs update-tempentry) :select (update-tempentry))
    :use-module ((tegfs webcore-add-entry) :select (webcore::add-entry))
    :use-module ((tegfs webcore-add-file-entry) :select (webcore::add-file-entry))
    :use-module ((tegfs webcore-add-tempentry) :select (webcore::add-tempentry))
    :use-module ((tegfs webcore-categorization) :select (webcore::categorization))
    :use-module ((tegfs webcore-check-api-access) :select (webcore::check-api-access))
    :use-module ((tegfs webcore-collectgarbage) :select (webcore::collectgarbage))
    :use-module ((tegfs webcore-create-server-operator-permission-bang) :select (webcore::create-server-operator-permission!))
    :use-module ((tegfs webcore-entry) :select (webcore::entry))
    :use-module ((tegfs webcore-get-tempentry) :select (webcore::get-tempentry))
    :use-module ((tegfs webcore-key) :select (webcore::key))
    :use-module ((tegfs webcore-link-shared) :select (webcore::link-shared))
    :use-module ((tegfs webcore-login) :select (webcore::login))
    :use-module ((tegfs webcore-make-temporary-permissions) :select (webcore::make-temporary-permissions))
    :use-module ((tegfs webcore-senderid-to-entry) :select (webcore::senderid->entry))
    :use-module ((tegfs webcore-set-categorization) :select (webcore::set-categorization))
    :use-module ((tegfs webcore-share-entry-full) :select (webcore::share-entry-full))
    :use-module ((tegfs webcore-share-entry-preview) :select (webcore::share-entry-preview))
    :use-module ((tegfs webcore-share-entry) :select (webcore::share-entry))
    :use-module ((tegfs webcore-shared-entry-contains) :select (webcore::shared-entry-contains))
    :use-module ((tegfs webcore-time-left) :select (webcore::time-left))
    :use-module ((tegfs webcore-update-entry) :select (webcore::update-entry))
    :use-module ((tegfs webcore-update-tempentry) :select (webcore::update-tempentry))
    :use-module ((tegfs webcore-write-tempentries) :select (webcore::write-tempentries))
    )))



(define (webcore::make-just-the-server-handler web::context)
  (profun-handler-extend
   (core::make-server-handler)

   (login (webcore::login web::context))
   (key (webcore::key web::context))
   (entry (webcore::entry web::context)) ;; OVERRIDES
   (add-entry webcore::add-entry) ;; OVERRIDES
   (add-file-entry webcore::add-file-entry) ;; OVERRIDES
   (update-entry (webcore::update-entry web::context)) ;; OVERRIDES
   (categorization (webcore::categorization web::context)) ;; OVERRIDES
   (set-categorization (webcore::set-categorization web::context)) ;; OVERRIDES
   (share-preview (webcore::share-entry-preview web::context))
   (share-full (webcore::share-entry-full web::context))
   (share-entry (webcore::share-entry web::context))
   (senderid->entry (webcore::senderid->entry web::context))
   (link-shared (webcore::link-shared web::context))
   (shared-entry-contains (webcore::shared-entry-contains web::context))
   (make-temporary-permissions (webcore::make-temporary-permissions web::context))
   (collectgarbage (webcore::collectgarbage web::context))
   (time-left (webcore::time-left web::context))
   (add-tempentry (webcore::add-tempentry web::context))
   (update-tempentry (webcore::update-tempentry web::context))
   (get-tempentry (webcore::get-tempentry web::context))
   (write-tempentries (webcore::write-tempentries web::context))
   (check-api-access (webcore::check-api-access web::context))

   ))

(define (webcore::make-server-handler web::context)
  (define perm (webcore::create-server-operator-permission! web::context))
  (define key (permission-token perm))
  (values (webcore::make-just-the-server-handler web::context)
          key))
