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

%var tegfs-make-server-handler
%var tegfs-make-server-handler/c

%use (profun-handler-extend) "./euphrates/profun-handler.scm"
%use (instantiate-profun-parameter) "./euphrates/profun-op-parameter.scm"
%use (profun-op-value) "./euphrates/profun-op-value.scm"
%use (profun-standard-handler) "./euphrates/profun-standard-handler.scm"
%use (add-entry-handler) "./add-entry-handler.scm"
%use (add-entry) "./add-entry.scm"
%use (entry-field-handler) "./entry-field-handler.scm"
%use (query-diropen?/p query-dirpreview?/p query-split/p) "./talk-parameters.scm"
%use (query-entry-handler) "./tegfs-entry-handler.scm"
%use (web-make-context) "./web-make-context.scm"

(define (tegfs-make-server-handler)
  (define context (web-make-context)) ;; TODO: make a non-web context
  (tegfs-make-server-handler/c context))

(define (tegfs-make-server-handler/c tegfs-context)
  (profun-handler-extend
   profun-standard-handler

   (entry (query-entry-handler tegfs-context))
   (entry-field entry-field-handler)
   (add-entry (add-entry-handler tegfs-context))

   (query (instantiate-profun-parameter query-split/p))
   (diropen? (instantiate-profun-parameter query-diropen?/p))
   (dirpreview? (instantiate-profun-parameter query-dirpreview?/p))

   (value (profun-op-value '() '()))

   ))
