;;;; Copyright (C) 2023  Otto Jung
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
  (define-module (tegfs webcore-get-tempentry)
    :export (webcore::get-tempentry)
    :use-module ((euphrates hashmap) :select (hashmap-ref))
    :use-module ((euphrates profun-accept) :select (profun-set))
    :use-module ((euphrates profun-error) :select (make-profun-error))
    :use-module ((euphrates profun-op-lambda) :select (profun-op-lambda))
    :use-module ((euphrates profun-request-value) :select (profun-request-value))
    :use-module ((euphrates profun-value) :select (profun-bound-value? profun-unbound-value?))
    :use-module ((tegfs webcore-access) :select (can-manage-tempentries?))
    :use-module ((tegfs webcore-context) :select (context-tempentries))
    :use-module ((tegfs webcore-get-current-permissions) :select (webcore::get-current-permissions))
    )))

(define (webcore::get-tempentry webcore::context)
  (define tempentries (context-tempentries webcore::context))
  (profun-op-lambda
   :with-env
   (ctx (id TE) (ID-name TE-name))

   (define perm (webcore::get-current-permissions))

   (cond
    ((profun-unbound-value? id)
     (profun-request-value ID-name))
    ((profun-bound-value? TE)
     (make-profun-error 'type-error "Entry variable must be free"))
    ((not (can-manage-tempentries? perm))
     (make-profun-error 'permission-denied "This user cannot access tempentries directly"))
    (else
     (profun-set (TE-name <- (hashmap-ref tempentries id #f)))))))
