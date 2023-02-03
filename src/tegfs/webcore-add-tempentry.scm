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
  (define-module (tegfs webcore-add-tempentry)
    :export (webcore::add-tempentry)
    :use-module ((euphrates catchu-case) :select (catchu-case))
    :use-module ((euphrates profun-accept) :select (profun-accept profun-set-meta))
    :use-module ((euphrates profun-default) :select (profun-default))
    :use-module ((euphrates profun-error) :select (make-profun-error profun-error?))
    :use-module ((euphrates profun-op-lambda) :select (profun-op-lambda))
    :use-module ((euphrates profun-request-value) :select (profun-request-value))
    :use-module ((euphrates profun-value) :select (profun-bound-value? profun-unbound-value?))
    :use-module ((euphrates stringf) :select (stringf))
    :use-module ((tegfs add-tempentry) :select (add-tempentry))
    :use-module ((tegfs keyword-stime) :select (keyword-stime))
    :use-module ((tegfs permission) :select (permission?))
    :use-module ((tegfs webcore-access) :select (can-manage-tempentries?))
    :use-module ((tegfs webcore-context) :select (context-tempentries))
    :use-module ((tegfs webcore-get-current-permissions) :select (webcore::get-current-permissions))
    )))

(define (webcore::add-tempentry web::context)
  (define tempentries (context-tempentries web::context))

  (profun-op-lambda
   :with-env
   (ctx (id tempentry) (ID-name E-name))

   (define perm (webcore::get-current-permissions))
   (define id* (profun-default id #f))

   (cond
    ((not (permission? perm))
     (make-profun-error 'permission-denied "Not authorized. Missing key?"))
    ((not (can-manage-tempentries? perm))
     (make-profun-error 'permission-denied "This user cannot create new tempentries"))
    ((profun-unbound-value? tempentry)
     (profun-request-value E-name))

    (else
     (let ((created-id
            (catchu-case
             (add-tempentry tempentries id* tempentry)
             (('tempentry-must-have-stime-set)
              (make-profun-error
               'type-error
               (stringf "Tempentry is missing the ~s field" keyword-stime))))))
       (cond
        ((profun-error? created-id) created-id)
        ((profun-bound-value? id) (profun-accept))
        (else
         (profun-set-meta (ID-name <- created-id)))))))))
