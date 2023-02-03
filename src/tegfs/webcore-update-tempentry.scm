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
  (define-module (tegfs webcore-update-tempentry)
    :export (webcore::update-tempentry)
    :use-module ((euphrates catchu-case) :select (catchu-case))
    :use-module ((euphrates profun-accept) :select (profun-accept))
    :use-module ((euphrates profun-error) :select (make-profun-error))
    :use-module ((euphrates profun-op-lambda) :select (profun-op-lambda))
    :use-module ((euphrates profun-request-value) :select (profun-request-value))
    :use-module ((euphrates profun-value) :select (profun-unbound-value?))
    :use-module ((tegfs update-tempentry) :select (update-tempentry))
    :use-module ((tegfs webcore-access) :select (can-manage-tempentries?))
    :use-module ((tegfs webcore-context) :select (context-tempentries))
    :use-module ((tegfs webcore-get-current-permissions) :select (webcore::get-current-permissions))
    )))

;; Replaces `original-tempentry' by the `updated-tempentry'.
;; Note that if `updated-tempentry' is `#f', then the `original-tempentry' is deleted.

(define (webcore::update-tempentry webcore::context)
  (define tempentries (context-tempentries webcore::context))
  (profun-op-lambda
   :with-env env
   (ctx (original-tempentry updated-tempentry) (O-name U-name))

   (define perm (webcore::get-current-permissions))

   (cond
    ((profun-unbound-value? original-tempentry)
     (profun-request-value O-name))
    ((profun-unbound-value? updated-tempentry)
     (profun-request-value U-name))
    ((not (can-manage-tempentries? perm))
     (make-profun-error 'permission-denied "This user cannot modify tempentries"))

    (else
     (catchu-case
      (begin
        (update-tempentry tempentries original-tempentry updated-tempentry)
        (profun-accept))

      (('type-error . args)
       (apply
        make-profun-error
        (cons 'type-error args)))

      (('permission-denied . args)
       (apply
        make-profun-error
        (cons 'permission-denied args))))))))
