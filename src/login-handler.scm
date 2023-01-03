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

%run guile

%var login-handler

%use (hashmap-ref) "./euphrates/hashmap.scm"
%use (profun-set-parameter) "./euphrates/profun-accept.scm"
%use (make-profun-error) "./euphrates/profun-error.scm"
%use (profun-op-envlambda) "./euphrates/profun-op-envlambda.scm"
%use (profun-request-value) "./euphrates/profun-request-value.scm"
%use (profun-unbound-value?) "./euphrates/profun-value.scm"
%use (default-login-expiery-time) "./default-login-expiery-time.scm"
%use (make-permission!) "./make-permission-bang.scm"
%use (sha256sum) "./sha256sum.scm"
%use (tegfs-permissions/p) "./talk-parameters.scm"
%use (context-passwords) "./web-context.scm"

(define login-handler
  (lambda (tegfs-context)
    (define passwords (context-passwords tegfs-context))

    (profun-op-envlambda
     (ctx env (P-name))

     (define password (env P-name))
     (define hashed
       (and (string? password)
            (sha256sum password)))
     (define registered?
       (and hashed
            (hashmap-ref passwords hashed #f)))

     (cond
      ((profun-unbound-value? password)
       (profun-request-value P-name))

      ((not registered?)
       (make-profun-error 'not-authorized "Bad password here"))

      (else
       (let ()
           (define admin? #t) ;; TODO: read from the config
           (define uploadaccess? #t) ;; TODO: read from the config
           (define detailsaccess? #t) ;; TODO: read from the config
           (define share-longer-than-view? #t) ;; TODO: read from the config
           (define perm
             (make-permission!
              tegfs-context
              default-login-expiery-time
              admin?
              uploadaccess?
              detailsaccess?
              share-longer-than-view?))
           (profun-set-parameter (tegfs-permissions/p <- perm))))))))
