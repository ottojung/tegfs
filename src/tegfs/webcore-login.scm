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
  (define-module (tegfs webcore-login)
    :export (webcore::login)
    :use-module ((euphrates hashmap) :select (hashmap-ref))
    :use-module ((euphrates profun-accept) :select (profun-set-parameter))
    :use-module ((euphrates profun-error) :select (make-profun-error))
    :use-module ((euphrates profun-op-lambda) :select (profun-op-lambda))
    :use-module ((euphrates profun-request-value) :select (profun-request-value))
    :use-module ((euphrates profun-value) :select (profun-unbound-value?))
    :use-module ((tegfs default-login-expiery-time) :select (default-login-expiery-time))
    :use-module ((tegfs password-to-tokenlike) :select (password->tokenlike))
    :use-module ((tegfs sha256sum) :select (sha256sum))
    :use-module ((tegfs web-context) :select (context-passwords context-tokens))
    :use-module ((tegfs webcore-create-admin-permission-bang) :select (webcore::create-admin-permission!))
    :use-module ((tegfs webcore-parameters) :select (webcore::permissions/p))
    )))



(define webcore::login
  (lambda (webcore::context)
    (define passwords (context-passwords webcore::context))
    (define tokens (context-tokens webcore::context))

    (profun-op-lambda
     :with-env
     (ctx (password) (P-name))

     (define hashed
       (and (string? password)
            (sha256sum password)))
     (define registered?
       (and hashed
            (hashmap-ref passwords hashed #f)))
     (define temporary
       (and hashed
            (let ((tokenlike (password->tokenlike password)))
              (hashmap-ref tokens tokenlike #f))))

     (cond
      ((profun-unbound-value? password)
       (profun-request-value P-name))

      (registered?
       (let ()
         (define perm
           (webcore::create-admin-permission!
            webcore::context password default-login-expiery-time))
         (profun-set-parameter (webcore::permissions/p <- perm))))

      (temporary
       (profun-set-parameter (webcore::permissions/p <- temporary)))

      (else
       (make-profun-error 'permission-denied "Bad password here"))))))
