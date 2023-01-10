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

%var webcore::login

%use (hashmap-ref) "./euphrates/hashmap.scm"
%use (profun-set-parameter) "./euphrates/profun-accept.scm"
%use (make-profun-error) "./euphrates/profun-error.scm"
%use (profun-op-lambda) "./euphrates/profun-op-lambda.scm"
%use (profun-request-value) "./euphrates/profun-request-value.scm"
%use (profun-unbound-value?) "./euphrates/profun-value.scm"
%use (default-login-expiery-time) "./default-login-expiery-time.scm"
%use (make-permission!) "./make-permission-bang.scm"
%use (password->tokenlike) "./password-to-tokenlike.scm"
%use (sha256sum) "./sha256sum.scm"
%use (context-passwords context-tokens) "./web-context.scm"
%use (webcore::permissions/p) "./webcore-parameters.scm"

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
         (define maybepassword password)

         ;; TODO: read below from the config
         (define admin? #t)
         (define dynamic '())

         (define perm
           (make-permission!
            webcore::context
            default-login-expiery-time
            admin? maybepassword dynamic))
         (profun-set-parameter (webcore::permissions/p <- perm))))

      (temporary
       (profun-set-parameter (webcore::permissions/p <- temporary)))

      (else
       (make-profun-error 'permission-denied "Bad password here"))))))
