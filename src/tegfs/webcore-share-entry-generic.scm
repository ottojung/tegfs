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
  (define-module (tegfs webcore-share-entry-generic)
    :export (webcore::share-entry-generic)
    :use-module ((euphrates profun-accept) :select (profun-set))
    :use-module ((euphrates profun-error) :select (make-profun-error))
    :use-module ((euphrates profun-meta-key) :select (profun-meta-key))
    :use-module ((euphrates profun-op-lambda) :select (profun-op-lambda))
    :use-module ((euphrates profun-request-value) :select (profun-request-value))
    :use-module ((euphrates profun-value) :select (profun-bound-value? profun-unbound-value?))
    :use-module ((tegfs entry-target-fullpath) :select (entry-target-fullpath))
    :use-module ((tegfs permission) :select (permission?))
    :use-module ((tegfs sharedinfo) :select (sharedinfo-senderid sharedinfo-stime))
    :use-module ((tegfs web-share-file) :select (web::share-file/dont-link-yet))
    :use-module ((tegfs webcore-parameters) :select (webcore::permissions/p))
    )))



(define webcore::share-entry-generic
  (lambda (get-shared-path)
    (lambda (web::context)
      (profun-op-lambda
       :with-env env
       (ctx (entry max-sharing-time actual-sharing-time senderid)
            (E-name MT-name AT-name R-name))

       (define perm (webcore::permissions/p))

       (define (continue entry target-fullpath)
         (define generic-fullpath
           (and target-fullpath
                (get-shared-path target-fullpath)))
         (if generic-fullpath
             (let ()
               (define info
                 (web::share-file/dont-link-yet
                  web::context perm entry generic-fullpath max-sharing-time))
               (define vid (and info (sharedinfo-senderid info)))
               (define actual (if info (sharedinfo-stime info) 0))
               (if (profun-bound-value? actual-sharing-time)
                   (if (equal? actual actual-sharing-time)
                       (profun-set (R-name <- vid))
                       (make-profun-error 'cannot-share-for-that-long))
                   (profun-set
                    (AT-name <- actual)
                    (profun-set (R-name <- vid)))))
             (profun-set (R-name <- #f))))

       (define (try entry)
         (and (profun-bound-value? entry)
              (let ((target-fullpath (entry-target-fullpath entry)))
                (continue entry target-fullpath))))

       (cond
        ((profun-bound-value? senderid)
         (make-profun-error 'type-error "Senderid is the return value and must not be set" R-name))
        ((profun-unbound-value? entry)
         (profun-request-value E-name))
        ((profun-unbound-value? max-sharing-time)
         (profun-request-value MT-name))
        ((not (and (number? max-sharing-time)
                   (< 0 max-sharing-time)))
         (make-profun-error
          'type-error "Sharing type must be a number greater than 0"
          max-sharing-time))
        ((permission? perm)
         (or (try (env (profun-meta-key E-name)))
             ;; (try (env E-name)) ;; NOTE: uncommenting allows forging of entries
             (make-profun-error 'type-error "Cannot confirm that the entry passed is the actual entry from the database")))
        (else
         (make-profun-error 'could-not-authorize)))))))
