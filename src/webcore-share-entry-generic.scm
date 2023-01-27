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

%var webcore::share-entry-generic

%use (profun-set) "./euphrates/profun-accept.scm"
%use (make-profun-error) "./euphrates/profun-error.scm"
%use (profun-meta-key) "./euphrates/profun-meta-key.scm"
%use (profun-op-lambda) "./euphrates/profun-op-lambda.scm"
%use (profun-request-value) "./euphrates/profun-request-value.scm"
%use (profun-bound-value? profun-unbound-value?) "./euphrates/profun-value.scm"
%use (entry-target-fullpath) "./entry-target-fullpath.scm"
%use (permission?) "./permission.scm"
%use (sharedinfo-senderid sharedinfo-stime) "./sharedinfo.scm"
%use (web::share-file/dont-link-yet) "./web-share-file.scm"
%use (webcore::get-share-plugins) "./webcore-get-share-plugins.scm"
%use (webcore::permissions/p) "./webcore-parameters.scm"
%use (webcore::run-share-plugins) "./webcore-run-share-plugins.scm"

(define webcore::share-entry-generic
  (lambda (get-shared-path)
    (lambda (web::context)
      (define plugins (webcore::get-share-plugins))
      (profun-op-lambda
       :with-env env
       (ctx (entry max-sharing-time actual-sharing-time senderid)
            (E-name MT-name AT-name R-name))

       (define perm (webcore::permissions/p))

       (define (continue entry target-fullpath)
         (define generic-fullpath/0
           (and target-fullpath
                (get-shared-path target-fullpath)))
         (define generic-fullpath
           (and generic-fullpath/0
                (webcore::run-share-plugins plugins entry generic-fullpath/0)))
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
