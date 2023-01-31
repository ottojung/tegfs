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
  (define-module (tegfs webcore-link-shared)
    :export (webcore::link-shared)
    :use-module ((euphrates profun-accept) :select (profun-set))
    :use-module ((euphrates profun-error) :select (make-profun-error))
    :use-module ((euphrates profun-op-lambda) :select (profun-op-lambda))
    :use-module ((euphrates profun-request-value) :select (profun-request-value))
    :use-module ((euphrates profun-value) :select (profun-unbound-value?))
    :use-module ((tegfs filemap) :select (filemap-ref-by-senderid))
    :use-module ((tegfs get-config) :select (get-config))
    :use-module ((tegfs get-root) :select (get-root))
    :use-module ((tegfs sharedinfo) :select (sharedinfo-entry sharedinfo-recepientid sharedinfo-sourcepath))
    :use-module ((tegfs symlink-shared-file) :select (symlink-shared-file))
    :use-module ((tegfs web-context) :select (context-filemap/2))
    :use-module ((tegfs web-get-adam-info) :select (web::get-adam-info))
    :use-module ((tegfs web-get-sharedinfo-url) :select (web::get-sharedinfo-url))
    :use-module ((tegfs webcore-get-share-plugins) :select (webcore::get-share-plugins))
    :use-module ((tegfs webcore-run-share-plugins) :select (webcore::run-share-plugins))
    )))



(define webcore::link-shared
  (lambda (web::context)
    (define plugins (webcore::get-share-plugins))
    (define config (get-config))
    (define root (get-root))
    (profun-op-lambda
     (ctx (R L) (R-name L-name))

     (define senderid R)

     (define filemap/2 (context-filemap/2 web::context))
     (define info (filemap-ref-by-senderid filemap/2 senderid #f))
     (define target-fullpath
       (and info (sharedinfo-sourcepath info)))
     (define recepientid
       (and info (sharedinfo-recepientid info)))

     (define adam-info
       (and info (web::get-adam-info filemap/2 info)))

     (define toplevel-entry?
       (eq? adam-info info))
     (define container-info
       (and (not toplevel-entry?) adam-info))

     (cond
      ((profun-unbound-value? R)
       (profun-request-value R-name))
      ((not senderid)
       (profun-set (L-name <- #f)))
      ((not (string? senderid))
       (make-profun-error 'type-error 'expected-string-for-senderid senderid))
      ((not info)
       (make-profun-error 'not-found "Bad senderid" senderid))
      (else
       (if target-fullpath
           (let ()
             (define location (web::get-sharedinfo-url web::context container-info info))
             (define entry (sharedinfo-entry info))
             (define target-fullpath* (webcore::run-share-plugins config root plugins entry target-fullpath))
             (symlink-shared-file
              web::context target-fullpath* recepientid)
             (profun-set (L-name <- location)))
           (profun-set (L-name <- #f))))))))
