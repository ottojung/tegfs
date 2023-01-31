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

(cond-expand
 (guile
  (define-module (tegfs web-share-file)
    :export (web::share-file/dont-link-yet)
    :use-module ((euphrates hashmap) :select (hashmap-set!))
    :use-module ((euphrates raisu) :select (raisu))
    :use-module ((euphrates string-to-seconds) :select (string->seconds))
    :use-module ((tegfs current-time-p) :select (current-time/p))
    :use-module ((tegfs filemap) :select (filemap-set!))
    :use-module ((tegfs get-sharedinfo-for-perm) :select (get-sharedinfo-for-perm))
    :use-module ((tegfs make-sharedinfo) :select (make-sharedinfo))
    :use-module ((tegfs permission-time-left) :select (permission-time-left))
    :use-module ((tegfs permission) :select (permission-filemap))
    :use-module ((tegfs sharedinfo) :select (set-sharedinfo-stime! sharedinfo-stime))
    :use-module ((tegfs web-context) :select (context-filemap/2))
    :use-module ((tegfs webcore-access) :select (can-share-longer-than-view?))
    )))



(define (web::share-file/dont-link-yet ctx perm entry target-fullpath for-duration)
  (define filemap/2 (context-filemap/2 ctx))
  (define now (or (current-time/p) (raisu 'current-time-is-not-set)))
  (define for-duration/parsed
    (cond
     ((number? for-duration) for-duration)
     ((string? for-duration) (string->seconds for-duration))
     (else (raisu 'type-error 'expected-number-or-string-for-for-duration for-duration))))
  (define for-duration*
    (if (can-share-longer-than-view? perm)
        for-duration/parsed
        (min for-duration/parsed (permission-time-left perm now))))
  (define perm-filemap (permission-filemap perm))
  (define existing (get-sharedinfo-for-perm ctx perm target-fullpath))
  (define extime (and existing (sharedinfo-stime existing)))

  (and (< 0 for-duration*)
       (cond
        ((equal? extime for-duration*) existing)
        (existing
         (set-sharedinfo-stime! existing for-duration*)
         existing)
        (else
         (let ((info (make-sharedinfo entry target-fullpath for-duration*)))
           (hashmap-set! perm-filemap target-fullpath info)
           (filemap-set! filemap/2 info)
           info)))))
