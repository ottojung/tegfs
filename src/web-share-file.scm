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

%run guile

%var web::share-file/dont-link-yet

%use (hashmap-delete! hashmap-set!) "./euphrates/hashmap.scm"
%use (raisu) "./euphrates/raisu.scm"
%use (string->seconds) "./euphrates/string-to-seconds.scm"
%use (current-time/p) "./current-time-p.scm"
%use (filemap-delete-by-recepientid! filemap-set!) "./filemap.scm"
%use (get-sharedinfo-for-perm) "./get-sharedinfo-for-perm.scm"
%use (make-sharedinfo) "./make-sharedinfo.scm"
%use (permission-time-left) "./permission-time-left.scm"
%use (permission-filemap) "./permission.scm"
%use (sharedinfo-recepientid sharedinfo-stime) "./sharedinfo.scm"
%use (context-filemap/2) "./web-context.scm"
%use (can-share-longer-than-view?) "./webcore-access.scm"

(define (delete-existing! ctx perm target-fullpath existing)
  (define filemap/2 (context-filemap/2 ctx))
  (define recepientid (sharedinfo-recepientid existing))
  (define perm-filemap (permission-filemap perm))
  (filemap-delete-by-recepientid! filemap/2 recepientid)
  (hashmap-delete! perm-filemap target-fullpath))

(define (web::share-file/new ctx perm entry target-fullpath for-duration)
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
       (if (equal? extime for-duration*)
           existing
           (let ((info (make-sharedinfo entry target-fullpath for-duration*)))
             (when existing
               (delete-existing! ctx perm target-fullpath existing))
             (hashmap-set! perm-filemap target-fullpath info)
             (filemap-set! filemap/2 info)
             info))))

(define (web::share-file/dont-link-yet ctx perm entry target-fullpath for-duration)
  (web::share-file/new ctx perm entry target-fullpath for-duration))
