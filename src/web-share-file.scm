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

%var web-share-file
%var web-share-file/new
%var web-share-file/dont-link-yet

%use (hashmap-set!) "./euphrates/hashmap.scm"
%use (raisu) "./euphrates/raisu.scm"
%use (string->seconds) "./euphrates/string-to-seconds.scm"
%use (current-time/p) "./current-time-p.scm"
%use (filemap-set!) "./filemap.scm"
%use (get-sharedinfo-for-perm) "./get-sharedinfo-for-perm.scm"
%use (make-sharedinfo) "./make-sharedinfo.scm"
%use (permission-time-left) "./permission-time-left.scm"
%use (permission-filemap permission-share-longer-than-view?) "./permission.scm"
%use (sharedinfo-recepientid) "./sharedinfo.scm"
%use (symlink-shared-file) "./symlink-shared-file.scm"
%use (context-filemap/2) "./web-context.scm"

(define (web-share-file/new ctx perm target-fullpath for-duration make-symlink?)
  (define filemap/2 (context-filemap/2 ctx))
  (define now (or (current-time/p) (raisu 'current-time-is-not-set)))
  (define for-duration/parsed
    (cond
     ((number? for-duration) for-duration)
     ((string? for-duration) (string->seconds for-duration))
     (else (raisu 'type-error 'expected-number-or-string-for-for-duration for-duration))))
  (define for-duration*
    (if (permission-share-longer-than-view? perm)
        for-duration/parsed
        (min for-duration/parsed (permission-time-left perm now))))
  (define info (make-sharedinfo target-fullpath for-duration*))
  (define recepientid (sharedinfo-recepientid info))
  (define perm-filemap (permission-filemap perm))

  (and (< 0 for-duration*)
       (begin
         (hashmap-set! perm-filemap target-fullpath info)
         (filemap-set! filemap/2 info)

         (when make-symlink?
           (symlink-shared-file ctx target-fullpath recepientid))

         info)))

(define (web-share-file ctx perm target-fullpath for-duration)
  (define make-symlink? #t)
  (or
   (get-sharedinfo-for-perm ctx perm target-fullpath)
   (web-share-file/new ctx perm target-fullpath for-duration make-symlink?)))

(define (web-share-file/dont-link-yet ctx perm target-fullpath for-duration)
  (define make-symlink? #f)
  (or
   (get-sharedinfo-for-perm ctx perm target-fullpath)
   (web-share-file/new ctx perm target-fullpath for-duration make-symlink?)))
