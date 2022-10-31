;;;; Copyright (C) 2022  Otto Jung
;;;;
;;;; This program is free software: you can redistribute it and/or modify
;;;; it under the terms of the GNU General Public License as published by
;;;; the Free Software Foundation; version 3 of the License.
;;;;
;;;; This program is distributed in the hope that it will be useful,
;;;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;;; GNU General Public License for more details.
;;;;
;;;; You should have received a copy of the GNU General Public License
;;;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

%run guile

%var web-share-file
%var web-share-file/new
%var web-share-file/dont-link-yet

%use (hashmap-set!) "./euphrates/ihashmap.scm"
%use (filemap-set!) "./filemap.scm"
%use (get-sharedinfo-for-perm) "./get-sharedinfo-for-perm.scm"
%use (make-sharedinfo) "./make-sharedinfo.scm"
%use (permission-time-left) "./permission-time-left.scm"
%use (permission-filemap permission-share-longer-than-view? permission-token) "./permission.scm"
%use (sharedinfo-sharedname) "./sharedinfo.scm"
%use (symlink-shared-file) "./symlink-shared-file.scm"
%use (web-callcontext/p) "./web-callcontext-p.scm"
%use (callcontext-time) "./web-callcontext.scm"
%use (web-context/p) "./web-context-p.scm"
%use (context-filemap/2) "./web-context.scm"
%use (web-get-permissions) "./web-get-permissions.scm"

(define (web-share-file/new target-fullpath for-duration make-symlink?)
  (define ctx (web-context/p))
  (define callctx (web-callcontext/p))
  (define filemap/2 (context-filemap/2 ctx))
  (define perm (web-get-permissions))
  (define token (permission-token perm))
  (define now (callcontext-time callctx))
  (define for-duration*
    (if (permission-share-longer-than-view? perm)
        for-duration
        (min for-duration (permission-time-left perm now))))
  (define info (make-sharedinfo token target-fullpath for-duration*))
  (define sharedname (sharedinfo-sharedname info))
  (define perm-filemap (permission-filemap perm))

  (and (< 0 for-duration*)
       (begin
         (hashmap-set! perm-filemap target-fullpath info)
         (filemap-set! filemap/2 info)

         (when make-symlink?
           (symlink-shared-file target-fullpath sharedname))

         info)))

(define (web-share-file target-fullpath for-duration)
  (define ctx (web-context/p))
  (define perm (web-get-permissions))
  (define make-symlink? #t)
  (or
   (get-sharedinfo-for-perm perm target-fullpath)
   (web-share-file/new target-fullpath for-duration make-symlink?)))

(define (web-share-file/dont-link-yet target-fullpath for-duration)
  (define ctx (web-context/p))
  (define perm (web-get-permissions))
  (define make-symlink? #f)
  (or
   (get-sharedinfo-for-perm perm target-fullpath)
   (web-share-file/new target-fullpath for-duration make-symlink?)))
