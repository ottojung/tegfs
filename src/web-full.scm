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

%var web-full

%use (assq-or) "./euphrates/assq-or.scm"
%use (hashmap-ref) "./euphrates/hashmap.scm"
%use (filemap-ref-by-senderid) "./filemap.scm"
%use (keyword-entry-parent-directory-senderid) "./keyword-entry-parent-directory-senderid.scm"
%use (permission-filemap) "./permission.scm"
%use (sharedinfo-entry sharedinfo-recepientid sharedinfo-sourcepath) "./sharedinfo.scm"
%use (symlink-shared-file) "./symlink-shared-file.scm"
%use (web-basic-headers) "./web-basic-headers.scm"
%use (web-context/p) "./web-context-p.scm"
%use (context-filemap/2 context-fileserver) "./web-context.scm"
%use (web-get-permissions) "./web-get-permissions.scm"
%use (web-get-query) "./web-get-query.scm"
%use (web-get-sharedinfo-url) "./web-get-sharedinfo-url.scm"
%use (web-not-found) "./web-not-found.scm"
%use (web-return!) "./web-return-bang.scm"

%for (COMPILER "guile")

(use-modules (web response))

%end

(define (web-full)
  (define ctx (web-context/p))
  (define filemap/2 (context-filemap/2 ctx))
  (define fileserver (context-fileserver ctx))
  (define perm (web-get-permissions))
  (define _87123
    (unless perm
      ;; not logged in
      (web-not-found)))
  (define ctxq (web-get-query))
  (define vid (hashmap-ref ctxq 'vid #f))
  (define info (filemap-ref-by-senderid filemap/2 vid #f))
  (define _8123
    (unless info
      (web-not-found)))
  (define recepientid (sharedinfo-recepientid info))
  (define target-fullpath (sharedinfo-sourcepath info))
  (define _11
    (unless (hashmap-ref (permission-filemap perm) target-fullpath #f)
      ;; file was not shared with this permission
      (web-not-found)))

  (define adam-info
    (let loop ((info info))
      (define entry (sharedinfo-entry info))
      (define parent-vid
        (assq-or keyword-entry-parent-directory-senderid entry))
      (define next
        (and parent-vid (filemap-ref-by-senderid filemap/2 parent-vid #f)))
      (if next (loop next)
          info)))

  (define toplevel-entry?
    (eq? adam-info info))
  (define container-info
    (and (not toplevel-entry?) adam-info))

  (define location
    (web-get-sharedinfo-url ctx container-info info))

  (when toplevel-entry?
    (symlink-shared-file ctx target-fullpath recepientid))

  (web-return!
   (build-response
    #:code 301
    #:headers
    (append web-basic-headers
            `((Location . ,location)
              (Cache-Control . "no-cache"))))
   #f))
