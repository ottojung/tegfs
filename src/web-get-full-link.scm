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

%var web-get-full-link

%use (append-posix-path) "./euphrates/append-posix-path.scm"
%use (assoc-or) "./euphrates/assoc-or.scm"
%use (file-is-directory?/no-readlink) "./euphrates/file-is-directory-q-no-readlink.scm"
%use (raisu) "./euphrates/raisu.scm"
%use (stringf) "./euphrates/stringf.scm"
%use (uri-encode) "./euphrates/uri-encode.scm"
%use (a-weblink?) "./a-weblink-q.scm"
%use (default-full-sharing-time) "./default-full-sharing-time.scm"
%use (entry-for-local-file?) "./entry-for-local-file-huh.scm"
%use (filemap-ref-by-senderid) "./filemap.scm"
%use (keyword-entry-parent-directory-senderid) "./keyword-entry-parent-directory-senderid.scm"
%use (keyword-target) "./keyword-target.scm"
%use (sharedinfo-recepientid sharedinfo-senderid) "./sharedinfo.scm"
%use (web-context/p) "./web-context-p.scm"
%use (context-filemap/2 context-fileserver) "./web-context.scm"
%use (web-get-permissions) "./web-get-permissions.scm"
%use (web-share-file/dont-link-yet) "./web-share-file.scm"

(define (web-get-full-link entry target-fullpath)
  (cond
   ((a-weblink? target-fullpath)
    target-fullpath)
   ((entry-for-local-file? entry)
    (let* ((parent-senderid (or (assoc-or keyword-entry-parent-directory-senderid entry #f)
                           (raisu 'entry-does-not-have-parent-senderid entry)))
           (ctx (web-context/p))
           (filemap/2 (context-filemap/2 ctx))
           (info (or (filemap-ref-by-senderid filemap/2 parent-senderid #f)
                     (raisu 'entry-has-bad-parent-senderid entry)))
           (suffix/raw
            (or (assoc-or keyword-target entry #f)
                (raisu 'entry-does-not-have-target entry)))
           (suffix (uri-encode suffix/raw)))
      (if (file-is-directory?/no-readlink target-fullpath)
          (stringf "/directory?vid=~a&s=~a" parent-senderid suffix)
          (let* ((fileserver (context-fileserver ctx))
                 (recepientid (sharedinfo-recepientid info)))
            (append-posix-path fileserver recepientid suffix)))))
   (else
    (let* ((perm (web-get-permissions))
           (info (web-share-file/dont-link-yet perm target-fullpath default-full-sharing-time))
           (vid (and info (sharedinfo-senderid info)))
           (location (and info (string-append "/full?vid=" vid))))
      (and info location)))))
