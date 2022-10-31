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
%use (filemap-ref-by-vid) "./filemap.scm"
%use (keyword-entry-parent-directory-vid) "./keyword-entry-parent-directory-vid.scm"
%use (keyword-target) "./keyword-target.scm"
%use (sharedinfo-sharedname sharedinfo-vid) "./sharedinfo.scm"
%use (web-context/p) "./web-context-p.scm"
%use (context-filemap/2 context-fileserver) "./web-context.scm"
%use (web-share-file/dont-link-yet) "./web-share-file.scm"

(define (web-get-full-link entry target-fullpath)
  (cond
   ((a-weblink? target-fullpath)
    target-fullpath)
   ((entry-for-local-file? entry)
    (let* ((parent-vid (or (assoc-or keyword-entry-parent-directory-vid entry #f)
                           (raisu 'entry-does-not-have-parent-vid entry)))
           (ctx (web-context/p))
           (filemap/2 (context-filemap/2 ctx))
           (info (or (filemap-ref-by-vid filemap/2 parent-vid #f)
                     (raisu 'entry-has-bad-parent-vid entry)))
           (suffix/raw
            (or (assoc-or keyword-target entry #f)
                (raisu 'entry-does-not-have-target entry)))
           (suffix (uri-encode suffix/raw)))
      (if (file-is-directory?/no-readlink target-fullpath)
          (stringf "/directory?vid=~a&s=~a" parent-vid suffix)
          (let* ((fileserver (context-fileserver ctx))
                 (sharedname (sharedinfo-sharedname info)))
            (append-posix-path fileserver sharedname suffix)))))
   (else
    (let* ((info (web-share-file/dont-link-yet target-fullpath default-full-sharing-time))
           (vid (and info (sharedinfo-vid info)))
           (location (and info (string-append "/full?vid=" vid))))
      (and info location)))))
