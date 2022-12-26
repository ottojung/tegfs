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
%use (debugs) "./euphrates/debugs.scm"
%use (file-is-directory?/no-readlink) "./euphrates/file-is-directory-q-no-readlink.scm"
%use (raisu) "./euphrates/raisu.scm"
%use (stringf) "./euphrates/stringf.scm"
%use (uri-encode) "./euphrates/uri-encode.scm"
%use (a-weblink?) "./a-weblink-q.scm"
%use (entry-for-local-file?) "./entry-for-local-file-huh.scm"
%use (entry-target-fullpath) "./entry-target-fullpath.scm"
%use (filemap-ref-by-senderid) "./filemap.scm"
%use (keyword-entry-parent-directory-senderid) "./keyword-entry-parent-directory-senderid.scm"
%use (sharedinfo-recepientid) "./sharedinfo.scm"
%use (web-context/p) "./web-context-p.scm"
%use (context-filemap/2 context-fileserver) "./web-context.scm"

(define (web-get-full-link entry target maybe-senderid)
  (cond
   (maybe-senderid (string-append "/full?vid=" maybe-senderid))
   ((a-weblink? target) target)
   ((entry-for-local-file? entry)
    (let* ((target-fullpath (or (entry-target-fullpath entry)
                                (raisu 'local-file-entry-does-not-have-target-info entry)))
           (parent-senderid (or (assoc-or keyword-entry-parent-directory-senderid entry #f)
                                (raisu 'entry-does-not-have-parent-senderid entry)))
           (ctx (web-context/p))
           (filemap/2 (context-filemap/2 ctx))
           (info (or (filemap-ref-by-senderid filemap/2 parent-senderid #f)
                     (raisu 'entry-has-bad-parent-senderid entry)))
           (suffix/raw
            (or target (raisu 'entry-does-not-have-target entry)))
           (suffix (uri-encode suffix/raw)))
      (if (file-is-directory?/no-readlink target-fullpath)
          (stringf "/directory?vid=~a&s=~a" parent-senderid suffix)
          (let* ((fileserver (context-fileserver ctx))
                 (recepientid (sharedinfo-recepientid info)))
            (append-posix-path fileserver recepientid suffix)))))
   (else
    (raisu 'not-enough-information-to-get-full-link entry))))
