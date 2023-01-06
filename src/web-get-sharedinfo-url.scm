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

%var web::get-sharedinfo-url

%use (append-posix-path) "./euphrates/append-posix-path.scm"
%use (file-is-directory?/no-readlink) "./euphrates/file-is-directory-q-no-readlink.scm"
%use (list-intersperse) "./euphrates/list-intersperse.scm"
%use (path-normalize) "./euphrates/path-normalize.scm"
%use (remove-common-prefix) "./euphrates/remove-common-prefix.scm"
%use (string-split/simple) "./euphrates/string-split-simple.scm"
%use (uri-encode) "./euphrates/uri-encode.scm"
%use (a-weblink?) "./a-weblink-q.scm"
%use (sharedinfo-recepientid sharedinfo-senderid sharedinfo-sourcepath) "./sharedinfo.scm"
%use (context-fileserver) "./web-context.scm"
%use (web::get-shared-link) "./web-get-shared-link.scm"

(define (web::get-sharedinfo-url ctx container-info info)
  (define vid (sharedinfo-senderid info))
  (define target-fullpath (sharedinfo-sourcepath info))
  (define recepientid (sharedinfo-recepientid info))
  (define fileserver (context-fileserver ctx))

  (define (share-containerized container-path)
    (define suffix
      (remove-common-prefix target-fullpath container-path))
    (define escaped-suffix
      (apply
       string-append
       (list-intersperse
        "/"
        (map uri-encode
             (string-split/simple suffix #\/)))))
    (define relative-path
      (path-normalize
       (string-append
        (sharedinfo-recepientid container-info)
        "/" escaped-suffix)))

    (append-posix-path fileserver relative-path))

  (define (share-toplevel)
    (web::get-shared-link fileserver target-fullpath recepientid))

  (cond
   ((a-weblink? target-fullpath)
    target-fullpath)
   ((file-is-directory?/no-readlink target-fullpath)
    (string-append "/directory?vid=" vid))
   (container-info
    (let ((container-path (sharedinfo-sourcepath container-info)))
      (if (string-prefix? container-path target-fullpath)
          (share-containerized container-path)
          (share-toplevel))))
   (else
    (share-toplevel))))
