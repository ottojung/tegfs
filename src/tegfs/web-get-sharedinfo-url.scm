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
  (define-module (tegfs web-get-sharedinfo-url)
    :export (web::get-sharedinfo-url)
    :use-module ((euphrates append-posix-path) :select (append-posix-path))
    :use-module ((euphrates file-is-directory-q-no-readlink) :select (file-is-directory?/no-readlink))
    :use-module ((euphrates list-intersperse) :select (list-intersperse))
    :use-module ((euphrates path-normalize) :select (path-normalize))
    :use-module ((euphrates remove-common-prefix) :select (remove-common-prefix))
    :use-module ((euphrates string-split-simple) :select (string-split/simple))
    :use-module ((euphrates uri-encode) :select (uri-encode))
    :use-module ((tegfs a-weblink-q) :select (a-weblink?))
    :use-module ((tegfs sharedinfo) :select (sharedinfo-recepientid sharedinfo-senderid sharedinfo-sourcepath))
    :use-module ((tegfs webcore-context) :select (context-fileserver))
    :use-module ((tegfs web-get-shared-link) :select (web::get-shared-link))
    )))



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
