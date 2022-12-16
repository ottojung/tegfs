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

%var web-get-sharedinfo-url

%use (file-is-directory?/no-readlink) "./euphrates/file-is-directory-q-no-readlink.scm"
%use (sharedinfo-recepientid sharedinfo-senderid sharedinfo-sourcepath) "./sharedinfo.scm"
%use (web-context/p) "./web-context-p.scm"
%use (context-fileserver) "./web-context.scm"
%use (web-get-shared-link) "./web-get-shared-link.scm"

(define (web-get-sharedinfo-url ctx info)
  (define vid (sharedinfo-senderid info))
  (define target-fullpath (sharedinfo-sourcepath info))
  (define recepientid (sharedinfo-recepientid info))
  (define fileserver (context-fileserver ctx))
  (if (file-is-directory?/no-readlink target-fullpath)
      (string-append "/directory?vid=" vid)
      (web-get-shared-link fileserver target-fullpath recepientid)))