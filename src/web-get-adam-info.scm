;;;; Copyright (C) 2022, 2023  Otto Jung
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

%var web-get-adam-info

%use (assq-or) "./euphrates/assq-or.scm"
%use (filemap-ref-by-senderid) "./filemap.scm"
%use (keyword-entry-parent-directory-senderid) "./keyword-entry-parent-directory-senderid.scm"
%use (sharedinfo-entry) "./sharedinfo.scm"

(define (web-get-adam-info filemap/2 info)
  (let loop ((info info))
    (define entry (sharedinfo-entry info))
    (define parent-vid
      (assq-or keyword-entry-parent-directory-senderid entry))
    (define next
      (and parent-vid (filemap-ref-by-senderid filemap/2 parent-vid #f)))
    (if next (loop next)
        info)))
