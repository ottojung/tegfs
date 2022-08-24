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

%var tegfs-list
%var tegfs-list/parse

%use (directory-files-depth-foreach) "./euphrates/directory-files-depth-foreach.scm"
%use (file-is-directory?/no-readlink) "./euphrates/file-is-directory-q-no-readlink.scm"
%use (fn) "./euphrates/fn.scm"
%use (entries-for-each) "./entries-for-each.scm"
%use (entry-print/formatted) "./entry-print-formatted.scm"
%use (entry-print) "./entry-print.scm"
%use (entry-target-fullpath) "./entry-target-fullpath.scm"
%use (standalone-file->entry) "./standalone-file-to-entry.scm"

(define (tegfs-list/parse <listdepth> --entries <list-format>)
  (tegfs-list
   <listdepth>
   (if --entries
       (lambda (e) (entry-print e) (display "\n\n"))
       (lambda (e) (entry-print/formatted <list-format> e) (display "\n")))))

(define (tegfs-list <listdepth> fn)
  (if <listdepth>
      (entries-for-each
       (lambda (entry)
         (define target-fullpath (entry-target-fullpath entry))
         (fn entry)
         (when (file-is-directory?/no-readlink target-fullpath)
           (directory-files-depth-foreach
            <listdepth>
            (lambda (p)
              (define path (car p))
              (fn (standalone-file->entry path)))
            target-fullpath))))
      (entries-for-each fn)))
