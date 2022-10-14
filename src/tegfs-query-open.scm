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

%var tegfs-query/open

%use (assoc-or) "./euphrates/assoc-or.scm"
%use (directory-files-depth-foreach) "./euphrates/directory-files-depth-foreach.scm"
%use (file-is-directory?/no-readlink) "./euphrates/file-is-directory-q-no-readlink.scm"
%use (list-or-map) "./euphrates/list-or-map.scm"
%use (raisu) "./euphrates/raisu.scm"
%use (entry-target-fullpath) "./entry-target-fullpath.scm"
%use (standalone-file->entry) "./standalone-file-to-entry.scm"
%use (tegfs-query) "./tegfs-query.scm"

(define (tegfs-query/open opening-properties <query...> for-each-fn)
  (define (wrapper entry)
    (define opener
      (list-or-map
       (lambda (prop) (assoc-or prop entry #f))
       opening-properties))

    (if opener
        (query-recurse opener entry for-each-fn)
        (for-each-fn entry)))

  (tegfs-query <query...> wrapper))

(define (query-recurse opener entry for-each-fn)
  (define target-fullpath (entry-target-fullpath entry))
  (define depth
    (cond
     ((integer? opener) opener)
     ((null? opener) 1)
     ((eq? #f opener) 1)
     (else (raisu 'bad-opener entry opener))))

  (when (file-is-directory?/no-readlink target-fullpath)
    (directory-files-depth-foreach
     depth
     (lambda (p)
       (define path (car p))
       (define subentry
         (standalone-file->entry path (assoc-or 'tags entry #f)))
       (for-each-fn subentry))
     target-fullpath)))
