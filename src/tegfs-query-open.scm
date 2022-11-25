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

%var tegfs-query/open

%use (assoc-or) "./euphrates/assoc-or.scm"
%use (assq-or) "./euphrates/assq-or.scm"
%use (directory-files-depth-iter) "./euphrates/directory-files-depth-iter.scm"
%use (file-is-directory?/no-readlink) "./euphrates/file-is-directory-q-no-readlink.scm"
%use (list-singleton?) "./euphrates/list-singleton-q.scm"
%use (raisu) "./euphrates/raisu.scm"
%use (entry-target-fullpath) "./entry-target-fullpath.scm"
%use (standalone-file->entry) "./standalone-file-to-entry.scm"
%use (tegfs-query/noopen) "./tegfs-query-noopen.scm"

(define (tegfs-query/open opening-properties <query...> for-each-fn)
  (define iter (tegfs-query/open/iter opening-properties <query...>))
  (let loop ()
    (define entry (iter))
    (when entry
      (for-each-fn entry)
      (loop))))

(define (tegfs-query/open/iter opening-properties <query...>)
  (define iter (tegfs-query/noopen <query...>))
  (define iter/open #f)

  (define (handle entry)
    (define opener
      (let loop ((buf opening-properties))
        (if (null? buf) #f
            (or (assoc-or (car buf) entry #f)
                (loop (cdr buf))))))

    (when opener
      (let ((target-fullpath (entry-target-fullpath entry)))
        (when (file-is-directory?/no-readlink target-fullpath)
          (set! iter/open
                (query-recurse opener entry target-fullpath)))))

    entry)

  (define (next)
    (if iter/open
        (let ((entry (iter/open)))
          (or entry
              (begin
                (set! iter/open #f)
                (next))))
        (let ((entry (iter)))
          (and entry (handle entry)))))

  next)

(define (query-recurse opener entry target-fullpath)
  (define depth
    (cond
     ((integer? opener) opener)
     ((null? opener) 1)
     ((eq? #f opener) 1)
     ((and (pair? opener) (list-singleton? opener) (integer? (car opener)))
      (car opener))
     (else (raisu 'bad-opener entry opener))))

  (define iter
    (directory-files-depth-iter depth target-fullpath))

  (define (next)
    (define p (iter))
    (and p
         (let ()
           (define path (car p))
           (define subentry
             (cons (cons 'tags (assq-or 'tags entry '()))
                   (standalone-file->entry path)))
           subentry)))

  next)
