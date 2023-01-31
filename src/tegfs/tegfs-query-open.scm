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

(cond-expand
 (guile
  (define-module (tegfs tegfs-query-open)
    :export (tegfs-query/open)
    :use-module ((euphrates assq-or) :select (assq-or))
    :use-module ((euphrates directory-files-depth-iter) :select (directory-files-depth-iter))
    :use-module ((euphrates file-is-directory-q-no-readlink) :select (file-is-directory?/no-readlink))
    :use-module ((euphrates list-singleton-q) :select (list-singleton?))
    :use-module ((euphrates raisu) :select (raisu))
    :use-module ((tegfs entry-target-fullpath) :select (entry-target-fullpath))
    :use-module ((tegfs keyword-tags) :select (keyword-tags))
    :use-module ((tegfs standalone-file-to-entry) :select (standalone-file->entry))
    :use-module ((tegfs tegfs-query-noopen) :select (tegfs-query/noopen))
    )))



(define (tegfs-query/open opening-properties <query...>)
  (define iter #f)
  (define iter/open #f)

  (define (handle entry)
    (define opener
      (let loop ((buf opening-properties))
        (if (null? buf) #f
            (or (assq-or (car buf) entry #f)
                (loop (cdr buf))))))

    (if opener
        (let ((target-fullpath (entry-target-fullpath entry)))
          (if (and target-fullpath
                   (file-is-directory?/no-readlink target-fullpath))
              (begin
                (set! iter/open
                      (query-recurse opener entry target-fullpath))
                (next))
              entry))
        entry))

  (define (next)
    (unless iter
      (set! iter (tegfs-query/noopen <query...>)))

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
             (cons (cons keyword-tags (assq-or keyword-tags entry '()))
                   (standalone-file->entry path)))
           subentry)))

  next)
