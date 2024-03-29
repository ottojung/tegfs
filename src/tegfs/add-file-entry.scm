;;;; Copyright (C) 2023  Otto Jung
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
  (define-module (tegfs add-file-entry)
    :export (add-file-entry add-file-entry/link)
    :use-module ((euphrates alphanum-lowercase-alphabet) :select (alphanum-lowercase/alphabet))
    :use-module ((euphrates append-posix-path) :select (append-posix-path))
    :use-module ((euphrates assq-or) :select (assq-or))
    :use-module ((euphrates assq-set-default) :select (assq-set-default))
    :use-module ((euphrates catch-any) :select (catch-any))
    :use-module ((euphrates file-or-directory-exists-q) :select (file-or-directory-exists?))
    :use-module ((euphrates make-directories) :select (make-directories))
    :use-module ((euphrates path-get-basename) :select (path-get-basename))
    :use-module ((euphrates path-get-dirname) :select (path-get-dirname))
    :use-module ((euphrates raisu) :select (raisu))
    :use-module ((euphrates random-choice) :select (random-choice))
    :use-module ((tegfs add-entry) :select (add-entry))
    :use-module ((tegfs default-db-path) :select (default-db-path))
    :use-module ((tegfs get-root) :select (get-root))
    :use-module ((tegfs keyword-target) :select (keyword-target))
    )))

(define (add-file-entry/link full-filepath/0 entry)
  (add-file-entry/generic #t full-filepath/0 entry))

(define (add-file-entry full-filepath/0 entry)
  (add-file-entry/generic #f full-filepath/0 entry))

(define (add-file-entry/generic --link full-filepath/0 entry)
  (define root (get-root))
  (define full-filepath
    (if (pair? full-filepath/0)
        (car full-filepath/0)
        full-filepath/0))
  (define filename
    (if (pair? full-filepath/0)
        (cdr full-filepath/0)
        (path-get-basename full-filepath)))

  (define (to-fullpath target)
    (append-posix-path root default-db-path target))

  (define (calculate-new-target full-filepath entry)
    (let restart ((prefix-len 2))
      (let loop ((tries 0))
        (if (< 10 tries)
            (restart (+ 1 prefix-len))
            (let ()
              (define prefix-dir
                (list->string
                 (random-choice prefix-len alphanum-lowercase/alphabet)))

              (define target (string-append prefix-dir "/" filename))
              (if (file-or-directory-exists? (to-fullpath target))
                  (loop (+ 1 tries))
                  target))))))

  (define (get-target full-filepath entry)
    (define existing (assq-or keyword-target entry #f))
    (if existing
        (if (file-or-directory-exists? (to-fullpath existing))
            (raisu 'file-exists "Such target already exists")
            existing)
        (calculate-new-target full-filepath entry)))

  (define target (get-target full-filepath entry))
  (define full-target (to-fullpath target))
  (define entry1
    (assq-set-default
     keyword-target target
     entry))

  (unless (file-or-directory-exists? full-filepath)
    (raisu 'cannot-move "Could not move the original file" full-filepath))

  (make-directories (path-get-dirname full-target))

  (catch-any
   (lambda _
     (rename-file full-filepath full-target))
   (lambda errs
     (define status
       (system* "mv" "--" full-filepath full-target))
     (unless (= 0 (status:exit-val status))
       (raisu 'cannot-move "Could not move the original file" full-filepath))))

  (when --link
    (catch-any
     (lambda _ (symlink full-filepath full-target))
     (lambda errs
       (raisu 'cannot-symlink "Could not create the symlink" full-filepath full-target))))

  (add-entry entry1))
