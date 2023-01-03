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

%run guile

%var add-entry

%use (absolute-posix-path?) "./euphrates/absolute-posix-path-q.scm"
%use (alphanum-lowercase/alphabet) "./euphrates/alphanum-lowercase-alphabet.scm"
%use (append-posix-path) "./euphrates/append-posix-path.scm"
%use (append-string-file) "./euphrates/append-string-file.scm"
%use (assoc-set-default) "./euphrates/assoc-set-default.scm"
%use (assoc-set-value) "./euphrates/assoc-set-value.scm"
%use (assq-or) "./euphrates/assq-or.scm"
%use (file-or-directory-exists?) "./euphrates/file-or-directory-exists-q.scm"
%use (list-deduplicate/reverse) "./euphrates/list-deduplicate.scm"
%use (make-directories) "./euphrates/make-directories.scm"
%use (path-get-dirname) "./euphrates/path-get-dirname.scm"
%use (raisu) "./euphrates/raisu.scm"
%use (random-choice) "./euphrates/random-choice.scm"
%use (read-string-file) "./euphrates/read-string-file.scm"
%use (string-strip) "./euphrates/string-strip.scm"
%use (system-re) "./euphrates/system-re.scm"
%use (write-string-file) "./euphrates/write-string-file.scm"
%use (a-weblink?) "./a-weblink-q.scm"
%use (entry-print) "./entry-print.scm"
%use (get-root) "./get-root.scm"
%use (keyword-date) "./keyword-date.scm"
%use (keyword-id) "./keyword-id.scm"
%use (keyword-tags) "./keyword-tags.scm"
%use (keyword-target) "./keyword-target.scm"
%use (last-id-filename) "./last-id-filename.scm"

(define (add-entry registry-file0 entry0)

  (define (generate-random-id)
    (list->string
     (random-choice 30 alphanum-lowercase/alphabet)))

  (define (get-date)
    (string-strip
     (car
      (system-re "date --utc '+%Y-%m-%dT%H:%M:%S+0000'"))))

  (define (init-registry-file <registry-file>)
    (define registry-file (append-posix-path (get-root) <registry-file>))

    (unless (file-or-directory-exists? registry-file)
      (let ((registry-dir (dirname registry-file)))
        (make-directories registry-dir))
      (write-string-file
       registry-file
       "\n;; This file was automatically created by tegfs-add\n\n\n"))

    registry-file)

  (define last-id-path
    (append-posix-path (get-root) last-id-filename))

  (define registry-file
    (init-registry-file registry-file0))

  (define registry-dir
    (path-get-dirname registry-file))

  (define id
    (generate-random-id))

  (define entry1
    (assoc-set-default
     keyword-date (get-date)
     (assoc-set-value
      keyword-id id
      entry0)))

  (define entry2
    (let ((tags0 (assq-or keyword-tags entry1 #f)))
      (if tags0
         (assoc-set-value
          keyword-tags (list-deduplicate/reverse tags0)
          entry1)
         entry1)))

  (define entry
    (let ((prev0 (assq-or keyword-tags entry2 #f)))
      (if (equal? prev0 '%LAST-ID)
          (and (or (file-or-directory-exists? last-id-path)
                   (raisu 'no-last-id-for-series))
               (string-strip (read-string-file last-id-path)))
          entry2)))

  (define target
    (assq-or keyword-target entry #f))

  (when target
    (when (absolute-posix-path? target)
      (raisu 'target-absolute-but-should-relative target))
    (let ((target-full (append-posix-path registry-dir target)))
      (unless (or (file-or-directory-exists? target-full)
                  (a-weblink? target))
        (raisu 'target-does-not-exist target))))

  (write-string-file last-id-path id)

  (append-string-file
   registry-file
   (with-output-to-string
     (lambda _
       (newline)
       (entry-print entry)
       (newline)))))
