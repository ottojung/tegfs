;;;; Copyright (C) 2023  Otto Jung
;;;; This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU Affero General Public License for more details. You should have received a copy of the GNU Affero General Public License along with this program.  If not, see <https://www.gnu.org/licenses/>.

(define (add-entry entry0)

  (define (generate-random-id)
    (list->string
     (random-choice 30 alphanum-lowercase/alphabet)))

  (define (get-date)
    (date-get-current-string "~Y-~m-~dT~H:~M:~S+0000"))

  (define (tosymbol x)
    (cond
     ((symbol? x) x)
     (else (string->symbol (~a x)))))

  (define root (get-root))

  (define registry-dir
    (append-posix-path root default-db-path))

  (define (init-registry-file)
    (define registry-file
      (append-posix-path root (car (get-registry-files))))

    (unless (file-or-directory-exists? registry-file)
      (make-directories registry-dir)
      (write-string-file
       registry-file
       "\n;; This file was automatically created by tegfs-add\n\n\n"))

    registry-file)

  (define last-id-path
    (append-posix-path root last-id-filename))

  (define registry-file
    (init-registry-file))

  (define existing-id
    (entry-get-id entry0))

  (define added-check
    (when (tegfs-get existing-id)
      (raisu 'entry-with-such-id-already-exists existing-id)))

  (define id
    (or existing-id
        (generate-random-id)))

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
           keyword-tags
           (list-deduplicate/reverse
            (map tosymbol tags0))
           entry1)
          entry1)))

  (define entry3
    (let ((prev0 (assq-or keyword-prev entry2 #f)))
      (if (equal? prev0 '%LAST-ID)
          (let ((prev
                 (and (or (file-or-directory-exists? last-id-path)
                          (raisu 'no-last-id-for-series))
                      (string-strip (read-string-file last-id-path)))))
            (assoc-set-value
             keyword-prev prev
             entry2))
          entry2)))

  (define target
    (entry-get-target entry3))

  (define target-full
    (cond
     ((not target) #f)
     ((absolute-posix-path? target)
      (raisu 'target-absolute-but-should-relative target))
     ((a-weblink? target) target)
     (else
      (append-posix-path registry-dir target))))

  (define _717231
    (when target
      (unless (or (file-or-directory-exists? target-full)
                  (a-weblink? target))
        (raisu 'target-does-not-exist target))))

  (define entry
    (if (assq keyword-mimetype entry3)
        entry3
        (let ((mimetype
               (and target-full
                    (get-file-mimetype target-full))))
          (if mimetype
              (assoc-set-value
               keyword-mimetype mimetype
               entry3)
              entry3))))

  (write-string-file last-id-path id)

  (append-string-file
   registry-file
   (with-output-stringified
    (newline)
    (entry-print entry)
    (newline)))

  entry)
