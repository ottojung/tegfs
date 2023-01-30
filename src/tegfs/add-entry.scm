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
  (define-module (tegfs add-entry)
    :export (add-entry)
    :use-module ((euphrates absolute-posix-path-q) :select (absolute-posix-path?))
    :use-module ((euphrates alphanum-lowercase-alphabet) :select (alphanum-lowercase/alphabet))
    :use-module ((euphrates append-posix-path) :select (append-posix-path))
    :use-module ((euphrates append-string-file) :select (append-string-file))
    :use-module ((euphrates assoc-set-default) :select (assoc-set-default))
    :use-module ((euphrates assoc-set-value) :select (assoc-set-value))
    :use-module ((euphrates assq-or) :select (assq-or))
    :use-module ((euphrates file-or-directory-exists-q) :select (file-or-directory-exists?))
    :use-module ((euphrates list-deduplicate) :select (list-deduplicate/reverse))
    :use-module ((euphrates make-directories) :select (make-directories))
    :use-module ((euphrates memconst) :select (memconst))
    :use-module ((euphrates path-get-dirname) :select (path-get-dirname))
    :use-module ((euphrates raisu) :select (raisu))
    :use-module ((euphrates random-choice) :select (random-choice))
    :use-module ((euphrates read-string-file) :select (read-string-file))
    :use-module ((euphrates string-strip) :select (string-strip))
    :use-module ((euphrates system-re) :select (system-re))
    :use-module ((euphrates tilda-a) :select (~a))
    :use-module ((euphrates write-string-file) :select (write-string-file))
    :use-module ((tegfs a-weblink-q) :select (a-weblink?))
    :use-module ((tegfs entry-get-target) :select (entry-get-target))
    :use-module ((tegfs entry-print) :select (entry-print))
    :use-module ((tegfs get-config) :select (get-config))
    :use-module ((tegfs get-file-mimetype) :select (get-file-mimetype))
    :use-module ((tegfs get-registry-files) :select (get-registry-files))
    :use-module ((tegfs get-root) :select (get-root))
    :use-module ((tegfs keyword-date) :select (keyword-date))
    :use-module ((tegfs keyword-id) :select (keyword-id))
    :use-module ((tegfs keyword-mimetype) :select (keyword-mimetype))
    :use-module ((tegfs keyword-prev) :select (keyword-prev))
    :use-module ((tegfs keyword-tags) :select (keyword-tags))
    :use-module ((tegfs last-id-filename) :select (last-id-filename))
    :use-module ((tegfs set-config) :select (set-config))
    :use-module ((tegfs warning) :select (warning)))))



(define (add-entry registry-file0 entry0)

  (define (generate-random-id)
    (list->string
     (random-choice 30 alphanum-lowercase/alphabet)))

  (define (get-date)
    (string-strip
     (car
      (system-re "date --utc '+%Y-%m-%dT%H:%M:%S+0000'"))))

  (define (tosymbol x)
    (cond
     ((symbol? x) x)
     (else (string->symbol (~a x)))))

  (define config (get-config))

  (define (init-registry-file <registry-file>)
    (define get-default-registry
      (memconst (assq-or 'default-save-registry config #f)))
    (define get-existing-registries
      (memconst (get-registry-files)))

    (define registry-file1
      (cond
       (<registry-file> <registry-file>)

       ((get-default-registry) (get-default-registry))

       ((not (null? (get-existing-registries)))
        (warning "Default registry not set. Choosing a first available one: ~s."
                 (car (get-existing-registries)))
        (car (get-existing-registries)))

       (else
        (warning "Default registry not set. A new one is created.")
        (let ((new "default/default.tegfs.reg.lisp"))
          (set-config 'registries new)
          new))))

    (define registry-file
      (append-posix-path (get-root) registry-file1))

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
   (with-output-to-string
     (lambda _
       (newline)
       (entry-print entry)
       (newline))))

  entry)
