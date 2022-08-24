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

%var tegfs-add
%var tegfs-add/parse

%use (absolute-posix-path?) "./euphrates/absolute-posix-path-q.scm"
%use (alphanum-lowercase/alphabet) "./euphrates/alphanum-lowercase-alphabet.scm"
%use (append-posix-path) "./euphrates/append-posix-path.scm"
%use (append-string-file) "./euphrates/append-string-file.scm"
%use (file-or-directory-exists?) "./euphrates/file-or-directory-exists-q.scm"
%use (fn-cons) "./euphrates/fn-cons.scm"
%use (list-deduplicate) "./euphrates/list-deduplicate.scm"
%use (list-zip) "./euphrates/list-zip.scm"
%use (make-directories) "./euphrates/make-directories.scm"
%use (random-choice) "./euphrates/random-choice.scm"
%use (read-string-file) "./euphrates/read-string-file.scm"
%use (string-strip) "./euphrates/string-strip.scm"
%use (system-re) "./euphrates/system-re.scm"
%use (~a) "./euphrates/tilda-a.scm"
%use (write-string-file) "./euphrates/write-string-file.scm"
%use (a-weblink?) "./a-weblink-q.scm"
%use (entry-print) "./entry-print.scm"
%use (fatal) "./fatal.scm"
%use (get-root) "./get-root.scm"
%use (id-name) "./id-name.scm"
%use (last-id-filename) "./last-id-filename.scm"

(define (generate-random-id)
  (list->string
   (random-choice 30 alphanum-lowercase/alphabet)))

(define (get-date)
  (string-strip
   (car
    (system-re "date --utc '+%Y-%m-%dT%H:%M:%S+0000'"))))

(define (init-registry-file <registry-file>)
  (define registry-file (append-posix-path (get-root) <registry-file>))

  (unless registry-file
    (fatal "Parameter <registry-file> is required, but it is not set"))

  (unless (file-or-directory-exists? registry-file)
    (let ((registry-dir (dirname registry-file)))
      (make-directories registry-dir))
    (write-string-file
     registry-file
     "\n;; This file was automatically created by tegfs-add\n\n\n"))

  registry-file)

(define (tegfs-add/parse
         <target> <title> <tag...>
         --series <key...> <value...>
         <registry-file> <date>)
  (define key-value-pairs
    (list-zip (or <key...> '()) (or <value...> '())))

  (define tags (or <tag...> '()))

  (tegfs-add
   <target> <title> tags
   --series key-value-pairs
   <registry-file> <date>)

  (display "Added!\n"))

(define (tosymbol x)
  (cond
   ((symbol? x) x)
   (else (string->symbol (~a x)))))

(define (tegfs-add
         <target> <title> tags0
         series? key-value-pairs0
         <registry-file> <date>)

  (define id
    (generate-random-id))

  (define last-id-path
    (append-posix-path (get-root) last-id-filename))

  (define prev
    (and series?
         (or (file-or-directory-exists? last-id-path)
             (fatal "Want series, but last-id file is not present"))
         (string-strip (read-string-file last-id-path))))

  (define tags
    (list-deduplicate
     (map tosymbol tags0)))

  (define date
    (or <date> (get-date)))

  (define key-value-pairs
    (map (fn-cons tosymbol identity)
         (if <target>
             (cons
              (cons 'target <target>)
              key-value-pairs0)
             key-value-pairs0)))

  (define registry-file
    (init-registry-file <registry-file>))

  (define registry-dir
    (dirname registry-file))

  (define entry
    (append
     (list (cons id-name id))
     (if <title>
         (list (cons 'title <title>))
         (list))
     (list (cons 'date date))
     (if (null? tags)
         (list)
         (list (cons 'tags tags)))
     (if prev
         (list (cons 'prev prev))
         (list))
     key-value-pairs))

  (when <target>
    (when (absolute-posix-path? <target>)
      (fatal "Target ~s must be a path relative to the registry file, not an absolute path" <target>))
    (let ((target-full (append-posix-path registry-dir <target>)))
      (unless (or (file-or-directory-exists? target-full)
                  (a-weblink? <target>))
        (fatal "Target ~s does not exist. Note that filepath must be relative to the registry file" <target>))))

  (write-string-file last-id-path id)

  (append-string-file
   registry-file
   (with-output-to-string
     (lambda _
       (newline)
       (entry-print entry)
       (newline)))))
