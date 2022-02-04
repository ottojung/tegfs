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

%use (with-cli define-cli:show-help) "./euphrates/define-cli.scm"
%use (system-environment-get) "./euphrates/system-environment.scm"
%use (make-directories) "./euphrates/make-directories.scm"
%use (read-all-port) "./euphrates/read-all-port.scm"
%use (current-program-path/p) "./euphrates/current-program-path-p.scm"
%use (file-or-directory-exists?) "./euphrates/file-or-directory-exists-q.scm"
%use (write-string-file) "./euphrates/write-string-file.scm"
%use (append-string-file) "./euphrates/append-string-file.scm"
%use (list-zip) "./euphrates/list-zip.scm"
%use (system-re) "./euphrates/system-re.scm"
%use (string-strip) "./euphrates/string-strip.scm"
%use (list-intersperse) "./euphrates/list-intersperse.scm"
%use (append-posix-path) "./euphrates/append-posix-path.scm"

%use (fatal) "./fatal.scm"
%use (root/p) "./root-p.scm"
%use (a-weblink?) "./a-weblink-q.scm"

(define (get-date)
  (string-strip
   (car
    (system-re "date --utc '+%Y-%m-%d %H:%M:%S+0000'"))))

(define (init-registry-file <registry-file>)
  (define registry-file (append-posix-path (root/p) <registry-file>))

  (unless registry-file
    (fatal "Parameter <registry-file> is required, but it is not set"))

  (unless (file-or-directory-exists? registry-file)
    (let ((registry-dir (dirname registry-file)))
      (make-directories registry-dir))
    (write-string-file
     registry-file
     "\n# This file was automatically created by tegfs-add\n\n\n"))

  registry-file)

(define (tegfs-add/parse
         <target> <title> <tag...>
         <key...> <value...>
         <registry-file> <date>)

  (define input
    (read-all-port (current-input-port)))

  (define key-value-pairs
    (list-zip (or <key...> '()) (or <value...> '())))

  (define tags (or <tag...> '()))

  (tegfs-add
   <target> <title> tags
   key-value-pairs
   <registry-file> <date>
   input)

  (display "Added!\n"))

(define (tegfs-add
         <target> <title> tags
         key-value-pairs0
         <registry-file> <date>
         input)

  (define date
    (or <date> (get-date)))

  (define key-value-pairs
    (cons
     (cons "target" <target>)
     (cons
      (cons "date" (string-append "[" date "]"))
      key-value-pairs0)))

  (define registry-file
    (init-registry-file <registry-file>))

  (define registry-dir
    (dirname registry-file))

  (unless <target>
    (fatal "No target specified"))

  (let ((target-full (append-posix-path registry-dir <target>)))
    (unless (or (file-or-directory-exists? target-full)
                (a-weblink? <target>))
      (fatal "Target ~s does not exist. Note that filepath must be relative to the root" <target>)))

  (append-string-file
   registry-file
   (with-output-to-string
     (lambda ()
       (newline)

       (display "*")
       (when <title>
         (display " ")
         (display <title>))
       (unless (null? tags)
         (display " ")
         (display ":")
         (display
          (apply string-append (list-intersperse ":" tags)))
         (display ":"))
       (newline)

       (for-each
        (lambda (pair)
          (display "  ")
          (display (car pair))
          (display ": ")
          (display (cdr pair))
          (newline))
        key-value-pairs)

       (unless (or (not input) (string-null? input))
         (display "  #+BEGIN_SRC text") (newline)
         (display input)
         (unless (string-suffix? "\n" input)
           (newline))
         (display "  #+END_SRC text") (newline))
       (newline)))))
