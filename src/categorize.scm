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

%var tegfs-categorize/parse
%var tegfs-categorize

%use (make-temporary-filename) "./euphrates/make-temporary-filename.scm"
%use (system-fmt) "./euphrates/system-fmt.scm"
%use (append-posix-path) "./euphrates/append-posix-path.scm"
%use (dprintln) "./euphrates/dprintln.scm"
%use (file-or-directory-exists?) "./euphrates/file-or-directory-exists-q.scm"
%use (write-string-file) "./euphrates/write-string-file.scm"
%use (read-string-file) "./euphrates/read-string-file.scm"
%use (comp) "./euphrates/comp.scm"
%use (fn) "./euphrates/fn.scm"
%use (string->lines) "./euphrates/string-to-lines.scm"
%use (string-strip) "./euphrates/string-strip.scm"
%use (string-split/simple) "./euphrates/string-split-simple.scm"
%use (list-split-on) "./euphrates/list-split-on.scm"
%use (define-tuple) "./euphrates/define-tuple.scm"
%use (read-list) "./euphrates/read-list.scm"
%use (lines->string) "./euphrates/lines-to-string.scm"
%use (list-map/flatten) "./euphrates/list-map-flatten.scm"
%use (curry-if) "./euphrates/curry-if.scm"
%use (CFG-CLI->CFG-AST) "./euphrates/parse-cfg-cli.scm"
%use (list-deduplicate) "./euphrates/list-deduplicate.scm"
%use (raisu) "./euphrates/raisu.scm"
%use (file-delete) "./euphrates/file-delete.scm"
%use (list-and-map) "./euphrates/list-and-map.scm"
%use (list-find-first) "./euphrates/list-find-first.scm"
%use (compose-under) "./euphrates/compose-under.scm"
%use (read-string-line) "./euphrates/read-string-line.scm"

%use (categorization-filename) "./categorization-filename.scm"
%use (root/p) "./root-p.scm"
%use (tegfs-edit-tags) "./edit-tags.scm"

%use (debugv) "./euphrates/debugv.scm"

(define (tegfs-categorize/parse)
  (define result (tegfs-categorize #f))
  (dprintln "Categorized! Chosen tags: ~s" (cdr result)))

(define (tegfs-categorize working-file-maybe)
  (define categorization-file (append-posix-path (root/p) categorization-filename))
  (define working-file
    (or working-file-maybe
        (make-temporary-filename)))

  (unless (file-or-directory-exists? categorization-file)
    (write-string-file
     categorization-file
     ";; This file is for categorization of the tags\n\n-----------\n\n"))

  (unless (file-or-directory-exists? working-file)
    (copy-file categorization-file working-file))

  (let loop ()
    (define result (tegfs-edit-tags working-file))
    (case (car result)
      ((ok)
       (system-fmt "sed 's/\\*//g ; s/\\w\\w*_\\(\\w\\w*\\)/_\\1/g ; s/\\(\\w\\w*\\)^\\w\\w*/\\1^/g ; s/\\(\\w\\w*\\)=\\w\\(\\w\\|,\\|=\\)*/\\1/g' ~a > ~a"
                   working-file categorization-file)
       (unless working-file-maybe
         (delete-file working-file))
       result)
      ((error)
       (dprintln "Error categorizing:")
       (print-errors (cdr result))
       (dprintln "Press enter to continue...")
       (read-string-line)
       (loop))
      ((duplicates)
       (dprintln "Error categorizing:")
       (dprintln "Some tags were chosen twice")
       (dprintln "Press enter to continue...")
       (read-string-line)
       (loop)))))

(define (print-errors errors)
  (for-each
   (lambda (line)
     (dprintln "Tag \"~s\" has ambiguous parents: ~s" (car line) (cdr line)))
   errors))
