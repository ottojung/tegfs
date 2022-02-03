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

%var tegfs-save/parse

%use (system-fmt) "./euphrates/system-fmt.scm"
%use (system-re) "./euphrates/system-re.scm"
%use (system-re) "./euphrates/system-re.scm"
%use (dprintln) "./euphrates/dprintln.scm"
%use (directory-files-rec/filter) "./euphrates/directory-files-rec-filter.scm"
%use (lines->string) "./euphrates/lines-to-string.scm"

%use (fatal) "./fatal.scm"
%use (regfile-suffix) "./regfile-suffix.scm"

%use (debug) "./euphrates/debug.scm"

(define (tegfs-save/parse)
  (define _
    (unless (= 0 (system-fmt "command -v xclip 1>/dev/null 2>/dev/null"))
      (fatal "Save requires 'xclip' program, but it is not available")))

  (define registry-files
    (map car
         (directory-files-rec/filter
          (lambda (fullname)
            (string-suffix? regfile-suffix (basename fullname)))
          ".")))

  (define chosen-registry-file
    (case (length registry-files)
      ((0)
       (dprintln "No existing registry files found.")
       (dprintln "Enter a name for a new one:")
       (string-append (symbol->string (read)) regfile-suffix))
      ((1) (car registry-files))
      (else
       (let* ((fzf-input (lines->string registry-files))
              (ret (system-re "echo ~a | fzf" fzf-input))
              (code (cdr ret))
              (chosen (car ret)))
         (unless (= 0 code)
           (fatal "Cancelled"))
         chosen))))

  (debug "FILES: ~s" registry-files)
  (debug "CHOSEN: ~s" chosen-registry-file)

  (dprintln "Saved!"))




