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
%use (write-string-file) "./euphrates/write-string-file.scm"
%use (dprintln) "./euphrates/dprintln.scm"
%use (dprint) "./euphrates/dprint.scm"
%use (directory-files-rec/filter) "./euphrates/directory-files-rec-filter.scm"
%use (lines->string) "./euphrates/lines-to-string.scm"
%use (string->lines) "./euphrates/string-to-lines.scm"
%use (string->words) "./euphrates/string-to-words.scm"
%use (string-strip) "./euphrates/string-strip.scm"
%use (read-string-line) "./euphrates/read-string-line.scm"
%use (list-or-map) "./euphrates/list-or-map.scm"
%use (random-choice) "./euphrates/random-choice.scm"
%use (alphanum/alphabet) "./euphrates/alphanum-alphabet.scm"
%use (append-posix-path) "./euphrates/append-posix-path.scm"
%use (make-temporary-filename) "./euphrates/make-temporary-filename.scm"
%use (mimetype/extensions) "./euphrates/mimetype-extensions.scm"
%use (file-delete) "./euphrates/file-delete.scm"
%use (file-or-directory-exists?) "./euphrates/file-or-directory-exists-q.scm"

%use (fatal) "./fatal.scm"
%use (regfile-suffix) "./regfile-suffix.scm"

%use (debug) "./euphrates/debug.scm"

(define (string-type? s)
  (or (member s '("STRING" "UTF8_STRING" "TEXT" "COMPOUND_TEXT"))
      (string-prefix? "text/" s)))

(define (read-tags)
  (dprintln "Enter tags separated by whitespace: ")
  (string->words (read-string-line)))

(define (read-title)
  (dprintln "Enter the title: ")
  (read-string-line))

(define (get-random-filename directory extension)
  (append-posix-path
   directory
   (string-append
    (list->string
     (random-choice 20 alphanum/alphabet))
    extension)))

(define (get-mime-extension mimetype)
  (let ((ext (assoc mimetype mimetype/extensions)))
    (and ext (string-append "." (car (cdr ext))))))

(define (get-chosen-type-extension chosen-type)
  (cond
   ((string-type? chosen-type) ".txt")
   (else
    (or (get-mime-extension chosen-type)
        (begin
          (dprintln "Cannot deduce file extension for %s, provide it below please:" chosen-type)
          (read-string-line))))))

(define (get-clipboard-content real-type chosen-type target-directory)
  (cond
   ((equal? real-type "text")
    (car (system-re "xclip -selection clipboard -out")))
   (else
    (let* ((extension (get-chosen-type-extension chosen-type))
           (target (get-random-filename target-directory extension)))
      (system-fmt "xclip -selection clipboard -target ~a -out > ~a"
                  chosen-type target)
      target))))

(define (a-weblink? string)
  (or (string-prefix? "http://" string)
      (string-prefix? "https://" string)))

(define (download-temp string)
  (let ((target (make-temporary-filename)))
    (unless (= 0 (system-fmt "curl ~a --output ~a" string target))
      (fatal "Could not download ~s" string))
    target))

(define (get-file-mimetype target)
  (let* ((ret (system-re "xdg-mime query filetype ~a" target))
         (mimetype (car ret))
         (code (cdr ret)))
    (unless (= 0 code)
      (fatal "Could not determine a file type of ~s" string))
    mimetype))

(define (a-media-mimetype? mimetype)
  (or (string-prefix? "video/" mimetype)
      (string-prefix? "audio/" mimetype)
      (string-prefix? "image/" mimetype)))

(define (download directory string)
  (let* ((temp-name (download-temp string))
         (mimetype (get-file-mimetype temp-name)))
    (if (a-media-mimetype? mimetype)
        (let* ((ext (get-mime-extension mimetype))
               (new-name (get-random-filename directory ext)))
          (rename-file temp-name new-name)
          new-name)
        (begin
          (file-delete temp-name)
          #f))))

(define (a-real-filepath? string)
  (file-or-directory-exists? string))

(define (tegfs-save/parse)
  (define _
    (begin
      (unless (= 0 (system-fmt "command -v xclip 1>/dev/null 2>/dev/null"))
        (fatal "Save requires 'xclip' program, but it is not available"))
      (unless (= 0 (system-fmt "command -v xdg-mime 1>/dev/null 2>/dev/null"))
        (fatal "Save requires 'xdg-mime' program, but it is not available"))))

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
       (let ((new-filename
              (string-append (symbol->string (read)) regfile-suffix)))
         (write-string-file
          new-filename
          "\n # This file was automatically created by tagfs-save command\n\n\n")
         new-filename))
      ((1) (car registry-files))
      (else
       (let* ((fzf-input (lines->string registry-files))
              (ret (system-re "echo ~a | fzf" fzf-input))
              (code (cdr ret))
              (chosen (string-strip (car ret))))
         (unless (= 0 code)
           (fatal "Cancelled"))
         chosen))))

  (define target-directory
    (dirname chosen-registry-file))

  (define xclip-types/str
    (car (system-re "xclip -o -target TARGETS -selection clipboard")))
  (define xclip-types/lines
    (string->lines xclip-types/str))

  (define chosen-type
    (let* ((ret (system-re "echo ~a | fzf" xclip-types/str))
           (chosen (car ret))
           (code (cdr ret)))
      (unless (= 0 code)
        (fatal "Cancelled"))
      (string-strip chosen)))

  (dprintln "You have chosen ~s" chosen-type)

  (define real-type0
    (cond
     ((string-type? chosen-type) "text")
     (else #f)))

  (if real-type0
      (dprintln "I think that is ~s" real-type0)
      (dprintln "I don't know what type that is, must assume it's a generic file"))

  (dprintln "To change type information, enter \"!\"")

  (define input1
    (read-title))

  (define-values (title real-type)
    (if (equal? input1 "!")
        (begin
          (dprintln "Enter correct type: ")
          (let ((real (read-string-line)))
            (dprintln "OK, changed the type") ;; FIXME: what if I dont know such type???
            (values (read-title) real)))
        (values input1 real-type0)))

  (define tags (read-tags))

  (define content
    (get-clipboard-content real-type chosen-type target-directory))

  (debug "content: ~s" content)

  (define target
    (cond
     ((a-weblink? content)
      (download target-directory content))
     ((a-real-filepath? content)
      content)
     (else
      (fatal "What you copied is neither a file nor a link. Don't know what to do"))))

  (debug "target: ~s" target)

  (dprintln "Saved!"))




