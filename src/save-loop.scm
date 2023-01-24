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

%run guile

%var CLI::save::loop

%use (alist-initialize!:current-setters) "./euphrates/alist-initialize-bang.scm"
%use (alist-initialize-loop) "./euphrates/alist-initialize-loop.scm"
%use (assq-or) "./euphrates/assq-or.scm"
%use (dprintln) "./euphrates/dprintln.scm"
%use (lines->string) "./euphrates/lines-to-string.scm"
%use (list-take-n) "./euphrates/list-take-n.scm"
%use (path-extensions) "./euphrates/path-extensions.scm"
%use (path-get-basename) "./euphrates/path-get-basename.scm"
%use (path-without-extension) "./euphrates/path-without-extension.scm"
%use (print-in-frame) "./euphrates/print-in-frame.scm"
%use (range) "./euphrates/range.scm"
%use (read-string-line) "./euphrates/read-string-line.scm"
%use (string-strip) "./euphrates/string-strip.scm"
%use (system-re) "./euphrates/system-re.scm"
%use (url-get-path) "./euphrates/url-get-path.scm"
%use (write-string-file) "./euphrates/write-string-file.scm"
%use (a-weblink?) "./a-weblink-q.scm"
%use (tegfs-categorize) "./categorize.scm"
%use (choose-clipboard-data-type classify-clipboard-text-content dump-clipboard-to-temporary get-clipboard-text-content get-clipboard-type-extension) "./clipboard.scm"
%use (download-to-temporary-file) "./download-to-temporary-file.scm"
%use (fatal) "./fatal.scm"
%use (file-is-audio?) "./file-is-audio-q.scm"
%use (file-is-image?) "./file-is-image-q.scm"
%use (file-is-video?) "./file-is-video-q.scm"
%use (get-config) "./get-config.scm"
%use (get-file-mimetype) "./get-file-mimetype.scm"
%use (get-random-basename) "./get-random-basename.scm"
%use (get-registry-files) "./get-registry-files.scm"
%use (keyword-default-save-registry) "./keyword-default-save-registry.scm"
%use (regfile-suffix) "./regfile-suffix.scm"

(define (dump-clipboard-temp data-type)
  (dprintln "Dumping clipboard...")
  (let ((result (dump-clipboard-to-temporary data-type)))
    (unless result
      (fatal "Could not dump"))
    result))

(define (get-target-extension edit?)
  (define input (read-answer "Enter target extension: "))
  (if (string-prefix? "." input) input
      (string-append "." input)))

(define (get-target-basename edit?)
  (read-answer "Enter target basename relative to the registry file: "))

(define (get-series edit?)
  (let loop ()
    (case (string->symbol (read-answer "Is this item related to the one previously saved? (yes/no)"))
      ((yes Yes YES) 'yes)
      ((no No NO) 'no)
      (else
       (dprintln "Please answer \"yes\" or \"no\"")
       (loop)))))

(define (get-confirm edit?)
  (if edit? #f
      (begin
        (read-answer "Press enter if parameters are OK")
        'done)))

(define (get-title edit?)
  (read-answer "Enter the title:"))

(define (get-registry-file edit?)
  (define registry-files (get-registry-files))

  (case (length registry-files)
    ((0)
     (let ((answer
            (read-answer "No existing registry files found.\nEnter a name for a new one:")))
       (string-append answer regfile-suffix)))
    ((1) (car registry-files))
    (else
     (let* ((fzf-input (lines->string registry-files))
            (ret (system-re "echo ~a | fzf" fzf-input))
            (code (cdr ret))
            (chosen (string-strip (car ret))))
       (unless (= 0 code)
         (fatal "Cancelled"))
       chosen))))

(define (get-real-type edit?)
  (let loop ()
    (define answer (string->symbol (read-answer "Real type: (data/link/localfile/pasta)")))
    (case answer
      ((data link localfile pasta) answer)
      (else
       (dprintln "Please answer either \"data\" or \"link\" or \"localfile\" or \"pasta\"")
       (loop)))))

(define (get-link-flag edit?)
  (let loop ()
    (case (string->symbol (read-answer "Link target to the new location? (yes/no)"))
      ((yes Yes YES) 'yes)
      ((no No NO) 'no)
      (else
       (dprintln "Please answer \"yes\" or \"no\"")
       (loop)))))

(define (get-download-flag edit?)
  (let loop ()
    (case (string->symbol (read-answer "Download the target? (yes/no)"))
      ((yes Yes YES) 'yes)
      ((no No NO) 'no)
      (else
       (dprintln "Please answer \"yes\" or \"no\"")
       (loop)))))

(define (get-diropen-flag edit?)
  (let loop ()
    (case (string->symbol (read-answer "Diropen? (yes/no)"))
      ((yes Yes YES) 'yes)
      ((no No NO) 'no)
      (else
       (dprintln "Please answer \"yes\" or \"no\"")
       (loop)))))

(define (get-dirpreview-flag edit?)
  (let loop ()
    (case (string->symbol (read-answer "Dirpreview? (yes/no)"))
      ((yes Yes YES) 'yes)
      ((no No NO) 'no)
      (else
       (dprintln "Please answer \"yes\" or \"no\"")
       (loop)))))

(define working-file/p (make-parameter #f))

(define (get-tags edit?)
  (cdr (tegfs-categorize (working-file/p))))

(define (print-text-content ret)
  (newline)
  (print-in-frame #t #f 3 35 0 #\space "    Clipboard text content")
  (newline)
  (print-in-frame #t #t 3 60 0 #\space ret)
  (newline))

(define (print-setter-fields)
  (define setters (alist-initialize!:current-setters))
  (dprintln "\n\n")
  (dprintln " Enter *number* to edit one of below.")
  (for-each
   (lambda (setter i)
     (unless (equal? '* (car setter))
       (let ()
         (define value0 ((cdr setter) 'current))
         (define value
           (if (and (string? value0) (< 50 (string-length value0)))
               (string-append (list->string (list-take-n 50 (string->list value0))) "...")
               value0))
         (dprintln "   ~a) ~a: ~s"
                   (+ 1 i)
                   (car setter)
                   value))))
   setters
   (range (length setters)))
  )

(define (useradvice name alist recalculate? thunk)
  (unless recalculate?
    (print-setter-fields))
  (thunk))

(define swiched-field?
  (make-parameter #f))

(define (index-to-key i0)
  (define setters (alist-initialize!:current-setters))
  (define i (- i0 1))
  (if (< i 0) #f
      (let loop ((setters setters) (i i))
        (if (null? setters) #f
            (let ((x (car setters)))
              (if (= 0 i) x
                  (loop (cdr setters) (- i 1))))))))

(define (read-answer question)
  (let loop ()
    (define _2 (dprintln " ~a" question))
    ;; (define state (state/p))
    (define answer (read-string-line))
    (define num (string->number answer))
    (if num
        (let ((name+setter (index-to-key num)))
          (if name+setter
              (parameterize ((swiched-field? #t))
                ((cdr name+setter) 'recalculate))
              (begin
                (dprintln "Bad index ~s, must be one of the listed items" num)
                (loop))))
        answer)))

(define (CLI::save::loop --link <savetext>)
  (alist-initialize-loop
   :initial
   ((series 'no)
    (confirm 'no)
    (diropen? 'no)
    (dirpreview? 'no)
    (link? (if --link 'yes 'no))

    (-text-content
     (or <savetext>
         (let ((ret (get-clipboard-text-content)))
           (print-text-content ret)
           ret)))

    (registry-file
     (define config (get-config))
     (and config
          (car (assq-or keyword-default-save-registry config (list #f)))))

    (download?
     (if (a-weblink? (-text-content))
         (let ((name (url-get-path (-text-content))))
           (and (or (file-is-video? name)
                    (file-is-image? name)
                    (file-is-audio? name))
                'yes))
         'no))

    (real-type
     (classify-clipboard-text-content (-text-content)))

    (-temporary-file
     (or
      (and (equal? 'localfile (real-type))
           (-text-content))
      (and (equal? 'link (real-type))
           (equal? 'yes (download?))
           (download-to-temporary-file (-text-content)))
      (and (equal? 'data (real-type))
           (data-type 'or #f)
           (dump-clipboard-temp (data-type)))
      (and (equal? 'pasta (real-type))
           (let* ((temp-name (dump-clipboard-temp (data-type))))
             (write-string-file temp-name (-text-content))
             temp-name))))

    (data-type
     (or
      (and (equal? (real-type) 'pasta)
           "text/plain")
      (and (equal? 'link (real-type))
           (equal? 'no (download?))
           "text/uri-list")
      (and (-temporary-file 'or #f)
           (or (get-file-mimetype (-temporary-file))
               (begin
                 (dprintln "Could not determine the file type of ~s" (-temporary-file))
                 #f)))))

    (target-extension
     (or
      (and (equal? 'pasta real-type) ".txt")
      (and (equal? 'localfile (real-type))
           (path-extensions (-text-content)))
      (and (equal? 'link (real-type))
           (equal? 'no (download?))
           'ignore)
      (and (data-type 'or #f)
           (get-clipboard-type-extension (data-type)))))

    (target-basename
     (or
      (and (equal? 'localfile (real-type))
           (path-without-extension (path-get-basename (-text-content))))
      (get-random-basename)))

    )

   :useradvice useradvice
   :user
   ((title (get-title #f))
    (tags (get-tags #f))
    (registry-file (get-registry-file #f))
    (real-type (get-real-type #f))
    (download? (get-download-flag #f))
    (diropen? (get-diropen-flag #f))
    (dirpreview? (get-dirpreview-flag #f))
    (link? (get-link-flag #f))
    (series (get-series #f))

    (data-type
     (if (a-weblink? (-text-content))
         (read-answer "Enter mimetype: ")
         (let* ((chosen (choose-clipboard-data-type)))
           (unless chosen
             (fatal "Cancelled"))
           chosen)))

    (target-extension (get-target-extension #f))
    (target-basename (get-target-basename #f))
    (confirm (get-confirm #f)))

   ))
