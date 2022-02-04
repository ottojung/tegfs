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
%use (assoc-set-value) "./euphrates/assoc-set-value.scm"
%use (remove-common-prefix) "./euphrates/remove-common-prefix.scm"
%use (read-string-file) "./euphrates/read-string-file.scm"
%use (eval-in-current-namespace) "./euphrates/eval-in-current-namespace.scm"
%use (read-list) "./euphrates/read-list.scm"
%use (range) "./euphrates/range.scm"
%use (assoc-set-default) "./euphrates/assoc-set-default.scm"
%use (comp) "./euphrates/comp.scm"

%use (fatal) "./fatal.scm"
%use (regfile-suffix) "./regfile-suffix.scm"
%use (root/p) "./root-p.scm"
%use (custom-preferences-filename) "./custom-preferences-filename.scm"

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

(define (dump-xclip-temp data-type)
  (dprintln "Dumping xclip...")
  (let ((target (make-temporary-filename)))
    (unless (= 0 (system-fmt "xclip -selection clipboard -target ~a -out > ~a"
                             data-type target))
      (fatal "Could not dump"))
    target))

(define (download-temp string)
  (dprintln "Downloading...")
  (let ((target (make-temporary-filename)))
    (unless (= 0 (system-fmt "curl ~a --output ~a" string target))
      (fatal "Could not download ~s" string))
    target))

(define (get-file-mimetype target)
  (let* ((ret (system-re "xdg-mime query filetype ~a" target))
         (mimetype (car ret))
         (code (cdr ret)))
    (if (= 0 code)
        (string-strip mimetype)
        #f)))

(define (a-media-mimetype? mimetype)
  (or (string-prefix? "video/" mimetype)
      (string-prefix? "audio/" mimetype)
      (string-prefix? "image/" mimetype)))

(define (download directory string target-maybe)
  (let* ((temp-name (download-temp string))
         (mimetype (get-file-mimetype temp-name)))
    (if (a-media-mimetype? mimetype)
        (let* ((ext (get-mime-extension mimetype))
               (new-name (or target-maybe
                             (get-random-filename directory ext))))
          (rename-file temp-name new-name)
          (dprintln "Downloaded a media file")
          (cons new-name mimetype))
        (begin
          (dprintln "Downloaded some garbage, not media...")
          (file-delete temp-name)
          #f))))

(define (a-real-filepath? string)
  (file-or-directory-exists? string))

(define property-table
  '((title . ,get-title)
    (registry-file . ,get-registry-file)
    (real-type . ,get-real-type)
    (download? . ,get-download-flag)
    (tags . ,get-tags)
    (description . ,get-description)
    (data-type . ,get-data-type)
    (target-extension . ,get-target-extension)
    (target . ,get-target)
    (confirm . ,get-confirm)
    (-temporary-file . #f)
    (-text-content . #f)
    (-selection-content . #f)
    (-types-list . #f)))

(define (initialize-state)
  (map
   (lambda (key setter)
     (cons key #f))
   property-table))

(define (set-selection-content-preference state)
  (define selection-content
    (car (system-re "xclip -selection primary -out")))
  (assoc-set-default '-selection-content selection-content state))

(define (set-text-content-preference state)
  (define text-content
    (car (system-re "xclip -selection clipboard -out")))
  (assoc-set-default '-text-content text-content state))

(define (set-types-list-preference state)
  (define types-list/str
    (car (system-re "xclip -o -target TARGETS -selection clipboard")))
  (define types-list
    (string->lines types-list))
  (assoc-set-default '-types-list types-list state))

(define (state-set-custom-preferences preferences-code state)
  (if preferences-code
      (eval-in-current-namespace preferences-code)
      state))

(define (get-title)
  (read-answer "Enter the title:"))

(define (get-registry-file)
  (define registry-files
    (map (lambda (path)
           (remove-common-prefix path (string-append (root/p) "/")))
         (map car
              (directory-files-rec/filter
               (lambda (fullname)
                 (string-suffix? regfile-suffix (basename fullname)))
               (root/p)))))

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

(define (get-real-type state)
  (let loop ()
    (define answer (string->symbol (read-answer "Real type: (data/link)")))
    (case answer
      ((data link) answer)
      (else
       (dprintln "Please answer either \"data\" or \"link\"")
       (loop)))))

(define (get-download-flag)
  (let loop ()
    (case (string->symbol (read-answer "Download the target? (yes/no)"))
      ((yes Yes YES) 'yes)
      ((no No NO) 'no)
      (else
       (dprintln "Please answer \"yes\" or \"no\"")
       (loop)))))

(define (get-tags)
  (string->words (read-answer "Enter tags separated by whitespace:")))

(define (set-real-type-preference state)
  (define text-content (cdr (assoc '-text-content state)))
  (define value
    (if (string-null? text-content)
        'data
        'link))
  (assoc-set-default 'real-type value state))

(define (get-description)
  (define answer (read-answer "Enter description: (-none/-selection/custom text)"))
  (cond
   ((member answer '("-none" "-selection")) (string->symbol answer))
   (else answer)))

(define (download-maybe state)
  (define download? (cdr (assoc 'download? state)))
  (define real-type (cdr (assoc 'real-type state)))
  (define text-content (cdr (assoc '-text-content state)))
  (define -temporary-file (cdr (assoc '-temporary-file state)))

  (cond
   ((and (equal? 'link real-type)
         (equal? 'yes download?)
         (not -temporary-file)
         (a-weblink? text-content))
    (let* ((temp-name (download-temp text-content)))
      (assoc-set-value '-temporary-file temp-name state)))
   (else state)))

(define (dump-xclip-data-maybe state)
  (define download? (cdr (assoc 'download? state)))
  (define data-type (cdr (assoc 'data-type state)))
  (define real-type (cdr (assoc 'real-type state)))
  (define -temporary-file (cdr (assoc '-temporary-file state)))

  (cond
   ((and data-type
         (equal? 'data real-type)
         (not -temporary-file))
    (let* ((temp-name (dump-xclip-temp data-type)))
      (assoc-set-value '-temporary-file temp-name state)))
   (else state)))

(define (set-data-type-preference state)
  (define data-type (cdr (assoc 'data-type state)))
  (define real-type (cdr (assoc 'real-type state)))
  (define text-content (cdr (assoc '-text-content state)))
  (define target (cdr (assoc 'target state)))
  (define registry-file (cdr (assoc 'registry-file state)))
  (define target-directory (and registry-file (dirname registry-file)))
  (define -temporary-file (cdr (assoc '-temporary-file state)))

  (cond
   ((and -temporary-file (not data-type))
    (let ((mimetype (get-file-mimetype -temporary-file)))
      (if mimetype
          (assoc-set-default 'data-type mimetype state)
          (begin
            (dprintln "Could not determine a file type of ~s" string)
            state))))
   (else state)))

(define (set-extension-preference state)
  (define data-type (cdr (assoc 'data-type state)))
  (define real-type (cdr (assoc 'real-type state)))
  (define text-content (cdr (assoc '-text-content state)))
  (define target (cdr (assoc 'target state)))
  (define registry-file (cdr (assoc 'registry-file state)))
  (define target-directory (and registry-file (dirname registry-file)))
  (define -temporary-file (cdr (assoc '-temporary-file state)))

  (cond
   ((and data-type)
    (assoc-set-default 'target-extension (get-mime-extension data-type) state))
   (else state)))

(define (set-target-preference state)
  (define data-type (cdr (assoc 'data-type state)))
  (define real-type (cdr (assoc 'real-type state)))
  (define text-content (cdr (assoc '-text-content state)))
  (define target (cdr (assoc 'target state)))
  (define registry-file (cdr (assoc 'registry-file state)))
  (define target-directory (and registry-file (dirname registry-file)))
  (define -temporary-file (cdr (assoc '-temporary-file state)))
  (define target-extension (cdr (assoc 'target-extension state)))

  (fatal "TODO"))

  ;; (cond
  ;;  ((and (not target) target-extension -temporary-file)
  ;;   (let ((new-name (string-append -temporary-file target-extension)))
  ;;     (assoc-set-default 'target new-name)))
  ;;  (else (not target) target-extension -temporary-file
  ;;   state)))

(define (get-data-type)
  (define state (state/p))
  (define types-list (cdr (assoc '-types-list state)))
  (define types-list/str (lines->string types-list))

  (let* ((ret (system-re "echo ~a | fzf" types-list/str))
         (chosen (car ret))
         (code (cdr ret)))
    (unless (= 0 code)
      (fatal "Cancelled"))
    (string-strip chosen)))

(define (get-target)
  (read-answer "Enter target path relative to tegfs root: "))

(define (get-target-extension)
  (read-answer "Enter extension with a dot: "))

(define (get-confirm)
  (dprintln "Press enter if parameters are OK")
  'done)

(define (index-to-key state i)
  (if (< i 0) #f
      (let loop ((state state) (i i))
        (if (null? state) #f
            (let* ((x (car state))
                   (key (car x)))
              (if (= 0 i) key
                  (loop (cdr state) (- i 1))))))))

(define (read-answer question)
  (let loop ()
    (define _ (dprintln question))
    (define state (state/p))
    (define answer (read-string-line))
    (define num (string->number answer))
    (if num
        (let ((key (index-to-key state num)))
          (if key
              ((menu-callback) key)
              (begin
                (dprintln "Bad index ~s, must be one of the listed items" num)
                (loop))))
        answer)))

(define (get-setter state-key)
  (define got (assoc state-key property-table))
  (and got (cdr got)))

(define menu-callback
  (make-parameter #f))

(define state/p
  (make-parameter #f))

(define (set-by-key key state)
  (define callback #f)
  (define setter
    (call-with-current-continuation
     (lambda (k)
       (set! callback k)
       (get-setter key))))
  (parameterize ((menu-callback callback))
    (and setter
         (assoc-set-value key (setter) state))))

(define (print-state state)
  (dprintln " Enter *number* to edit one of below:")
  (for-each
   (lambda (param i)
     (when (get-setter (car param))
       (dprintln "   ~a) ~a: ~a" (+ 1 i) (car param) (cdr param))))
   state
   (range (length state))))

(define (eval-state-next set-preferences state0)
  (define state (set-preferences state0))

  (print-state state)
  (newline) (newline)

  (let loop ((cur state))
    (if (null? cur) #f
        (let* ((param (car cur))
               (key (car param))
               (value (cdr param)))
          (if value
              (loop (cdr cur))
              (parameterize ((state/p state))
                (set-by-key key state)))))))

(define state-set-generic-preferences
  (comp
   (assoc-set-default 'confirm 'no)
   (assoc-set-default 'description '-none)
   (assoc-set-default 'download 'yes)
   set-real-type-preference
   download-maybe
   dump-xclip-data-maybe
   set-data-type-preference
   set-target-preference
   ))

(define (get-custom-prefernences-code)
  (define custom-file (append-posix-path (root/p) custom-preferences-filename))
  (and (file-or-directory-exists? custom-file)
       (with-input-from-file custom-file
         (lambda _
           (cons 'let (cons '() (read-list (current-input-port))))))))

(define (tegfs-save/parse)
  (define _
    (begin
      (unless (= 0 (system-fmt "command -v xclip 1>/dev/null 2>/dev/null"))
        (fatal "Save requires 'xclip' program, but it is not available"))
      (unless (= 0 (system-fmt "command -v xdg-mime 1>/dev/null 2>/dev/null"))
        (fatal "Save requires 'xdg-mime' program, but it is not available"))))

  (define preferences-code (get-custom-prefernences-code))
  (define set-preferences
    (comp state-set-generic-preferences
          (state-set-custom-preferences preferences-code)))

  (define state
    (let loop ((state (initialize-state)))
      (let ((next (eval-state-next set-preferences state)))
        (if next (loop next) state))))

  ;; (define chosen-type
  ;;   (let* ((ret (system-re "echo ~a | fzf" xclip-types/str))
  ;;          (chosen (car ret))
  ;;          (code (cdr ret)))
  ;;     (unless (= 0 code)
  ;;       (fatal "Cancelled"))
  ;;     (string-strip chosen)))

  ;; (dprintln "You have chosen ~s" chosen-type)

  ;; (define real-type0
  ;;   (cond
  ;;    ((string-type? chosen-type) "text")
  ;;    (else #f)))

  ;; (if real-type0
  ;;     (dprintln "I think that is ~s" real-type0)
  ;;     (dprintln "I don't know what type that is, must assume it's a generic file"))

  ;; (dprintln "To change type information, enter \"!\"")

  ;; (define input1
  ;;   (read-title))

  ;; (define-values (title real-type)
  ;;   (if (equal? input1 "!")
  ;;       (begin
  ;;         (dprintln "Enter correct type: ")
  ;;         (let ((real (read-string-line)))
  ;;           (dprintln "OK, changed the type") ;; FIXME: what if I dont know such type???
  ;;           (values (read-title) real)))
  ;;       (values input1 real-type0)))

  ;; (define tags (read-tags))

  ;; (define content
  ;;   (get-clipboard-content real-type chosen-type target-directory))

  ;; (debug "content: ~s" content)

  ;; (define target
  ;;   (cond
  ;;    ((a-weblink? content)
  ;;     (download target-directory content))
  ;;    ((a-real-filepath? content)
  ;;     content)
  ;;    (else
  ;;     (fatal "What you copied is neither a file nor a link. Don't know what to do"))))

  ;; (debug "target: ~s" target)

  (dprintln "Saved!"))




