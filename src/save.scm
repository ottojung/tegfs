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
%use (alphanum-lowercase/alphabet) "./euphrates/alphanum-lowercase-alphabet.scm"
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
%use (path-without-extension) "./euphrates/path-without-extension.scm"
%use (path-get-basename) "./euphrates/path-get-basename.scm"
%use (path-extension) "./euphrates/path-extension.scm"
%use (make-directories) "./euphrates/make-directories.scm"
%use (list-take-n) "./euphrates/list-take-n.scm"
%use (print-in-frame) "./euphrates/print-in-frame.scm"

%use (fatal) "./fatal.scm"
%use (regfile-suffix) "./regfile-suffix.scm"
%use (root/p) "./root-p.scm"
%use (custom-preferences-filename) "./custom-preferences-filename.scm"
%use (a-weblink?) "./a-weblink-q.scm"
%use (tegfs-add) "./add.scm"
%use (tegfs-categorize) "./tags-reader.scm"

%use (debug) "./euphrates/debug.scm"
%use (debugv) "./euphrates/debugv.scm"

;; TODO: factor out graph UI to euphrates
;;       and use progfun in it.

(define (assoc-set-preference key value state)
  (define got (assoc key state))
  (define current (and got (cadr got)))
  (define set-by-user? (and got (caddr got)))
  (if set-by-user?
      state
      (assoc-set-value key (list value set-by-user?) state)))

(define (string-type? s)
  (or (member s '("STRING" "UTF8_STRING" "TEXT" "COMPOUND_TEXT"))
      (string-prefix? "text/" s)))

(define (get-random-basename)
  (list->string
   (random-choice 20 alphanum-lowercase/alphabet)))

(define (get-random-filename directory extension)
  (append-posix-path directory (string-append (get-random-basename) extension)))

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

(define (dump-xclip-temp data-type)
  (dprintln "Dumping xclip...")
  (let ((target (make-temporary-filename)))
    (unless (= 0 (system-fmt "xclip -selection clipboard -target ~a -out > ~a"
                             data-type target))
      (fatal "Could not dump"))
    target))

(define (handle-description description)
  (case description
    ((-none) #f)
    ((-selection) (car (system-re "xclip -selection primary -out")))
    (else description)))

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

(define (initialize-state)
  (map
   (lambda (p)
     (list (car p) #f #f))
   property-table))

(define (set-selection-content-preference state)
  (define selection-content
    (car (system-re "xclip -selection primary -out")))
  (assoc-set-preference '-selection-content selection-content state))

(define (set-text-content-preference state)
  (define text-content
    (car (system-re "xclip -selection clipboard -out")))
  (if (cadr (assoc '-text-content state)) state
      (begin
        (newline)
        (print-in-frame #t #f 3 35 0 #\space "    Clipboard text content")
        (newline)
        (print-in-frame #t #t 3 60 0 #\space text-content)
        (newline)
        (assoc-set-preference '-text-content text-content state))))

(define (set-types-list-preference state)
  (define types-list/str
    (car (system-re "xclip -o -target TARGETS -selection clipboard")))
  (define types-list
    (string->lines types-list/str))
  (assoc-set-preference '-types-list types-list state))

(define (state-set-custom-preferences preferences-code state)
  (if preferences-code
      (eval-in-current-namespace preferences-code)
      state))

(define (get-title edit?)
  (read-answer "Enter the title:"))

(define (get-registry-file edit?)
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

(define (get-real-type edit?)
  (let loop ()
    (define answer (string->symbol (read-answer "Real type: (data/link/localfile/pasta)")))
    (case answer
      ((data link localfile pasta) answer)
      (else
       (dprintln "Please answer either \"data\" or \"link\" or \"localfile\" or \"pasta\"")
       (loop)))))

(define (get-download-flag edit?)
  (let loop ()
    (case (string->symbol (read-answer "Download the target? (yes/no)"))
      ((yes Yes YES) 'yes)
      ((no No NO) 'no)
      (else
       (dprintln "Please answer \"yes\" or \"no\"")
       (loop)))))

(define (set-download-preference state)
  (define text-content (cadr (assoc '-text-content state)))
  (if (a-weblink? text-content) state
      (assoc-set-preference 'download? 'no state)))

(define working-file
  (make-temporary-filename))

(define (get-tags edit?)
  (define result (cdr (tegfs-categorize working-file)))
  (map symbol->string result))

(define (set-real-type-preference state)
  (define text-content (cadr (assoc '-text-content state)))
  (define value
    (cond
     ((string-null? text-content) 'data)
     ((a-weblink? text-content) 'link)
     ((file-or-directory-exists? text-content) 'localfile)
     (else 'pasta)))
  (assoc-set-preference 'real-type value state))

(define (get-description edit?)
  (define answer (read-answer "Enter description: (-none/-selection/custom text)"))
  (cond
   ((member answer '("-none" "-selection")) (string->symbol answer))
   (else answer)))

(define (download-maybe state)
  (define download? (cadr (assoc 'download? state)))
  (define real-type (cadr (assoc 'real-type state)))
  (define text-content (cadr (assoc '-text-content state)))
  (define -temporary-file (cadr (assoc '-temporary-file state)))

  (cond
   ((and (equal? 'link real-type)
         (equal? 'yes download?)
         (not -temporary-file)
         (a-weblink? text-content))
    (let* ((temp-name (download-temp text-content)))
      (assoc-set-preference '-temporary-file temp-name state)))
   (else state)))

(define (dump-xclip-data-maybe state)
  (define download? (cadr (assoc 'download? state)))
  (define data-type (cadr (assoc 'data-type state)))
  (define real-type (cadr (assoc 'real-type state)))
  (define -temporary-file (cadr (assoc '-temporary-file state)))

  (cond
   ((and data-type
         (equal? 'data real-type)
         (not -temporary-file))
    (let* ((temp-name (dump-xclip-temp data-type)))
      (assoc-set-preference '-temporary-file temp-name state)))
   (else state)))

(define (handle-localfile-maybe state)
  (define data-type (cadr (assoc 'data-type state)))
  (define real-type (cadr (assoc 'real-type state)))
  (define -temporary-file (cadr (assoc '-temporary-file state)))
  (define -text-content (cadr (assoc '-text-content state)))

  (cond
   ((and (equal? 'localfile real-type)
         (not -temporary-file))
    (assoc-set-preference '-temporary-file -text-content state))
   (else state)))

(define (handle-pasta-maybe state)
  (define data-type (cadr (assoc 'data-type state)))
  (define real-type (cadr (assoc 'real-type state)))
  (define -temporary-file (cadr (assoc '-temporary-file state)))
  (define -text-content (cadr (assoc '-text-content state)))

  (cond
   ((and (equal? 'pasta real-type))
    (assoc-set-preference 'description -text-content state))
   (else state)))

(define (set-data-type-preference state)
  (define data-type (cadr (assoc 'data-type state)))
  (define real-type (cadr (assoc 'real-type state)))
  (define text-content (cadr (assoc '-text-content state)))
  (define registry-file (cadr (assoc 'registry-file state)))
  (define target-directory (and registry-file (dirname registry-file)))
  (define -temporary-file (cadr (assoc '-temporary-file state)))
  (define download? (cadr (assoc 'download? state)))

  (cond
   ((and (member real-type '(localfile pasta)))
    (assoc-set-preference 'data-type 'ignore state))
   ((and (equal? 'link real-type) (a-weblink? text-content) (equal? 'no download?))
    (assoc-set-preference 'data-type 'ignore state))
   ((and -temporary-file (not data-type))
    (let ((mimetype (get-file-mimetype -temporary-file)))
      (if mimetype
          (assoc-set-preference 'data-type mimetype state)
          (begin
            (dprintln "Could not determine a file type of ~s" string)
            state))))
   (else state)))

(define (set-target-extension-preference state)
  (define data-type (cadr (assoc 'data-type state)))
  (define real-type (cadr (assoc 'real-type state)))
  (define text-content (cadr (assoc '-text-content state)))
  (define registry-file (cadr (assoc 'registry-file state)))
  (define target-directory (and registry-file (dirname registry-file)))
  (define -temporary-file (cadr (assoc '-temporary-file state)))
  (define download? (cadr (assoc 'download? state)))

  (cond
   ((and (equal? 'localfile real-type))
    (assoc-set-preference 'target-extension (path-extension text-content) state))
   ((and (equal? 'pasta real-type))
    (assoc-set-preference 'target-extension 'ignore state))
   ((and (equal? 'link real-type) (a-weblink? text-content) (equal? 'no download?))
    (assoc-set-preference 'target-extension 'ignore state))
   ((and data-type)
    (assoc-set-preference 'target-extension (get-mime-extension data-type) state))
   (else state)))

(define (set-target-basename-preference state)
  (define data-type (cadr (assoc 'data-type state)))
  (define real-type (cadr (assoc 'real-type state)))
  (define text-content (cadr (assoc '-text-content state)))
  (define registry-file (cadr (assoc 'registry-file state)))
  (define target-directory (and registry-file (dirname registry-file)))
  (define -temporary-file (cadr (assoc '-temporary-file state)))
  (define download? (cadr (assoc 'download? state)))

  (cond
   ((and (equal? 'pasta real-type))
    (assoc-set-preference 'target-basename 'ignore state))
   ((and (equal? 'localfile real-type))
    (let ((name (path-without-extension (path-get-basename text-content))))
      (assoc-set-preference 'target-basename name state)))
   (else
    (assoc-set-preference 'target-basename (get-random-basename) state))))

(define (get-data-type edit?)
  (define state (state/p))
  (define types-list (cadr (assoc '-types-list state)))
  (define types-list/str (lines->string types-list))
  (define -text-content (cadr (assoc '-text-content state)))

  (if (a-weblink? -text-content)
      (read-answer "Enter mimetype: ")
      (let* ((ret (system-re "echo ~a | fzf" types-list/str))
             (chosen (car ret))
             (code (cdr ret)))
        (unless (= 0 code)
          (fatal "Cancelled"))
        (string-strip chosen))))

(define (get-target-extension edit?)
  (define input (read-answer "Enter target extension: "))
  (if (string-prefix? "." input) input
      (string-append "." input)))

(define (get-target-basename edit?)
  (read-answer "Enter target basename relative to the registry file: "))

(define (get-confirm edit?)
  (if edit? #f
      (begin
        (dprintln "Press enter if parameters are OK")
        'done)))

(define (index-to-key state i0)
  (define i (- i0 1))
  (if (< i 0) #f
      (let loop ((state state) (i i))
        (if (null? state) #f
            (let* ((x (car state))
                   (key (car x)))
              (if (= 0 i) key
                  (loop (cdr state) (- i 1))))))))

(define (read-answer question)
  (let loop ()
    (define _ (dprintln " ~a" question))
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

(define property-table
  `((title . ,get-title)
    (tags . ,get-tags)
    (registry-file . ,get-registry-file)
    (real-type . ,get-real-type)
    (download? . ,get-download-flag)
    (description . ,get-description)
    (data-type . ,get-data-type)
    (target-extension . ,get-target-extension)
    (target-basename . ,get-target-basename)
    (confirm . ,get-confirm)
    (-temporary-file . #f)
    (-text-content . #f)
    (-selection-content . #f)
    (-types-list . #f)))

(define (get-setter state-key)
  (define got (assoc state-key property-table))
  (and got (cdr got)))

(define menu-callback
  (make-parameter #f))

(define state/p
  (make-parameter #f))

(define (set-by-key key0 state)
  (define callback #f)
  (define counter 0)
  (define key
    (call-with-current-continuation
     (lambda (k) (set! callback k) key0)))
  (set! counter (+ 1 counter))
  (define setter (get-setter key))
  (parameterize ((menu-callback callback))
    (and setter
         (assoc-set-value key (list (setter (> counter 1)) #t) state))))

(define (print-state state)
  (dprintln "\n\n")
  (dprintln " Enter *number* to edit one of below:")
  (for-each
   (lambda (param i)
     (define value0 (cadr param))
     (define value
            (if (and (string? value0) (< 50 (string-length value0)))
                (string-append (list->string (list-take-n 50 (string->list value0))) "...")
                value0))
     (when (get-setter (car param))
       (dprintln "   ~a) ~a: ~s"
                 (+ 1 i)
                 (car param)
                 value)))
   state
   (range (length state))))

(define (eval-state-next state)
  (print-state state)
  (newline) (newline)

  (let loop ((cur state))
    (if (null? cur) #f
        (let* ((param (car cur))
               (key (car param))
               (value (cadr param)))
          (if value
              (loop (cdr cur))
              (parameterize ((state/p state))
                (set-by-key key state)))))))

(define (loop-state set-preferences initial)
  (let loop ((current initial))
    (let* ((state (set-preferences current))
           (next (eval-state-next state)))
      (if next (loop next) state))))

(define state-set-generic-preferences
  (comp
   set-selection-content-preference
   set-text-content-preference
   set-types-list-preference
   (assoc-set-preference 'confirm 'no)
   (assoc-set-preference 'description '-none)
   set-download-preference
   set-real-type-preference
   download-maybe
   dump-xclip-data-maybe
   handle-localfile-maybe
   handle-pasta-maybe
   set-data-type-preference
   set-target-extension-preference
   set-target-basename-preference
   ))

(define (get-custom-prefernences-code)
  (define custom-file (append-posix-path (root/p) custom-preferences-filename))
  (and (file-or-directory-exists? custom-file)
       (with-input-from-file custom-file
         (lambda _
           (cons 'let (cons '() (read-list (current-input-port))))))))

(define (send-state state)
  (define title (cadr (assoc 'title state)))
  (define tags (cadr (assoc 'tags state)))
  (define target-extension (cadr (assoc 'target-extension state)))
  (define target-basename (cadr (assoc 'target-basename state)))
  (define types-list (cadr (assoc '-types-list state)))
  (define data-type (cadr (assoc 'data-type state)))
  (define real-type (cadr (assoc 'real-type state)))
  (define description (cadr (assoc 'description state)))
  (define registry-file (cadr (assoc 'registry-file state)))
  (define -temporary-file (cadr (assoc '-temporary-file state)))
  (define -text-content (cadr (assoc '-text-content state)))
  (define registry-dir (append-posix-path (root/p) (dirname registry-file)))
  (define source (and (a-weblink? -text-content) -temporary-file -text-content))
  (define key-value-pairs (if source (list (cons "source" source)) (list)))
  (define <input> (handle-description description))
  (define <date> #f)
  (define <target>
    (cond
     (-temporary-file
      (string-append target-basename target-extension))
     ((equal? real-type 'pasta) #f)
     (else -text-content)))

  (unless (file-or-directory-exists? registry-dir)
    (make-directories registry-dir))

  (when -temporary-file
    (let* ((new-name0 (append-posix-path registry-dir <target>))
           (new-name (if (file-or-directory-exists? new-name0)
                         (append-posix-path
                          registry-dir
                          (string-append (get-random-basename) <target>))
                         new-name0)))
      (rename-file -temporary-file new-name)))

  (file-delete working-file)

  (tegfs-add
   <target> title tags
   key-value-pairs
   registry-file <date>
   <input>))

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
    (loop-state set-preferences (initialize-state)))

  (send-state state)

  (dprintln "Saved!"))




