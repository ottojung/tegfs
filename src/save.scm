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

%use (append-posix-path) "./euphrates/append-posix-path.scm"
%use (assoc-or) "./euphrates/assoc-or.scm"
%use (assoc-set-value) "./euphrates/assoc-set-value.scm"
%use (comp) "./euphrates/comp.scm"
%use (dprintln) "./euphrates/dprintln.scm"
%use (eval-in-current-namespace) "./euphrates/eval-in-current-namespace.scm"
%use (file-delete) "./euphrates/file-delete.scm"
%use (file-or-directory-exists?) "./euphrates/file-or-directory-exists-q.scm"
%use (lines->string) "./euphrates/lines-to-string.scm"
%use (list-take-n) "./euphrates/list-take-n.scm"
%use (make-directories) "./euphrates/make-directories.scm"
%use (path-extensions) "./euphrates/path-extensions.scm"
%use (path-get-basename) "./euphrates/path-get-basename.scm"
%use (path-without-extension) "./euphrates/path-without-extension.scm"
%use (print-in-frame) "./euphrates/print-in-frame.scm"
%use (raisu) "./euphrates/raisu.scm"
%use (range) "./euphrates/range.scm"
%use (read-list) "./euphrates/read-list.scm"
%use (read-string-file) "./euphrates/read-string-file.scm"
%use (read-string-line) "./euphrates/read-string-line.scm"
%use (string-split-3) "./euphrates/string-split-3.scm"
%use (string-split/simple) "./euphrates/string-split-simple.scm"
%use (string-strip) "./euphrates/string-strip.scm"
%use (system-environment-get) "./euphrates/system-environment.scm"
%use (system-fmt) "./euphrates/system-fmt.scm"
%use (system-re) "./euphrates/system-re.scm"
%use (~s) "./euphrates/tilda-s.scm"
%use (url-get-path) "./euphrates/url-get-path.scm"
%use (write-string-file) "./euphrates/write-string-file.scm"
%use (a-weblink?) "./a-weblink-q.scm"
%use (tegfs-add) "./add.scm"
%use (tegfs-categorize) "./categorize.scm"
%use (choose-clipboard-data-type classify-clipboard-text-content dump-clipboard-to-file dump-clipboard-to-temporary get-clipboard-text-content get-clipboard-type-extension) "./clipboard.scm"
%use (custom-preferences-filename) "./custom-preferences-filename.scm"
%use (default-save-registry-key) "./default-save-registry-key.scm"
%use (tegfs-dump-clipboard tegfs-dump-clipboard/pasta) "./dump-clipboard.scm"
%use (fatal) "./fatal.scm"
%use (file-is-audio?) "./file-is-audio-q.scm"
%use (file-is-image?) "./file-is-image-q.scm"
%use (file-is-video?) "./file-is-video-q.scm"
%use (get-config) "./get-config.scm"
%use (get-random-basename) "./get-random-basename.scm"
%use (get-registry-files) "./get-registry-files.scm"
%use (get-root) "./get-root.scm"
%use (make-temporary-filename/local) "./make-temporary-filename-local.scm"
%use (regfile-suffix) "./regfile-suffix.scm"
%use (target-name) "./target-name.scm"

;; TODO: factor out graph UI to euphrates
;;       and use progfun in it.

(define (assoc-set-preference key value state)
  (define got (assoc key state))
  (define current (and got (cadr got)))
  (define set-by-user? (and got (caddr got)))
  (if set-by-user?
      state
      (assoc-set-value key (list value set-by-user?) state)))

(define (get-random-filename directory extension)
  (append-posix-path directory (string-append (get-random-basename) extension)))

(define (get-chosen-type-extension chosen-type)
  (or (get-clipboard-type-extension chosen-type)
      (begin
        (dprintln "Cannot deduce file extension for %s, provide it below please:" chosen-type)
        (read-string-line))))

(define (get-clipboard-content real-type chosen-type target-directory)
  (cond
   ((equal? real-type "text")
    (or (get-clipboard-text-content)
        (fatal "Could not get clipboard text content")))
   (else
    (let* ((extension (get-chosen-type-extension chosen-type))
           (target (get-random-filename target-directory extension)))
      (or (dump-clipboard-to-file chosen-type target)
          (fatal "Could not dump clipboard data"))
      target))))

(define (dump-clipboard-temp data-type)
  (dprintln "Dumping clipboard...")
  (let ((result (dump-clipboard-to-temporary data-type)))
    (unless result
      (fatal "Could not dump"))
    result))

(define (url-get-domain-name url)
  (define split (string-split/simple url #\/))
  (let loop ((i 0) (split split))
    (cond
     ((null? split) "")
     ((> i 2) "")
     (else
      (string-append (car split) "/" (loop (+ 1 i) (cdr split)))))))

(define (download-temp url)
  (dprintln "Downloading...")
  (let* ((target (make-temporary-filename/local))
         ;; NOTE: some websites (looking at you 8chan) require referer to be set to its domain name, which is silly!! and which is stupid >:
         (domain-name (url-get-domain-name url))
         (headers (string-append "referer: " (~s domain-name))))
    (unless (= 0 (system-fmt "wget ~a -O ~a" url target))
      (unless (= 0 (system-fmt "wget --header ~a ~a -O ~a" headers url target))
        (fatal "Could not download ~s" url)))
    target))

(define (get-file-mimetype target)
  (let* ((ret (system-re "file --brief --mime-type ~a" target))
         (mimetype (car ret))
         (code (cdr ret)))
    (if (= 0 code)
        (string-strip mimetype)
        #f)))

(define (a-real-filepath? string)
  (file-or-directory-exists? string))

(define (initialize-state)
  (map
   (lambda (p)
     (list (car p) #f #f))
   property-table))

(define (set-savefile-preference <savetext> state)
  (if (not <savetext>) state
      (let* ()
        (assoc-set-preference
         '-text-content <savetext>
         state))))

(define (set-link-preference --link state)
  (assoc-set-preference
   'link? (if --link 'yes 'no)
   state))

(define (set-text-content-preference state)
  (if (cadr (assoc '-text-content state)) state
      (let ((text-content (or (get-clipboard-text-content)
                              (fatal "Could not get clipboard text"))))
        (newline)
        (print-in-frame #t #f 3 35 0 #\space "    Clipboard text content")
        (newline)
        (print-in-frame #t #t 3 60 0 #\space text-content)
        (newline)
        (assoc-set-preference '-text-content text-content state))))

(define (set-registry-file-preference state)
  (define config (get-config))
  (if (not config) state
      (let* ((default-save-registry (assoc-or default-save-registry-key config #f)))
        (if (not default-save-registry) state
            (assoc-set-preference
             'registry-file
             (car default-save-registry)
             state)))))

(define (state-set-custom-preferences preferences-code state)
  (if preferences-code
      (eval-in-current-namespace preferences-code)
      state))

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

(define (set-download-preference state)
  (define text-content (cadr (assoc '-text-content state)))
  (if (a-weblink? text-content)
      (let ((name (url-get-path text-content)))
        (if (or (file-is-video? name)
                (file-is-image? name)
                (file-is-audio? name))
            (assoc-set-preference 'download? 'yes state)
            state))
      (assoc-set-preference 'download? 'no state)))

(define working-file/p (make-parameter #f))

(define (get-tags edit?)
  (define result (cdr (tegfs-categorize (working-file/p))))
  (map symbol->string result))

(define (set-real-type-preference state)
  (define text-content (cadr (assoc '-text-content state)))
  (define value (classify-clipboard-text-content text-content))
  (assoc-set-preference 'real-type value state))

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

(define (dump-clipboard-data-maybe state)
  (define download? (cadr (assoc 'download? state)))
  (define data-type (cadr (assoc 'data-type state)))
  (define real-type (cadr (assoc 'real-type state)))
  (define -temporary-file (cadr (assoc '-temporary-file state)))

  (cond
   ((and data-type
         (equal? 'data real-type)
         (not -temporary-file))
    (let* ((temp-name (dump-clipboard-temp data-type)))
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
    (let* ((temp-name (dump-clipboard-temp data-type)))
      (write-string-file temp-name -text-content)
      (assoc-set-preference
       'target-extension ".txt"
       (assoc-set-preference
        '-temporary-file temp-name
        state))))
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
   ((and (equal? real-type 'localfile))
    (assoc-set-preference 'data-type 'ignore state))
   ((and (equal? 'link real-type) (a-weblink? text-content) (equal? 'no download?))
    (assoc-set-preference 'data-type 'ignore state))
   ((and (equal? real-type 'pasta))
    (assoc-set-preference 'data-type 'TEXT state))
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
    (assoc-set-preference 'target-extension (path-extensions text-content) state))
   ((and (equal? 'pasta real-type))
    (assoc-set-preference 'target-extension ".txt" state))
   ((and (equal? 'link real-type) (a-weblink? text-content) (equal? 'no download?))
    (assoc-set-preference 'target-extension 'ignore state))
   ((and data-type)
    (assoc-set-preference 'target-extension (get-clipboard-type-extension data-type) state))
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
   ((and (equal? 'localfile real-type))
    (let ((name (path-without-extension (path-get-basename text-content))))
      (assoc-set-preference 'target-basename name state)))
   (else
    (assoc-set-preference 'target-basename (get-random-basename) state))))

(define (get-data-type edit?)
  (define state (state/p))
  (define -text-content (cadr (assoc '-text-content state)))

  (if (a-weblink? -text-content)
      (read-answer "Enter mimetype: ")
      (let* ((chosen (choose-clipboard-data-type)))
        (unless chosen
          (fatal "Cancelled"))
        chosen)))

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
    (link? . ,get-link-flag)
    (series . ,get-series)
    (data-type . ,get-data-type)
    (target-extension . ,get-target-extension)
    (target-basename . ,get-target-basename)
    (confirm . ,get-confirm)
    (-temporary-file . #f)
    (-text-content . #f)))

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

(define (state-set-generic-preferences --link <savetext>)
  (comp
   (set-savefile-preference <savetext>)
   (set-link-preference --link)
   set-text-content-preference
   set-registry-file-preference
   (assoc-set-preference 'series 'no)
   (assoc-set-preference 'confirm 'no)
   set-download-preference
   set-real-type-preference
   download-maybe
   handle-localfile-maybe
   set-data-type-preference
   dump-clipboard-data-maybe
   handle-pasta-maybe
   set-target-extension-preference
   set-target-basename-preference
   ))

(define (get-custom-prefernences-code)
  (define custom-file (append-posix-path (get-root) custom-preferences-filename))
  (and (file-or-directory-exists? custom-file)
       (with-input-from-file custom-file
         (lambda _
           (cons 'let (cons '() (read-list (current-input-port))))))))

(define (send-state state)
  (define title (cadr (assoc 'title state)))
  (define tags (cadr (assoc 'tags state)))
  (define target-extension (cadr (assoc 'target-extension state)))
  (define target-basename (cadr (assoc 'target-basename state)))
  (define data-type (cadr (assoc 'data-type state)))
  (define real-type (cadr (assoc 'real-type state)))
  (define series (cadr (assoc 'series state)))
  (define series? (case series ((yes) #t) ((no) #f) (else (fatal "Bad value for series ~s" series))))
  (define link? (case (cadr (assoc 'link? state)) ((yes) #t) ((no) #f) (else (fatal "Bad value for link? ~s" (cadr (assoc 'link? state))))))
  (define registry-file (cadr (assoc 'registry-file state)))
  (define -temporary-file (cadr (assoc '-temporary-file state)))
  (define -text-content (cadr (assoc '-text-content state)))
  (define registry-dir (append-posix-path (get-root) (dirname registry-file)))
  (define source (and (a-weblink? -text-content) -temporary-file -text-content))
  (define key-value-pairs (if source (list (cons "source" source)) (list)))
  (define <date> #f)
  (define _11
    (unless (file-or-directory-exists? registry-dir)
      (make-directories registry-dir)))
  (define <target>
    (cond
     (-temporary-file
      (let* ((target-name0 (string-append target-basename target-extension))
             (target-name (if (file-or-directory-exists?
                                (append-posix-path registry-dir target-name0))
                               (string-append (get-random-basename) target-name0)
                               target-name0))
             (target-fullname (append-posix-path registry-dir target-name)))
        (rename-file -temporary-file target-fullname)
        (when link?
          (symlink target-fullname -temporary-file))
        target-name))
     ((equal? real-type 'pasta) #f)
     (else -text-content)))

  (file-delete (working-file/p))

  (tegfs-add
   <target> title tags
   series? key-value-pairs
   registry-file <date>))

(define (tegfs-save/parse/no-remote --link <savetext>)
  (define preferences-code
    (get-custom-prefernences-code))
  (define generic-preferences
    (state-set-generic-preferences --link <savetext>))
  (define custom-preferences
    (comp (state-set-custom-preferences preferences-code)))
  (define set-preferences
    (compose generic-preferences custom-preferences))

  (parameterize ((working-file/p (make-temporary-filename/local)))
    (let ((state
           (loop-state set-preferences (initialize-state))))
      (send-state state)))

  (dprintln "Saved!"))

(define (tegfs-save/parse/remote <remote> <savetext>)
  (define savetext
    (and <savetext>
         (case (classify-clipboard-text-content <savetext>)
           ((data) (raisu 'savetext-cannot-be-data <savetext>))
           ((link localfile) <savetext>)
           ((pasta) (tegfs-dump-clipboard/pasta <savetext>))
           (else (raisu 'unexpected-real-type <savetext>)))))
  (define working-text
    (or savetext
        (tegfs-dump-clipboard)))

  (define real-type (classify-clipboard-text-content working-text))

  (define remote-name
    (case real-type
      ((localfile)
       (unless (file-or-directory-exists? working-text)
         (raisu 'file-must-have-been-created working-text))
       (unless (= 0 (system-fmt "rsync --info=progress2 --mkpath --partial ~a ~a:tegfs-remote-hub/" working-text <remote>))
         (fatal "Syncing to remote failed"))
       (append-posix-path "tegfs-remote-hub" (path-get-basename working-text)))
      ((link) working-text)
      ((data pasta) (raisu 'impossible-real-type real-type working-text))
      (else (raisu 'unhandled-real-type real-type working-text))))

  (define temp-file (get-random-basename))
  (define temp-file-content (string-append (~s real-type) ":" remote-name))
  (write-string-file temp-file temp-file-content)

  (unless (= 0 (system-fmt "exec scp ~a ~a:tegfs-remote-name" temp-file <remote>))
    (file-delete temp-file)
    (fatal "Something went wrong on the other side"))
  (file-delete temp-file)

  (unless (= 0 (system-fmt "exec ssh -t ~a \"exec /bin/sh -l -c \\\"exec tegfs save --from-remote\\\"\"" <remote>))
    (fatal "Something went wrong on the other side"))

  (dprintln "Saved!"))

(define (tegfs-save/parse/from-remote)
  (define HOME (system-environment-get "HOME"))
  (define temp-file-content
    (read-string-file
     (append-posix-path HOME "tegfs-remote-name")))
  (define-values (real-type/string col remote-name)
    (string-split-3 #\: temp-file-content))
  (define _12737123
    (when (string-null? col)
      (fatal "Client sent bad tegfs-remote-name")))
  (define real-type (string->symbol real-type/string))
  (define <savetext>
    (case real-type
      ((localfile) (append-posix-path HOME remote-name))
      ((link) remote-name)
      ((data pasta) (fatal "Impossible real type: ~s" real-type))
      (else (raisu 'unhandled-real-type-in-server real-type))))
  (dprintln "Remote file content: ~s" <savetext>)
  (tegfs-save/parse/no-remote #f <savetext>)
  (file-delete "tegfs-remote-name"))

(define (tegfs-save/parse <remote> --from-remote --link <savetext>)
  (cond
   (<remote>
    (tegfs-save/parse/remote <remote> <savetext>))
   (--from-remote
    (tegfs-save/parse/from-remote))
   (else
    (tegfs-save/parse/no-remote --link <savetext>))))
