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

(cond-expand
 (guile
  (define-module (tegfs cli-save-loop)
    :export (CLI::save::loop)
    :use-module ((euphrates alist-initialize-bang) :select (alist-initialize!:current-setters))
    :use-module ((euphrates alist-initialize-loop) :select (alist-initialize-loop))
    :use-module ((euphrates assq-or) :select (assq-or))
    :use-module ((euphrates asyncproc-input-text-p) :select (asyncproc-input-text/p))
    :use-module ((euphrates dprintln) :select (dprintln))
    :use-module ((euphrates lines-to-string) :select (lines->string))
    :use-module ((euphrates list-intersperse) :select (list-intersperse))
    :use-module ((euphrates list-take-n) :select (list-take-n))
    :use-module ((euphrates path-extensions) :select (path-extensions))
    :use-module ((euphrates path-get-basename) :select (path-get-basename))
    :use-module ((euphrates path-without-extension) :select (path-without-extension))
    :use-module ((euphrates print-in-frame) :select (print-in-frame))
    :use-module ((euphrates range) :select (range))
    :use-module ((euphrates read-string-line) :select (read-string-line))
    :use-module ((euphrates run-syncproc-re-star) :select (run-syncproc/re*))
    :use-module ((euphrates string-strip) :select (string-strip))
    :use-module ((euphrates stringf) :select (stringf))
    :use-module ((euphrates tilda-a) :select (~a))
    :use-module ((euphrates tilda-s) :select (~s))
    :use-module ((euphrates url-get-path) :select (url-get-path))
    :use-module ((euphrates write-string-file) :select (write-string-file))
    :use-module ((tegfs a-weblink-q) :select (a-weblink?))
    :use-module ((tegfs categorize) :select (tegfs-categorize))
    :use-module ((tegfs clipboard) :select (classify-clipboard-text-content dump-clipboard-to-temporary get-clipboard-text-content get-clipboard-type-extension))
    :use-module ((tegfs download-to-temporary-file) :select (download-to-temporary-file))
    :use-module ((tegfs fatal) :select (fatal))
    :use-module ((tegfs file-is-audio-q) :select (file-is-audio?))
    :use-module ((tegfs file-is-image-q) :select (file-is-image?))
    :use-module ((tegfs file-is-video-q) :select (file-is-video?))
    :use-module ((tegfs get-config) :select (get-config))
    :use-module ((tegfs get-file-mimetype) :select (get-file-mimetype))
    :use-module ((tegfs get-random-basename) :select (get-random-basename))
    :use-module ((tegfs get-registry-files) :select (get-registry-files))
    :use-module ((tegfs get-root) :select (get-root))
    :use-module ((tegfs get-save-plugins) :select (get-save-plugins))
    :use-module ((tegfs keyword-default-save-registry) :select (keyword-default-save-registry))
    :use-module ((tegfs regfile-suffix) :select (regfile-suffix))
    :use-module ((tegfs run-save-plugins) :select (run-save-plugins))
    )))



(define (dump-clipboard-temp mimetype)
  (dprintln "Dumping clipboard...")
  (let ((result (dump-clipboard-to-temporary mimetype)))
    (unless result
      (fatal "Could not dump"))
    result))

(define (get-target-extension)
  (define input (read-answer "Enter target extension: "))
  (if (string-prefix? "." input) input
      (string-append "." input)))

(define (get-confirm)
  (if (swiched-field?) #f
      (begin
        (read-answer "Press enter if parameters are OK")
        'done)))

(define (get-registry-file)
  (define registry-files (get-registry-files))

  (case (length registry-files)
    ((0)
     (let ((answer
            (read-answer "No existing registry files found.\nEnter a name for a new one:")))
       (string-append answer regfile-suffix)))
    ((1) (car registry-files))
    (else
     (let ()
       (define fzf-input (lines->string registry-files))
       (define-values (output code)
         (parameterize ((asyncproc-input-text/p fzf-input))
           (run-syncproc/re* "fzf")))
       (define chosen (string-strip output))
       (unless (= 0 code)
         (fatal "Cancelled"))
       chosen))))

(define (read-enumeration name option-list/0)
  (define option-list (map ~a option-list/0))
  (define hint/inner
    (apply string-append (list-intersperse "/" option-list)))
  (define hint
    (stringf "~a (~a)" name hint/inner))
  (define option-list/down
    (map string-downcase option-list))

  (let loop ()
    (define answer (string-downcase (read-answer hint)))
    (if (member answer option-list/down)
        (string->symbol answer)
        (begin
          (dprintln "\nPlease choose on of the following: ~a" hint/inner)
          (loop)))))

(define working-file/p (make-parameter #f))

(define (get-tags)
  (cdr (tegfs-categorize (working-file/p))))

(define (print-text-content ret)
  (newline)
  (print-in-frame #t #f 3 35 0 #\space "    Clipboard text content")
  (newline)
  (print-in-frame #t #t 3 60 0 #\space ret)
  (newline))

(define (print-setter-fields current-setter)
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
         (dprintln "   ~a~a) ~a: ~a"
                   (if (equal? current-setter (car setter)) ">" " ")
                   (+ 1 i)
                   (car setter)
                   (if value (~s value) "")))))
   setters
   (range (length setters)))
  )

(define (useradvice name alist recalculate? thunk)
  (if recalculate?
      (dprintln "\n Switched to ~s" (~s name))
      (print-setter-fields name))
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
  (define plugins (get-save-plugins))
  (define config (get-config))
  (define root (get-root))
  (alist-initialize-loop
   :current current
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

    (* (run-save-plugins config root current plugins))

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

    (kind
     (classify-clipboard-text-content (-text-content)))

    (-temporary-file
     (or
      (and (equal? 'localfile (kind))
           (-text-content))
      (and (equal? 'link (kind))
           (equal? 'yes (download?))
           (download-to-temporary-file (-text-content)))
      (and (equal? 'data (kind))
           (mimetype 'or #f)
           (dump-clipboard-temp (mimetype)))
      (and (equal? 'pasta (kind))
           (let* ((temp-name (dump-clipboard-temp (mimetype))))
             (write-string-file temp-name (-text-content))
             temp-name))))

    (mimetype
     (or
      (and (equal? (kind) 'pasta)
           "text/plain")
      (and (equal? 'link (kind))
           (equal? 'no (download?))
           "text/uri-list")
      (and (-temporary-file 'or #f)
           (or (get-file-mimetype (-temporary-file))
               (begin
                 (dprintln "Could not determine the file type of ~s" (-temporary-file))
                 #f)))))

    (target-basename
     (or
      (and (equal? 'localfile (kind))
           (path-without-extension (path-get-basename (-text-content))))
      (get-random-basename)))

    (target-extension
     (or
      (and (equal? 'pasta (kind)) ".txt")
      (and (equal? 'localfile (kind))
           (path-extensions (-text-content)))
      (and (equal? 'link (kind))
           (equal? 'no (download?))
           'ignore)
      (and (mimetype 'or #f)
           (get-clipboard-type-extension (mimetype)))))

    (note 'none)

    )

   :useradvice useradvice
   :user
   ((title (read-answer "Enter the title:"))
    (tags (get-tags))
    (kind (read-enumeration "Kind: " '(data link localfile pasta)))
    (download? (read-enumeration "Download target to the new location?" '(yes no)))
    (series (read-enumeration "Is this item related to the one previously saved?" '(yes no)))
    (mimetype (read-answer "Enter mimetype: "))
    (target-basename (read-answer "Enter target basename relative to the registry file: "))
    (target-extension (get-target-extension))
    (link? (read-enumeration "Link target to the new location?" '(yes no)))
    (diropen? (read-enumeration "Diropen?" '(yes no)))
    (dirpreview? (read-enumeration "Dirpreview?" '(yes no)))
    (note (read-answer "Enter note: "))
    (registry-file (get-registry-file))
    ;; (confirm (get-confirm)) ;; TODO: fix this and enable again
    )

   ))
