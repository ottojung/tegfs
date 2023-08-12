;;;; Copyright (C) 2022, 2023  Otto Jung
;;;; This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU Affero General Public License for more details. You should have received a copy of the GNU Affero General Public License along with this program.  If not, see <https://www.gnu.org/licenses/>.

(define (dump-clipboard-temp mimetype)
  (dprintln "Dumping clipboard...")
  (let ((result (dump-clipboard-to-temporary mimetype)))
    (unless result
      (fatal "Could not dump"))
    result))

(define (get-target-extension)
  (define input (CLI::read-answer-string "Target extension: "))
  (if (string-prefix? "." input) input
      (string-append "." input)))

(define (get-confirm)
  (if (CLI::swiched-field?/p) #f
      (begin
        (CLI::read-answer-string "Press enter if parameters are OK")
        'done)))

(define (get-tags)
  (define result
    (tegfs-categorize (CLI::save-working-file/p)))

  (define choices
    (assq-or 'choices result))

  (alist-initialize!:return-multiple
   `((tags-choices . ,choices)
     ,(alist-initialize!:unset 'inferred-tags)
     ,(alist-initialize!:unset 'tags)
     ,(alist-initialize!:unset 'inferred-tags))))

(define (print-text-content ret)
  (newline)
  (print-in-frame #t #f 3 35 0 #\space "    Clipboard text content")
  (newline)
  (print-in-frame #t #t 3 60 0 #\space ret)
  (newline))

(define (print-inferred-tags inferred-tags)
  (when inferred-tags
    (display " Inferred tags: ")
    (display (words->string (map ~a inferred-tags)))
    (newline)
    (newline)))

(define (print-setter-fields get-alist current-setter)
  (define setters (alist-initialize!:current-setters))
  (print-inferred-tags (assq-or 'inferred-tags (get-alist)))

  (dprintln " Enter *number* to edit one of below.")
  (for-each
   (lambda (setter i)
     (unless (equal? '* (car setter))
       (let ()
         (define value0 ((cdr setter) 'current))
         (define value1
           (if (and (string? value0) (< 50 (string-length value0)))
               (string-append (list->string (list-take-n 50 (string->list value0))) "...")
               value0))
         (define value
           (cond
            ((list? value1) (words->string (map ~s value1)))
            (value1 (~s value1))
            (else "")))

         (dprintln "   ~a~a) ~a: ~a"
                   (if (equal? current-setter (car setter)) ">" " ")
                   (+ 1 i)
                   (car setter)
                   value))))
   setters
   (range (length setters))))

(define (useradvice get-alist --interactive)
  (lambda (name alist recalculate? thunk)
    (unless --interactive
      (fatal "Cannot infer the value of ~s" name))

    (if recalculate?
        (dprintln "\n Switched to ~s" (~s name))
        (begin
          (newline) (newline) (newline) (newline) (newline) (newline) (newline) (newline) (newline) (newline)
          (newline) (newline) (newline) (newline) (newline) (newline) (newline) (newline) (newline) (newline)
          (newline) (newline) (newline) (newline) (newline) (newline) (newline) (newline) (newline) (newline)
          (newline) (newline) (newline) (newline) (newline) (newline) (newline) (newline) (newline) (newline)
          (newline) (newline) (newline) (newline) (newline) (newline) (newline) (newline) (newline) (newline)
          (newline) (newline) (newline) (newline) (newline) (newline) (newline) (newline) (newline) (newline)
          (print-setter-fields get-alist name)))
    (newline)
    (thunk)))

(define (CLI::save::loop

         --content <content>
         --kind <kind>
         --interactive
         --no-interactive
         --title <title>
         --tag <tag...>
         --series
         --no-series
         --diropen
         --no-diropen
         --dirpreview
         --no-dirpreview
         --download
         --no-download
         --source <source>
         --no-source
         --unsure-if-download
         --target <add-target>
         --mimetype <mimetype>
         --note <note>
         --link
         --share <share-duration>
         --remote <remote>
         --no-remote
         --date <date>
         --key <key...> <value...>

         )

  (define plugins (get-save-plugins))
  (define root (get-root))
  (alist-initialize-loop
   :current current
   :initial
   ((confirm 'no)
    (diropen? (if --diropen 'yes 'no))
    (dirpreview? (if --dirpreview 'yes 'no))
    (series (if --series 'yes 'no))
    (link? (if --link 'yes 'no))
    (title (or <title> (if --interactive #f 'none)))

    (tags-choices
     (or (and <tag...>
              (let ((parser (make-tag-parser 0)))
                (list-map/flatten
                 (comp (categorization-translate-direct-choice parser))
                 <tag...>)))))

    (tags
     (or
      (and (tags-choices)
           (list-deduplicate/reverse
            (map tag-choice->immediate-tag
                 (tags-choices))))
      (if --interactive #f '())))

    (inferred-tags
     (and (tags-choices)
          (assq-or
           'ok (categorization-complete-selection
                (categorization-read)
                (tags-choices)))))

    (additional-properties
     (if <key...> (map cons (map string->symbol <key...>) <value...>) '()))

    (source
     (if --source <source>
         (or (and (-text-content) (a-weblink? (-text-content)) (-temporary-file))
             'none)))

    (-text-content
     (or <content>
         (and --interactive
              (let ((ret (get-clipboard-text-content)))
                (and ret (print-text-content ret))
                ret))))

    (* (run-save-plugins root current plugins))

    (download?
     (cond
      (--download 'yes)
      (--no-download 'no)
      (else
       (cond
        ((equal? <kind> 'localfile) 'no)
        ((and (-text-content) (a-weblink? (-text-content)))
         (let ((name (url-get-path (-text-content))))
           (and (or (file-is-video? name)
                    (file-is-image? name)
                    (file-is-audio? name))
                'yes)))
        (else 'no)))))

    (kind
     (or <kind>
         (and (-text-content)
              (classify-clipboard-text-content (-text-content)))
         'data))

    (-temporary-file
     (or
      (and (equal? 'localfile (kind))
           (-text-content))
      (and (equal? 'link (kind))
           (equal? 'yes (download?))
           (-text-content)
           (download-to-temporary-file (-text-content)))
      (and (equal? 'data (kind))
           (mimetype 'or #f)
           (and --interactive
                (dump-clipboard-temp (mimetype))))
      (and (equal? 'pasta (kind))
           (-text-content)
           (let* ((temp-name (make-temporary-filename)))
             (write-string-file temp-name (-text-content))
             temp-name))))

    (mimetype
     (or
      <mimetype>
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
      (and <add-target>
           (path-without-extension <add-target>))
      (and (equal? 'localfile (kind))
           (-text-content)
           (path-without-extension (path-get-basename (-text-content))))
      (get-random-basename)))

    (target-extension
     (or
      (and <add-target>
           (path-extensions <add-target>))
      (and (equal? 'pasta (kind)) ".txt")
      (and (equal? 'localfile (kind))
           (-text-content)
           (path-extensions (-text-content)))
      (and (equal? 'link (kind))
           (equal? 'no (download?))
           'ignore)
      (and (mimetype 'or #f)
           (get-clipboard-type-extension (mimetype)))
      (and (not (-temporary-file)) 'none)))

    (date (or <date> (generate-entry-date-timestamp)))

    (note (or <note> 'none))

    )

   :useradvice (useradvice (lambda _ current) --interactive)
   :user
   ((title (CLI::read-answer-string "Title: "))
    (tags (get-tags))
    (kind (CLI::read-enumeration "Kind: " '(data link localfile pasta)))
    (download? (CLI::read-enumeration "Download target to the new location?" '(yes no)))
    (series (CLI::read-enumeration "Is this item related to the one previously saved?" '(yes no)))
    (mimetype (CLI::read-answer-string "Mimetype: "))
    (source (CLI::read-answer-string "Original source for this entry: "))
    (target-basename (CLI::read-answer-string "Target basename: "))
    (target-extension (get-target-extension))
    (link? (CLI::read-enumeration "Link target to the new location?" '(yes no)))
    (diropen? (CLI::read-enumeration "Diropen?" '(yes no)))
    (dirpreview? (CLI::read-enumeration "Dirpreview?" '(yes no)))
    (date (CLI::read-answer-string "Date (example: \"2023-08-11T22:45:42+0000\")"))
    (note (CLI::read-answer-string "Note: "))
    ;; (confirm (get-confirm)) ;; TODO: fix this and enable again
    )

   ))
