;;;; Copyright (C) 2022, 2023  Otto Jung
;;;; This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU Affero General Public License for more details. You should have received a copy of the GNU Affero General Public License along with this program.  If not, see <https://www.gnu.org/licenses/>.

(define (tegfs-categorize/parse)
  (define result (tegfs-categorize #f))
  (log-info
   "Categorized! The list of chosen tags is:\n~a"
   (words->string (map ~a (cdr result)))))

(define (tegfs-categorize working-file-maybe)
  (define categorization-file (append-posix-path (get-root) categorization-filename))
  (define working-file0
    (or working-file-maybe
        (make-temporary-filename/local)))
  (define working-file working-file0)
  (define cfg-part #f)
  (define state (make-hashmap))
  (define currently-handling-var tags-this-variable/string)

  (define (add-choice tag-term)
    (hashset-add! (hashmap-ref state 'choices #f) tag-term))
  (define (add-handled-var var)
    (hashset-add! (hashmap-ref state 'handled-vars #f) var))
  (define (add-all-var var)
    (hashset-add! (hashmap-ref state 'all-vars #f) var))
  (define (add-tag tag)
    (hashset-add! (hashmap-ref state 'tags #f) tag))

  (define (handle-new-tag tag)
    (define parsed (parser tag))
    (define variables (apply append (map cdr parsed)))
    (add-tag tag)
    (for-each add-all-var variables))

  (define counter
    (let ((cnt 0))
      (lambda _ (set! cnt (+ 1 cnt)) cnt)))
  (define parser (make-tag-parser counter))

  (define (add-current-to-tags tags)
    (define parsed (apply append (map parser tags)))
    (define replace
      (curry-if (comp (equal? tags-this-variable/string))
                (const currently-handling-var)))
    (define add
      (curry-if (negate (comp (member currently-handling-var)))
                (lambda (tag) (append tag (list currently-handling-var)))))
    (define added
      (map (comp replace add) parsed))
    (define unparsed (map unparse-tag added))
    unparsed)

  (define (update-working-file)
    (system-fmt "sed -i 's/\\*//g ; s/\\w\\w*_\\(\\w\\w*\\)/_\\1/g ; s/\\(\\w\\w*\\)^\\w\\w*/\\1^/g ; s/\\(\\w\\w*\\)=\\w\\(\\w\\|,\\|=\\)*/\\1=/g' ~a"
                working-file0)
    (call-with-values
        (lambda _ (categorization-split (read-string-file working-file0)))
      (lambda (cfg rules)
        (set! cfg-part cfg))))

  (define (copy-contents from to)
    (write-string-file
     to (read-string-file working-file0)))

  (define (finish)
    (define return-tags (hashset->list (hashmap-ref state 'tags #f)))
    (define return-choices (hashset->list (hashmap-ref state 'choices #f)))

    (copy-contents working-file0 categorization-file)

    (unless working-file-maybe
      (delete-file working-file0))

    (alist-initialize
     (ok return-tags)
     (choices return-choices)))

  (unless (file-or-directory-exists? working-file)
    (write-string-file working-file (categorization-read)))

  (hashmap-set! state 'choices (make-hashset))
  (hashmap-set! state 'handled-vars (make-hashset))
  (hashmap-set! state 'all-vars (make-hashset))
  (hashmap-set! state 'tags (make-hashset))

  (add-all-var tags-this-variable)

  (let loop ()
    (define result (tegfs-edit-tags working-file))

    (add-handled-var tags-this-variable)

    (cond
     ((assoc 'ambiguous result)
      (log-error
       (lines->string
        (append
         (list "Ambiguous choices:")
         (format-errors (cdr (assoc 'ambiguous result))))))
      (log-question "Press enter to continue.")
      (read-string-line)
      (loop))
     ((assoc 'duplicates result)
      (log-error
       "These tags were chosen twice: ~s."
       (cdr (assoc 'duplicates result)))
      (log-question "Press enter to continue.")
      (read-string-line)
      (loop))
     ((assoc 'choices result)
      (unless (equal? working-file working-file0)
        (delete-file working-file))
      (when (equal? working-file working-file0)
        (update-working-file))

      (add-handled-var currently-handling-var)

      (let ((chosen
             (assq-or 'choices result
                      (raisu 'type-error "Expected ~s return" (~a 'choices)))))
        (for-each add-choice chosen))

      (let* ((tags (add-current-to-tags (cdr (assoc 'ok result))))
             (do (for-each handle-new-tag tags))
             (all-vars (hashmap-ref state 'all-vars #f))
             (handled-vars (hashmap-ref state 'handled-vars #f))
             (diff (hashset->list (hashset-difference all-vars handled-vars))))

        (if (null? diff)
            (finish)
            (let ()
              (define new-var (car diff))
              (define temp-content
                (string-append ";; Now handling variable '" (~a new-var) "'\n\n"
                               cfg-part))

              (set! currently-handling-var new-var)
              (set! working-file (make-temporary-filename/local))
              (write-string-file working-file temp-content)

              (loop)))))
     (else
      (raisu 'type-error
             (stringf "Unexpected result from edit: ~s" result)
             result)))))

(define (format-errors errors)
  (map
   (lambda (line)
     (stringf "Tag ~s has ambiguous parents: ~s." (~a (car line)) (cdr line)))
   errors))
