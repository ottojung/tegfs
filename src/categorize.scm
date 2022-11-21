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

%use (append-posix-path) "./euphrates/append-posix-path.scm"
%use (dprintln) "./euphrates/dprintln.scm"
%use (file-or-directory-exists?) "./euphrates/file-or-directory-exists-q.scm"
%use (hashmap-ref hashmap-set! make-hashmap) "./euphrates/hashmap.scm"
%use (hashset->list hashset-add! hashset-difference make-hashset) "./euphrates/hashset.scm"
%use (read-string-file) "./euphrates/read-string-file.scm"
%use (read-string-line) "./euphrates/read-string-line.scm"
%use (system-fmt) "./euphrates/system-fmt.scm"
%use (~a) "./euphrates/tilda-a.scm"
%use (words->string) "./euphrates/words-to-string.scm"
%use (write-string-file) "./euphrates/write-string-file.scm"
%use (categorization-filename) "./categorization-filename.scm"
%use (categorization-split) "./categorization-split.scm"
%use (tegfs-edit-tags) "./edit-tags.scm"
%use (get-root) "./get-root.scm"
%use (make-temporary-filename/local) "./make-temporary-filename-local.scm"
%use (parse-tag-structure) "./parse-tag.scm"
%use (tags-this-variable) "./tags-this-variable.scm"
%use (unparse-tag) "./unparse-tag.scm"

(define (tegfs-categorize/parse)
  (define result (tegfs-categorize #f))
  (dprintln "Categorized! The list of chosen tags is:\n~a" (words->string (map ~a (cdr result)))))

(define (tegfs-categorize working-file-maybe)
  (define categorization-file (append-posix-path (get-root) categorization-filename))
  (define working-file0
    (or working-file-maybe
        (make-temporary-filename/local)))
  (define working-file working-file0)
  (define cfg-part #f)
  (define state (make-hashmap))
  (define currently-handling-var tags-this-variable)

  (define (add-handled-var var)
    (hashset-add! (hashmap-ref state 'handled-vars #f) var))
  (define (add-all-var var)
    (hashset-add! (hashmap-ref state 'all-vars #f) var))
  (define (add-tag tag)
    (hashset-add! (hashmap-ref state 'tags #f) tag))

  (define (handle-new-tag tag)
    (define parsed (parser tag))
    (define variables (apply append (map cddr parsed)))
    (add-tag tag)
    (for-each add-all-var variables))

  (define counter
    (let ((cnt 0))
      (lambda _ (set! cnt (+ 1 cnt)) cnt)))
  (define parser (parse-tag-structure counter))

  (define (add-current-to-tags tags)
    (define parsed (apply append (map parser tags)))
    (define added
      (map (lambda (p)
             (if (and (equal? tags-this-variable currently-handling-var)
                      (member (car p) '(single prefix-suffix)))
                 (cdr p)
                 (append (cdr p) `(,currently-handling-var))))
           parsed))
    (define unparsed (map unparse-tag added))
    unparsed)

  (define (update-working-file)
    (system-fmt "sed -i 's/\\*//g ; s/\\w\\w*_\\(\\w\\w*\\)/_\\1/g ; s/\\(\\w\\w*\\)^\\w\\w*/\\1^/g ; s/\\(\\w\\w*\\)=\\w\\(\\w\\|,\\|=\\)*/\\1=/g' ~a"
                working-file0)
    (call-with-values
        (lambda _ (categorization-split (read-string-file working-file0)))
      (lambda (cfg rules)
        (set! cfg-part cfg))))

  (define (finish)
    (define return (hashset->list (hashmap-ref state 'tags #f)))

    (copy-file working-file0 categorization-file)

    (unless working-file-maybe
      (delete-file working-file0))

    (cons 'ok return))

  (unless (file-or-directory-exists? categorization-file)
    (write-string-file
     categorization-file
     ";; This file is for categorization of the tags\n\n-----------\n\n"))

  (unless (file-or-directory-exists? working-file)
    (copy-file categorization-file working-file))

  (hashmap-set! state 'handled-vars (make-hashset))
  (hashmap-set! state 'all-vars (make-hashset))
  (hashmap-set! state 'tags (make-hashset))
  (add-all-var tags-this-variable)

  (let loop ()
    (define result (tegfs-edit-tags working-file))
    (cond
      ((= 1 (length result))
       (unless (equal? working-file working-file0)
         (delete-file working-file))
       (when (equal? working-file working-file0)
         (update-working-file))

       (add-handled-var currently-handling-var)
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
      ((assoc 'ambiguous result)
       (dprintln "Error categorizing:")
       (print-errors (cdr (assoc 'ambiguous result)))
       (dprintln "Press enter to continue...")
       (read-string-line)
       (loop))
      ((assoc 'duplicates result)
       (dprintln "Error categorizing:")
       (dprintln "These tags were chosen twice: ")
       (dprintln "~s" (cdr (assoc 'duplicates result)))
       (dprintln "Press enter to continue...")
       (read-string-line)
       (loop)))))

(define (print-errors errors)
  (for-each
   (lambda (line)
     (dprintln "Tag \"~s\" has ambiguous parents: ~s" (car line) (cdr line)))
   errors))
