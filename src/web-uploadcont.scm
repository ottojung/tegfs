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

%var web::uploadcont

%use (append-posix-path) "./euphrates/append-posix-path.scm"
%use (appcomp) "./euphrates/comp.scm"
%use (file-delete) "./euphrates/file-delete.scm"
%use (file-or-directory-exists?) "./euphrates/file-or-directory-exists-q.scm"
%use (hashmap-foreach hashmap-ref) "./euphrates/hashmap.scm"
%use (make-directories) "./euphrates/make-directories.scm"
%use (open-file-port) "./euphrates/open-file-port.scm"
%use (path-get-dirname) "./euphrates/path-get-dirname.scm"
%use (string-drop-n) "./euphrates/string-drop-n.scm"
%use (string->words) "./euphrates/string-to-words.scm"
%use (~a) "./euphrates/tilda-a.scm"
%use (add-entry) "./add-entry.scm"
%use (categorization-complete-selection) "./categorization-complete-selection.scm"
%use (get-random-basename) "./get-random-basename.scm"
%use (get-root) "./get-root.scm"
%use (keyword-tags) "./keyword-tags.scm"
%use (keyword-target) "./keyword-target.scm"
%use (keyword-title) "./keyword-title.scm"
%use (web::body::get-data web::body::get-data/decode) "./web-body-get-data.scm"
%use (web::body-not-found) "./web-body-not-found.scm"
%use (web::callcontext/p) "./web-callcontext-p.scm"
%use (callcontext-body callcontext-token) "./web-callcontext.scm"
%use (web::handle-profun-results/hooked) "./web-handle-profun-results.scm"
%use (web::iterate-profun-results) "./web-iterate-profun-results.scm"
%use (parse-multipart-as-hashmap) "./web-parse-multipart.scm"
%use (web::static-error-message) "./web-static-error-message.scm"
%use (webcore::ask) "./webcore-ask.scm"

%for (COMPILER "guile")
(use-modules (ice-9 binary-ports))
%end

;; TODO: read from config
(define upload-registry-filename "upload/upload.tegfs.reg.lisp")

(define (error-tags-list tags)
  (web::static-error-message 400 (string-append "Some tags are ambiguous: " (~a tags))))

(define duplicates-tags-list
  (web::static-error-message 400 "Tags contain duplicates"))

(define (upload-success-page <target>)
  (if <target>
      (web::static-error-message
       200 (string-append "Uploaded successfully to filename: " <target>))
      (web::static-error-message 200 "Uploaded successfully")))

(define (web::uploadcont/3 callctx body/bytes categorization-text)
  (define body/hash (parse-multipart-as-hashmap body/bytes))

  (define title
    (web::body::get-data/decode body/hash "title"))

  (define tags/checked
    (let ((coll '()))
      (hashmap-foreach
       (lambda (key val)
         (when (string-prefix? "tag:" key)
           (set! coll (cons (string-drop-n 4 key) coll))))
       body/hash)
      coll))

  (define tags/additional
    (string->words
     (web::body::get-data/decode body/hash "additional-tags")))

  (define tags
    (map (compose string->symbol ~a)
         (append tags/additional tags/checked)))

  (define file-content
    (web::body::get-data body/hash "file"))

  (define filename
    (appcomp (hashmap-ref body/hash "file" #f)
             (assoc 'Content-Disposition:filename)
             cdr))

  (define-values (<target> full-filename)
    (if (or (not filename)
            (string-null? filename))
        (values #f #f)
        (let* ((f1
                (append-posix-path (get-root)
                                   (path-get-dirname upload-registry-filename)
                                   filename))
               (t
                (if (file-or-directory-exists? f1)
                    (string-append (get-random-basename) "-" filename)
                    filename))
               (f2
                (append-posix-path (get-root)
                                   (path-get-dirname upload-registry-filename)
                                   t)))
          (values t f2))))

  (define _44
    (begin
      (when full-filename
        (make-directories (path-get-dirname full-filename))
        (let ((port (open-file-port full-filename "w")))
          (put-bytevector port file-content)
          (close-port port)))
      (set! file-content #f)
      (set! body/hash #f)))

  (define tags-list-result
    (categorization-complete-selection categorization-text tags))

  (let ((tags-list (cdr (assoc 'ok tags-list-result))))
    (define entry
      (append

       (if (and title (not (string-null? title)))
           `((,keyword-title . ,title))
           '())

       (if (and <target> (not (string-null? <target>)))
           `((,keyword-target . ,<target>))
           '())

       (if (and tags-list (not (null? tags-list)))
           `((,keyword-tags ,@tags-list))
           '())))

    (define result
      (webcore::ask
       `(whats
         (key ,(callcontext-token callctx))
         (add-entry ,upload-registry-filename ,entry)
         )))

    (web::handle-profun-results/hooked
     result
     (lambda _ ((upload-success-page <target>)))
     (lambda _ (when full-filename (file-delete full-filename))))))

(define (web::uploadcont/2 callctx body/bytes)
  (define key (callcontext-token callctx))
  (define result
    (webcore::ask
     `(whats (key ,key) (categorization C))))

  (web::iterate-profun-results
   result (C)
   (web::uploadcont/3 callctx body/bytes C)))

(define (web::uploadcont)
  (define callctx (web::callcontext/p))
  (define body/bytes (callcontext-body callctx))
  (if body/bytes
      (web::uploadcont/2 callctx body/bytes)
      (web::body-not-found)))
