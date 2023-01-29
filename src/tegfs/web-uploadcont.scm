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
  (define-module (tegfs web-uploadcont)
    :export (web::uploadcont)
    :use-module ((euphrates append-posix-path) :select (append-posix-path))
    :use-module ((euphrates comp) :select (appcomp))
    :use-module ((euphrates file-delete) :select (file-delete))
    :use-module ((euphrates file-or-directory-exists-q) :select (file-or-directory-exists?))
    :use-module ((euphrates hashmap) :select (hashmap-foreach hashmap-ref))
    :use-module ((euphrates make-directories) :select (make-directories))
    :use-module ((euphrates open-file-port) :select (open-file-port))
    :use-module ((euphrates path-get-dirname) :select (path-get-dirname))
    :use-module ((euphrates string-drop-n) :select (string-drop-n))
    :use-module ((euphrates string-to-words) :select (string->words))
    :use-module ((euphrates tilda-a) :select (~a))
    :use-module ((tegfs add-entry) :select (add-entry))
    :use-module ((tegfs categorization-complete-selection) :select (categorization-complete-selection))
    :use-module ((tegfs default-share-expiery-time) :select (default-share-expiery-time))
    :use-module ((tegfs get-random-basename) :select (get-random-basename))
    :use-module ((tegfs get-root) :select (get-root))
    :use-module ((tegfs keyword-entry-registry-path) :select (keyword-entry-registry-path))
    :use-module ((tegfs keyword-tags) :select (keyword-tags))
    :use-module ((tegfs keyword-target) :select (keyword-target))
    :use-module ((tegfs keyword-title) :select (keyword-title))
    :use-module ((tegfs web-body-get-data) :select (web::body::get-data web::body::get-data/decode))
    :use-module ((tegfs web-body-not-found) :select (web::body-not-found))
    :use-module ((tegfs web-callcontext-p) :select (web::callcontext/p))
    :use-module ((tegfs web-callcontext) :select (callcontext-body callcontext-token))
    :use-module ((tegfs web-handle-profun-results) :select (web::handle-profun-results/default-fail-fun))
    :use-module ((tegfs web-iterate-profun-results) :select (web::iterate-profun-results))
    :use-module ((tegfs web-make-info-box-response) :select (web::make-info-box-response))
    :use-module ((tegfs web-parse-multipart) :select (parse-multipart-as-hashmap))
    :use-module ((tegfs web-return) :select (web::return))
    :use-module ((tegfs web-static-error-message) :select (web::static-error-message))
    :use-module ((tegfs webcore-ask) :select (webcore::ask)))))



(cond-expand
 (guile
  (use-modules (ice-9 binary-ports))
  ))

;; TODO: read from config
(define upload-registry-filename "upload/upload.tegfs.reg.lisp")

(define (error-tags-list tags)
  (web::static-error-message 400 (string-append "Some tags are ambiguous: " (~a tags))))

(define duplicates-tags-list
  (web::static-error-message 400 "Tags contain duplicates"))

(define (upload-success-page)
  (web::make-info-box-response "Uploaded successfully"))

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

       `((,keyword-entry-registry-path . ,upload-registry-filename))

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
         (share-full ,entry ,default-share-expiery-time _ F))))

    (web::iterate-profun-results
     :or (lambda _
           (when full-filename (file-delete full-filename))
           (web::handle-profun-results/default-fail-fun result))
     :results result (F)
     (if F
         ;; TODO: figure out how to get the vid even without <target>
         (let ((L (string-append "details?vid=" F)))
           (web::return
            303
            `((Location . ,L)
              (Cache-Control . "no-cache"))
            #f))
         (upload-success-page)))))

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
