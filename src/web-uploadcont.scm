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

%var web-uploadcont

%use (append-posix-path) "./euphrates/append-posix-path.scm"
%use (appcomp) "./euphrates/comp.scm"
%use (file-delete) "./euphrates/file-delete.scm"
%use (file-or-directory-exists?) "./euphrates/file-or-directory-exists-q.scm"
%use (hashmap-ref) "./euphrates/hashmap.scm"
%use (make-directories) "./euphrates/make-directories.scm"
%use (open-file-port) "./euphrates/open-file-port.scm"
%use (path-get-dirname) "./euphrates/path-get-dirname.scm"
%use (profune-communicator-handle) "./euphrates/profune-communicator.scm"
%use (~a) "./euphrates/tilda-a.scm"
%use (~s) "./euphrates/tilda-s.scm"
%use (words->string) "./euphrates/words-to-string.scm"
%use (add-entry) "./add-entry.scm"
%use (tegfs-process-categorization-text) "./edit-tags.scm"
%use (get-random-basename) "./get-random-basename.scm"
%use (get-root) "./get-root.scm"
%use (keyword-tags) "./keyword-tags.scm"
%use (keyword-target) "./keyword-target.scm"
%use (keyword-title) "./keyword-title.scm"
%use (web-bad-request) "./web-bad-request.scm"
%use (web-body-not-found) "./web-body-not-found.scm"
%use (web-callcontext/p) "./web-callcontext-p.scm"
%use (callcontext-body callcontext-request callcontext-token) "./web-callcontext.scm"
%use (web-context/p) "./web-context-p.scm"
%use (web-make-communicator) "./web-make-communicator.scm"
%use (parse-multipart-as-hashmap) "./web-parse-multipart.scm"
%use (web-static-error-message) "./web-static-error-message.scm"

%for (COMPILER "guile")
(use-modules (ice-9 iconv))
(use-modules (ice-9 binary-ports))
%end

;; TODO: read from config
(define upload-registry-filename "upload/upload.tegfs.reg.lisp")

(define (error-tags-list tags)
  (web-static-error-message 400 (string-append "Some tags are ambiguous: " (~a tags))))

(define duplicates-tags-list
  (web-static-error-message 400 "Tags contain duplicates"))

(define (upload-success-page <target>)
  (if <target>
      (web-static-error-message
       200 (string-append "Uploaded successfully to filename: " <target>))
      (web-static-error-message 200 "Uploaded successfully")))

(define (web-uploadcont/2 callctx body/bytes)
  (define request (callcontext-request callctx))
  (define body/hash (parse-multipart-as-hashmap body/bytes))

  (define (get-data a-key)
    (appcomp (hashmap-ref body/hash a-key #f)
             (assoc 'data)
             cdr))

  (define (get-data/decode a-key)
    (bytevector->string (get-data a-key) "utf-8"))

  (define title
    (get-data/decode "title"))

  (define tags
    (get-data/decode "tags"))

  (define file-content
    (get-data "file"))

  (define filename
    (appcomp (hashmap-ref body/hash "file" #f)
             (assoc 'Content-Disposition:filename)
             cdr))

  (define-values (<target> full-filename)
    (if (not filename) (values #f #f)
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
    (when full-filename
      (make-directories (path-get-dirname full-filename))
      (let ((port (open-file-port full-filename "w")))
        (put-bytevector port file-content)
        (close-port port))))

  (define tags-list-result
    (tegfs-process-categorization-text tags))

  ;; TODO: edit the categorization file
  (define tags-list
    (cond
     ((assoc 'ambiguous tags-list-result)
      (error-tags-list (cdr (assoc 'ambiguous tags-list-result))))
     (else
      (cdr (assoc 'ok tags-list-result)))))

  (define entry
    `((,keyword-target . ,<target>)
      (,keyword-title . ,title)
      (,keyword-tags ,@tags-list)))

  (define result
    (profune-communicator-handle
     (web-make-communicator (web-context/p))
     `(whats
       (key ,(callcontext-token callctx))
       (add-entry ,upload-registry-filename ,entry)
       )))

  (case (car result)
    ((its) ((upload-success-page <target>)))
    (else
     (when full-filename (file-delete full-filename))
     (web-bad-request "error: ~a" (words->string (map ~s (cadr result)))))))

(define (web-uploadcont)
  (define callctx (web-callcontext/p))
  (define body/bytes (callcontext-body callctx))
  (if body/bytes
      (web-uploadcont/2 callctx body/bytes)
      (web-body-not-found)))
