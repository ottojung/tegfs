;;;; Copyright (C) 2022  Otto Jung
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

%var webcore::shared-entry-contains

%use (directory-files-depth-iter) "./euphrates/directory-files-depth-iter.scm"
%use (profun-accept? profun-ctx-set profun-set profun-set-meta) "./euphrates/profun-accept.scm"
%use (make-profun-error) "./euphrates/profun-error.scm"
%use (profun-op-lambda) "./euphrates/profun-op-lambda.scm"
%use (profun-reject) "./euphrates/profun-reject.scm"
%use (profun-bound-value? profun-unbound-value?) "./euphrates/profun-value.scm"
%use (filemap-ref-by-senderid) "./filemap.scm"
%use (keyword-target) "./keyword-target.scm"
%use (sharedinfo-sourcepath) "./sharedinfo.scm"
%use (standalone-file->entry/prefixed) "./standalone-file-to-entry.scm"
%use (context-filemap/2) "./web-context.scm"

(define webcore::shared-entry-contains
  (lambda (web::context)
    (define filemap/2 (context-filemap/2 web::context))

    (profun-op-lambda
     (ctx (vid file)
          (vid-name file-name))

     (if ctx (ctx)
         (cond
          ((and (profun-bound-value? vid)
                (not (string? vid)))
           (make-profun-error 'vid-must-be-a-string vid))

          ((and (profun-bound-value? vid)
                (profun-unbound-value? file))
           (let ()
             (define info (filemap-ref-by-senderid filemap/2 vid #f))
             (define dir (and info (sharedinfo-sourcepath info)))
             (define iter0
               (and dir (directory-files-depth-iter #t 1 dir)))
             (define (iter1)
               (define x (iter0))
               (and x (car x)))
             (define (iter)
               (define full (iter1))
               (define full-entry (and full (standalone-file->entry/prefixed dir vid full)))
               (define entry (and full (list (assq keyword-target full-entry))))
               (if full
                   (profun-set-meta
                    (file-name <- full-entry)
                    (profun-set (file-name <- entry)))
                   (profun-reject)))

             (cond
              ((not info)
               (make-profun-error 'bad-vid vid))
              ((not iter0)
               (make-profun-error 'shared-entry-is-not-a-directory vid))

              (else
               (let ((val (iter)))
                 (if (profun-accept? val)
                     (profun-ctx-set iter val)
                     val))))))

          ((profun-unbound-value? 0)
           (make-profun-error 'vid-must-be-given))
          ((profun-bound-value? file)
           (make-profun-error 'file-must-be-a-variable file))
          (else
           (make-profun-error 'entry-must-be-given-and-file-must-be-a-variable)))))))
