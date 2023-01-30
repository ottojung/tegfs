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

(cond-expand
 (guile
  (define-module (tegfs webcore-shared-entry-contains)
    :export (webcore::shared-entry-contains)
    :use-module ((euphrates directory-files-depth-iter) :select (directory-files-depth-iter))
    :use-module ((euphrates file-is-directory-q-no-readlink) :select (file-is-directory?/no-readlink))
    :use-module ((euphrates profun-accept) :select (profun-accept? profun-ctx-set profun-set profun-set-meta))
    :use-module ((euphrates profun-error) :select (make-profun-error))
    :use-module ((euphrates profun-op-lambda) :select (profun-op-lambda))
    :use-module ((euphrates profun-reject) :select (profun-reject))
    :use-module ((euphrates profun-value) :select (profun-bound-value? profun-unbound-value?))
    :use-module ((tegfs filemap) :select (filemap-ref-by-senderid))
    :use-module ((tegfs keyword-mimetype) :select (keyword-mimetype))
    :use-module ((tegfs keyword-target) :select (keyword-target))
    :use-module ((tegfs sharedinfo) :select (sharedinfo-sourcepath))
    :use-module ((tegfs standalone-file-to-entry) :select (standalone-file->entry/prefixed))
    :use-module ((tegfs web-context) :select (context-filemap/2)))))



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
               (define full-entry/0 (and full (standalone-file->entry/prefixed dir vid full)))
               (define full-entry
                 (and full-entry/0
                      (if (file-is-directory?/no-readlink full)
                          (cons (cons keyword-mimetype "inode/directory") full-entry/0)
                          full-entry/0)))
               (define entry
                 (and full
                      (filter (lambda (p) (or (eq? (car p) keyword-target)
                                              (eq? (car p) keyword-mimetype)))
                              full-entry)))
               (if full
                   (profun-set-meta
                    (file-name <- full-entry)
                    (profun-set (file-name <- entry)))
                   (profun-reject)))

             (cond
              ((not info)
               (make-profun-error 'not-found "Bad vid" vid))
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
