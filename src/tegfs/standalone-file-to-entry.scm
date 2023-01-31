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
  (define-module (tegfs standalone-file-to-entry)
    :export (standalone-file->entry standalone-file->entry/prefixed)
    :use-module ((euphrates append-posix-path) :select (append-posix-path))
    :use-module ((euphrates path-get-basename) :select (path-get-basename))
    :use-module ((euphrates path-normalize) :select (path-normalize))
    :use-module ((tegfs keyword-entry-parent-directory-senderid) :select (keyword-entry-parent-directory-senderid))
    :use-module ((tegfs keyword-entry-parent-directory) :select (keyword-entry-parent-directory))
    :use-module ((tegfs keyword-id) :select (keyword-id))
    :use-module ((tegfs keyword-target) :select (keyword-target))
    )))



(define (standalone-file->entry filepath)
  (define norm0 (path-normalize filepath))
  (define norm (if (string-prefix? "/" norm0)
                   norm0
                   (string-append "/" norm0)))
  ;; (define dir (dirname norm))
  (define name (path-get-basename norm))
  (define id norm)
  `((,keyword-id . ,id)
    (,keyword-target . ,name)
    ;; (,keyword-entry-parent-directory . ,dir)
    ))

(define (standalone-file->entry/prefixed prefix vid filepath)
  (define norm0 (path-normalize filepath))
  (define norm (if (string-prefix? "/" norm0)
                   norm0
                   (string-append "/" norm0)))
  (define dir0 prefix)
  (define dir (if (string-prefix? "/" dir0)
                  dir0
                  (string-append "/" dir0)))
  (define name (path-get-basename norm))
  (define id
    (path-normalize (append-posix-path dir norm)))
  `((,keyword-id . ,id)
    (,keyword-target . ,name)
    (,keyword-entry-parent-directory . ,dir)
    (,keyword-entry-parent-directory-senderid . ,vid)
    ))
