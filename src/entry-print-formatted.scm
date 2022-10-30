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

%var entry-print/formatted

%use (read-list) "./euphrates/read-list.scm"
%use (entry-target-fullpath) "./entry-target-fullpath.scm"
%use (get-preview-path) "./get-preview-path.scm"
%use (keyword-id) "./keyword-id.scm"

(define (entry-print/formatted <query-format> entry)
  (define format-elements
    (with-input-from-string <query-format>
      (lambda _
        (read-list))))

  (for-each
   (lambda (element)
     (cond
      ((equal? '%F element) ;; target-fullpath
       (let ((fullpath (entry-target-fullpath entry)))
         (display (or fullpath "//NA//"))))
      ((equal? '%P element) ;; preview-fullpath
       (let* ((target-fullpath (entry-target-fullpath entry))
              (id/p (assoc keyword-id entry))
              (preview (and id/p target-fullpath
                            (get-preview-path (cdr id/p) target-fullpath))))
         (display (or preview "//NA//"))))
      ((symbol? element)
       (let ((p (assoc element entry)))
         (if p
             (display (cdr p))
             (display "//NA//"))))
      (else (display element))))
   format-elements))
