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
%use (append-posix-path) "./euphrates/append-posix-path.scm"

%use (root/p) "./root-p.scm"
%use (entry-registry-path-key) "./entry-registry-path-key.scm"

(define (entry-print/formatted <query-format> entry)
  (define format-elements
    (with-input-from-string <query-format>
      (lambda _
        (read-list))))

  (for-each
   (lambda (element)
     (cond
      ((equal? '%F element)
       (let ((target/p (assoc 'target entry)))
         (if target/p
             (let* ((target (cdr target/p))
                    (registry-path (cdr (assoc entry-registry-path-key entry)))
                    (target-fullpath
                     (append-posix-path
                      (root/p) (dirname registry-path) target)))
               (display target-fullpath))
             (display "//NA//"))))
      ((symbol? element)
       (let ((p (assoc element entry)))
         (if p
             (display (cdr p))
             (display "//NA//"))))
      (else (display element))))
   format-elements))
