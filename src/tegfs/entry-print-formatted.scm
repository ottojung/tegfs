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
  (define-module (tegfs entry-print-formatted)
    :export (entry-print/formatted)
    :use-module ((euphrates read-list) :select (read-list))
    :use-module ((euphrates words-to-string) :select (words->string))
    :use-module ((tegfs entry-get-tags) :select (entry-get-tags))
    :use-module ((tegfs entry-target-fullpath) :select (entry-target-fullpath))
    :use-module ((tegfs get-preview-path) :select (get-preview-path))
    :use-module ((tegfs keyword-tags) :select (keyword-tags))
    :use-module ((tegfs unparse-tag) :select (unparse-tag))
    )))



(define (entry-print/formatted/full <query-format> entry port)
  (define format-elements
    (with-input-from-string <query-format>
      (lambda _
        (read-list))))

  (for-each
   (lambda (element)
     (cond
      ((equal? '\n element) ;; used as default separator
       (newline port))
      ((equal? '%F element) ;; target-fullpath
       (let ((fullpath (entry-target-fullpath entry)))
         (display (or fullpath "//NA//") port)))
      ((equal? '%P element) ;; preview-fullpath
       (let* ((target-fullpath (entry-target-fullpath entry))
              (preview (and target-fullpath
                            (get-preview-path target-fullpath))))
         (display (or preview "//NA//") port)))
      ((equal? keyword-tags element)
       (let ((tags (entry-get-tags entry)))
         (display
          (or (and tags (words->string (map unparse-tag tags))) "//NA//")
          port)))
      ((symbol? element)
       (let ((p (assoc element entry)))
         (if p
             (display (cdr p) port)
             (display "//NA//" port))))
      (else (display element port))))
   format-elements))

(define entry-print/formatted
  (case-lambda
   ((<query-format> entry)
    (entry-print/formatted/full <query-format> entry (current-output-port)))
   ((<query-format> entry port)
    (entry-print/formatted/full <query-format> entry port))))
