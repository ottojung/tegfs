;;;; Copyright (C) 2023  Otto Jung
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
  (define-module (tegfs update-entry)
    :export (update-entry)
    :use-module ((euphrates assq-or) :select (assq-or))
    :use-module ((euphrates file-delete) :select (file-delete))
    :use-module ((euphrates raisu) :select (raisu))
    :use-module ((tegfs entries-map-bang) :select (entries-map!))
    :use-module ((tegfs entry-target-fullpath) :select (entry-target-fullpath))
    :use-module ((tegfs keyword-entry-registry-path) :select (keyword-entry-registry-path))
    :use-module ((tegfs keyword-id) :select (keyword-id)))))



(define (update-entry::continue id updated-entry)
  (define updated-target
    (and updated-entry
         (entry-target-fullpath updated-entry)))

  (define (update registry-path iterated-entry)
    (define registry-property
      (cons keyword-entry-registry-path registry-path))
    (define original-target
      (entry-target-fullpath
       (cons registry-property iterated-entry)))
    (unless (equal? original-target updated-target)
      (if (and updated-target
               (not (string-null? updated-target)))
          (rename-file original-target updated-target)
          (file-delete original-target)))
    updated-entry)

  (define (mapper registry-path iterated-entry)
    (define iterated-id
      (assq-or keyword-id iterated-entry #f))
    (if (equal? id iterated-id)
        (update registry-path iterated-entry)
        iterated-entry))

  (entries-map! mapper))

(define (update-entry original-entry updated-entry)
  (define id
    (assq-or keyword-id original-entry #f))
  (if id
      (update-entry::continue id updated-entry)
      (raisu 'type-error:original-entry-does-not-have-an-id)))
