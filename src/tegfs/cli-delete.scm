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
  (define-module (tegfs cli-delete)
    :export (CLI::delete)
    :use-module ((euphrates debugs) :select (debugs))
    :use-module ((euphrates file-delete) :select (file-delete))
    :use-module ((euphrates profune-communicator) :select (profune-communicator-handle))
    :use-module ((euphrates raisu) :select (raisu))
    :use-module ((tegfs entry-target-fullpath) :select (entry-target-fullpath))
    :use-module ((tegfs fatal) :select (fatal))
    :use-module ((tegfs tegfs-make-communicator) :select (tegfs-make-communicator))
    :use-module ((tegfs update-entry) :select (update-entry))
    )))

(define (CLI::delete <entry-id> --keep-files)
  (define result
    (profune-communicator-handle
     (tegfs-make-communicator)
     `(whats
       (entry E)
       (entry-field E "id" ,<entry-id>)
       (entry-field E "id" ,<entry-id>)
       (update-entry E #f))))

  (cond
   ((equal? 'error (car result))
    (fatal "Error: ~s" (cdr result)))
   ((equal? 'its (car result))
    (when (equal? '(false) (cadr result))
      (fatal "Entry with such id not found"))

    (unless --keep-files
      (let ()
        (debugs (cadr result))
        (define E (cddr (cadr result)))
        (debugs E)
        (define fullpath (entry-target-fullpath E))
        (when fullpath
          (file-delete fullpath)))))
   (else
    (raisu 'unexpected-result-from-backend-4324 result))))
