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
  (define-module (tegfs cli-edit)
    :export (CLI::edit)
    :use-module ((euphrates system-environment) :select (system-environment-get))
    :use-module ((tegfs entry-target-fullpath) :select (entry-target-fullpath))
    :use-module ((tegfs fatal) :select (fatal))
    :use-module ((tegfs get) :select (tegfs-get))
    )
  (use-modules (ice-9 binary-ports))
  (use-modules (scheme base))
  ))

(define (CLI::edit <entry-id>)
  (define entry (tegfs-get <entry-id>))
  (unless entry
    (fatal "Entry with id ~s not found" <entry-id>))

  (let ()
    (define fullpath (entry-target-fullpath entry))
    (define EDITOR (or (system-environment-get "EDITOR") "vi"))
    (if fullpath
        (system* EDITOR fullpath)
        (fatal "Entry is not associated with any file"))))
