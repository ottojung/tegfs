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
  (define-module (tegfs get-texteditor)
    :export (get-texteditor get-texteditor/default)
    :use-module ((euphrates memconst) :select (memconst))
    :use-module ((euphrates system-environment) :select (system-environment-get))
    :use-module ((tegfs texteditor-p) :select (texteditor/p))
    )))

(define TEXTEDITOR_VAR_NAME "EDITOR")

(define get-texteditor/env
  (memconst
   (or (system-environment-get TEXTEDITOR_VAR_NAME) "vi")))

(define get-texteditor/default get-texteditor/env)

(define (get-texteditor)
  (or (texteditor/p)
      (get-texteditor/default)))
