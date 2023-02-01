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
  (define-module (tegfs webcore-context)
    :export (context-ctr context? context-passwords context-database context-tokens context-port context-fileserver context-sharedir context-filemap/2)
    :use-module ((euphrates define-type9) :select (define-type9))
    )))



(define-type9 <context>
  (context-ctr passwords tokens port fileserver sharedir filemap/2) context?
  (passwords context-passwords) ;; user credentials passwords
  (tokens context-tokens) ;; temporary session tokens
  (port context-port) ;; port to host the server on
  (fileserver context-fileserver) ;; full URI of the file server
  (sharedir context-sharedir) ;; directory with shared wiles
  (filemap/2 context-filemap/2) ;; cons of hashmaps of type [: vid -> info] and [: recepientid -> info]
  )
