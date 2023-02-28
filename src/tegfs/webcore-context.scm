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
  (define-module (tegfs webcore-context)
    :export (context-ctr context? context-users context-database context-tempentries context-port context-sharedir context-filemap/2)
    :use-module ((euphrates define-type9) :select (define-type9))
    )))

(define-type9 <context>
  (context-ctr users tempentries port sharedir) context?
  (users context-users) ;; permanent user accounts
  (tempentries context-tempentries) ;; temporary entries. hold things like session tokens and shared files infos
  (port context-port) ;; port to host the server on
  (sharedir context-sharedir) ;; directory with shared wiles
  )

(define (context-filemap/2 ctx)
  (context-tempentries ctx))
