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

%run guile

%var web-query/foreach

%use (assq-or) "./euphrates/assq-or.scm"
%use (profune-communicator-handle) "./euphrates/profune-communicator.scm"
%use (raisu) "./euphrates/raisu.scm"
%use (web-context/p) "./web-context-p.scm"
%use (web-get-filemap/2) "./web-get-filemap-2.scm"
%use (web-get-permissions) "./web-get-permissions.scm"
%use (web-make-communicator) "./web-make-communicator.scm"

(define (web-query/foreach query/split for-each-fn)
  (define result
    (profune-communicator-handle
     (web-make-communicator (web-context/p))
     `(whats
       (permissions ,(web-get-permissions))
       (filemap/2 ,(web-get-filemap/2))
       (query ,query/split)
       (entry E)
       more (99999)
       )))

  (define equals (cadr (cadr result)))
  (for-each
   (lambda (bindings)
     (define entry
       (assq-or 'E bindings (raisu 'unexpected-result-from-backend bindings)))
     (for-each-fn entry))
   equals))
