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

%var web-query-monad

%use (monad-make/hook) "./euphrates/monad-make-hook.scm"
%use (web-get-filemap/2) "./web-get-filemap-2.scm"
%use (web-get-permissions) "./web-get-permissions.scm"

(define (web-query-monad handler query/split)
  (monad-make/hook
   (lambda (tags args)
     (cond
      ((memq 'entry tags) (handler (car args)))
      ((memq 'ask tags)
       (case (car args)
         ((query/split) query/split)
         ((permissions) (web-get-permissions))
         ((filemap/2) (web-get-filemap/2))
         ((diropen?) #t)
         ((dirpreview?) #f)))))))
