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

%var web-query-monad

%use (monad-make/hook) "./euphrates/monad-make-hook.scm"
%use (web-get-filemap/2) "./web-get-filemap-2.scm"
%use (web-get-permissions) "./web-get-permissions.scm"

(define (web-query-monad handler query/split)
  (monad-make/hook
   (lambda (tags)
     (cond
      ((memq 'unfold-entry tags) (lambda (f) (f)))
      ((memq 'entry tags) handler)
      ;; ((memq 'share-preview tags)
      ;;  (lambda (info)
      ;;    (debugv (serialize/human info))))
      ((memq 'ask tags)
       (lambda (type)
         (case type
           ((query/split) query/split)
           ((permissions) (web-get-permissions))
           ((filemap/2) (web-get-filemap/2))
           ((diropen?) #t)
           ((dirpreview?) #f))))))))

