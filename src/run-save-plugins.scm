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

%run guile

%var run-save-plugins

%use (alist-initialize!:return-multiple) "./euphrates/alist-initialize-bang.scm"
%use (list-fold) "./euphrates/list-fold.scm"
%use (plugin-function) "./plugin.scm"

(define (run-save-plugins config root current-alist plugins)
  (alist-initialize!:return-multiple
   (list-fold
    (ret '())
    (cur plugins)
    (begin
      (define fun (plugin-function cur))
      (append (fun config root current-alist) ret)))))
