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
  (define-module (tegfs run-save-plugins)
    :export (run-save-plugins)
    :use-module ((euphrates alist-initialize-bang) :select (alist-initialize!:return-multiple))
    :use-module ((euphrates list-fold) :select (list-fold))
    :use-module ((tegfs plugin) :select (plugin-function))
    )))



(define (run-save-plugins root current-alist plugins)
  (alist-initialize!:return-multiple
   (list-fold
    (ret '())
    (cur plugins)
    (begin
      (define fun (plugin-function cur))
      (append (fun root current-alist) ret)))))
