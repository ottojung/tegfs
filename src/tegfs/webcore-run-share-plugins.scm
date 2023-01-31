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
  (define-module (tegfs webcore-run-share-plugins)
    :export (webcore::run-share-plugins)
    :use-module ((euphrates list-map-first) :select (list-map-first))
    :use-module ((tegfs plugin) :select (plugin-function))
    )))



(define (webcore::run-share-plugins config root plugins entry generic-fullpath/0)
  (or
   (list-map-first
    (lambda (plugin)
      (define fun (plugin-function plugin))
      (fun config root entry generic-fullpath/0))
    #f plugins)
   generic-fullpath/0))
