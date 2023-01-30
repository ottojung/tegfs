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
  (define-module (tegfs webcore-get-share-plugins)
    :export (webcore::get-share-plugins)
    :use-module ((euphrates memconst) :select (memconst))
    :use-module ((tegfs load-plugin) :select (load-plugin))
    :use-module ((tegfs webcore-get-share-plugins-files) :select (webcore::get-share-plugins-files)))))



(define webcore::get-share-plugins
  (memconst
   (let ((files (webcore::get-share-plugins-files)))
     (map load-plugin files))))
