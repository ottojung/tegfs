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
  (define-module (tegfs get-save-plugins)
    :export (get-save-plugins)
    :use-module ((euphrates memconst) :select (memconst))
    :use-module ((tegfs get-save-plugins-files) :select (get-save-plugins-files))
    :use-module ((tegfs load-plugin) :select (load-plugin)))))



(define get-save-plugins
  (memconst
   (let ((files (get-save-plugins-files)))
     (map load-plugin files))))
