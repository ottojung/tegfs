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

%var webcore::get-share-plugins-files

%use (directory-files) "./euphrates/directory-files.scm"
%use (webcore::get-share-plugins-directory) "./webcore-get-share-plugins-directory.scm"

(define (webcore::get-share-plugins-files)
  (define dir (webcore::get-share-plugins-directory))
  (map car (directory-files dir)))
