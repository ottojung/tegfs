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

%run guile

%var web::previewunknownurl

%use (web::define-static-file) "./web-define-static-file.scm"
%use (web::preview-height) "./web-preview-height.scm"
%use (web::preview-width) "./web-preview-width.scm"
%use (web::url-icon/svg) "./web-url-icon-svg.scm"

(define unknownurl-image-string
  (web::url-icon/svg web::preview-width web::preview-height))

(web::define-static-file web::previewunknownurl
  '(image/svg+xml) unknownurl-image-string)
