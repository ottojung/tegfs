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

%var web::basic-headers

(define web::basic-headers
  '((referrer-policy . "strict-origin-when-cross-origin") ;; optional, ensures not to send too much user data.
    (x-frame-options . "DENY") ;; optional, bans embedding in <iframe> and such.
    (strict-transport-security . "max-age=63072000; includeSubDomains; preload") ;; something something security.
    ;; TODO: add more SECURITY!!!!
    ))
