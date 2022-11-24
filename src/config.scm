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

%var tegfs-config/parse

%use (fatal) "./fatal.scm"
%use (get-config) "./get-config.scm"
%use (set-config) "./set-config.scm"

(define (tegfs-config/parse get set <name> <value>)
  (define config (get-config))

  (unless config
    (fatal "Could not parse the config"))

  (cond
   (get
    (let ((p (assoc (string->symbol <name>) config)))
      (if p
          (display (cadr p))
          (display "Not set\n" (current-error-port)))))
   (set
    (set-config (string->symbol <name>) (list <value>))
    (display "Ok\n" (current-error-port)))))
