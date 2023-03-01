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
  (define-module (tegfs config-check-value-format)
    :export (config-check-value-format)
    :use-module ((euphrates stringf) :select (stringf))
    :use-module ((euphrates tilda-a) :select (~a))
    :use-module ((tegfs fatal) :select (fatal))
    )))

(define (config-check-value-format name p)
  (when p
    (unless (and (list? p) (= 2 (length p)))
      (fatal "Bad config format~a"
             (if name
                 (stringf ", field ~s is the issue" (~a name))
                 "")))))
