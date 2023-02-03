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
  (define-module (tegfs custom-tempentry)
    :export (custom-tempentry custom-tempentry-constructor custom-tempentry? custom-tempentry-id custom-tempentry-date set-custom-tempentry-date! custom-tempentry-stime set-custom-tempentry-stime! custom-tempentry-fields set-custom-tempentry-fields!)
    :use-module ((euphrates define-type9) :select (define-type9))
    )))

(define-type9 <custom-tempentry>
  (custom-tempentry-constructor id date stime fields) custom-tempentry?
  (id custom-tempentry-id)
  (date custom-tempentry-date set-custom-tempentry-date!)
  (stime custom-tempentry-stime set-custom-tempentry-stime!)
  (fields custom-tempentry-fields set-custom-tempentry-fields!)
  )
