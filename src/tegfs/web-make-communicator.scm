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

%var web::make-communicator

%use (profun-create-database) "./euphrates/profun.scm"
%use (make-profune-communicator) "./euphrates/profune-communicator.scm"
%use (web::make-context) "./web-make-context.scm"
%use (webcore::make-server-handler) "./webcore-server-handler.scm"

(define (web::make-communicator)
  (define webcore::context
    (web::make-context))

  (define-values (handler server-operator-key)
    (webcore::make-server-handler webcore::context))

  (define db
    (profun-create-database handler '()))

  (define comm
    (make-profune-communicator db))

  (values comm server-operator-key))
