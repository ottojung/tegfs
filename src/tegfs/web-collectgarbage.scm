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

(cond-expand
 (guile
  (define-module (tegfs web-collectgarbage)
    :export (web::collectgarbage)
    :use-module ((tegfs web-collect-own-garbage) :select (web::collect-own-garbage))
    :use-module ((tegfs web-return) :select (web::return))
    :use-module ((tegfs webcore-ask) :select (webcore::ask))
    )))



(define (web::collectgarbage)
  (webcore::ask `(whats (collectgarbage)))
  (web::collect-own-garbage)

  (web::return
   200
   `((Cache-Control . "no-cache"))
   "ok\n"))
