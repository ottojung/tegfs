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
  (define-module (tegfs web-user-loged-in-q)
    :export (web::user-loged-in?)
    :use-module ((tegfs web-callcontext) :select (callcontext-token))
    :use-module ((tegfs webcore-ask) :select (webcore::ask))
    )))

(define (web::user-loged-in? callctx)
  (define token (callcontext-token callctx))
  (define result
    (webcore::ask `(whats (time-left ,token _TL))))
  (define loged-in?
    (equal? 'its (car result)))

  loged-in?)
