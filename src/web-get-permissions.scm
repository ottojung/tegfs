;;;; Copyright (C) 2022  Otto Jung
;;;;
;;;; This program is free software: you can redistribute it and/or modify
;;;; it under the terms of the GNU General Public License as published by
;;;; the Free Software Foundation; version 3 of the License.
;;;;
;;;; This program is distributed in the hope that it will be useful,
;;;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;;; GNU General Public License for more details.
;;;;
;;;; You should have received a copy of the GNU General Public License
;;;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

%run guile

%var web-get-permissions

%use (web-callcontext/p) "./web-callcontext-p.scm"
%use (callcontext-ctr callcontext? callcontext-break callcontext-request callcontext-query callcontext-body callcontext-time callcontext-key set-callcontext-key! callcontext-permissions) "./web-callcontext.scm"

(define (web-get-permissions)
  (define callctx (web-callcontext/p))
  (define f (callcontext-permissions callctx))
  (f))