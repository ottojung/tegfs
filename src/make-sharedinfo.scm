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

%var make-sharedinfo

%use (get-random-network-name) "./get-random-network-name.scm"
%use (sharedinfo-ctr) "./sharedinfo.scm"
%use (web-callcontext/p) "./web-callcontext-p.scm"
%use (callcontext-time) "./web-callcontext.scm"

(define (make-sharedinfo target-fullpath for-duration)
  (define callctx (web-callcontext/p))
  (define now (callcontext-time callctx))
  (define recepientid (get-random-network-name))
  (define senderid (get-random-network-name))
  (sharedinfo-ctr target-fullpath recepientid senderid now for-duration))
