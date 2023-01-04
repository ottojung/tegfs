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

%var web-collectgarbage

%use (profune-communicator-handle) "./euphrates/profune-communicator.scm"
%use (web-basic-headers) "./web-basic-headers.scm"
%use (web-context/p) "./web-context-p.scm"
%use (web-make-communicator) "./web-make-communicator.scm"
%use (web-return!) "./web-return-bang.scm"

%for (COMPILER "guile")
(use-modules (web response))
%end

(define (web-collectgarbage)
  (profune-communicator-handle
   (web-make-communicator (web-context/p))
   `(whats (collectgarbage)))

  (web-return!
   (build-response
    #:code 200
    #:headers
    (append web-basic-headers
            `((Cache-Control . "no-cache"))))
   "ok\n"))
