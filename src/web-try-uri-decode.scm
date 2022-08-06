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

%var web-try-uri-decode

%use (catch-any) "./euphrates/catch-any.scm"
%use (raisu) "./euphrates/raisu.scm"

%for (COMPILER "guile")

(use-modules (web uri))

%end

(define (web-try-uri-decode url)
  (cond
   ((string? url)
    (catch-any
     (lambda _ (uri-decode url))
     (lambda _ url)))
   (else
    (raisu 'url-is-not-a-string url))))
