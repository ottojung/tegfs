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

%var web-request-get-domainname

%use (~a) "./euphrates/tilda-a.scm"

%for (COMPILER "guile")

(use-modules (web request))

%end

(define (web-request-get-domainname req)
  (define headers (request-headers req))
  (define host/get (assoc 'host headers))
  (and host/get
       (let* ((host/pair (cdr host/get))
              (host
               (if (pair? host/pair)
                   (if (cdr host/pair)
                       (string-append (~a (car host/pair)) ":" (~a (cdr host/pair)))
                       (~a (car host/pair)))
                   host/pair)))
         (string-append "http://" host))))
