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

%var web-decode-query

%use (appcomp) "./euphrates/comp.scm"
%use (web-try-uri-decode) "./web-try-uri-decode.scm"

(define (web-decode-query query/encoded)
  (appcomp query/encoded
           web-try-uri-decode
           string->list
           (map (lambda (c) (if (equal? #\+ c) #\space c)))
           (map (lambda (c) (if (equal? #\: c) #\= c)))
           list->string))
