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

%var filemap-set!
%var filemap-ref-by-vid
%var filemap-ref-by-sharedname
%var filemap-delete-by-vid!
%var filemap-delete-by-sharedname!
%var get-current-filemap/2

%use (hashmap-delete! hashmap-ref hashmap-set!) "./euphrates/ihashmap.scm"
%use (web-context/p) "./web-context-p.scm"
%use (context-filemap/2) "./web-context.scm"
%use (sharedinfo-sharedname sharedinfo-vid) "./web-sharedinfo.scm"

(define (filemap-set! filemap/2 info)
  (define vid (sharedinfo-vid info))
  (define sharedname (sharedinfo-sharedname info))
  (define first (car filemap/2))
  (define second (cdr filemap/2))
  (hashmap-set! first vid info)
  (hashmap-set! second sharedname info))

(define (filemap-ref-by-vid filemap/2 id default)
  (define first (car filemap/2))
  (hashmap-ref first id default))

(define (filemap-ref-by-sharedname filemap/2 sharedname default)
  (define second (cdr filemap/2))
  (hashmap-ref second sharedname default))

(define (filemap-delete-by-vid! filemap/2 id)
  (define first (car filemap/2))
  (define second (cdr filemap/2))
  (define info (filemap-ref-by-vid filemap/2 id #f))
  (when info
    (let ((sharedname (sharedinfo-sharedname info)))
      (hashmap-delete! first id)
      (hashmap-delete! second sharedname))))

(define (filemap-delete-by-sharedname! filemap/2 sharedname)
  (define first (car filemap/2))
  (define second (cdr filemap/2))
  (define info (filemap-ref-by-sharedname filemap/2 sharedname #f))
  (when info
    (let ((id (sharedinfo-vid info)))
      (hashmap-delete! first id)
      (hashmap-delete! second sharedname))))

(define (get-current-filemap/2)
  (define ctx (web-context/p))
  (context-filemap/2 ctx))
