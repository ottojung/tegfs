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

%var filemap-make/empty
%var filemap-set!
%var filemap-ref-by-vid
%var filemap-ref-by-recepientid
%var filemap-delete-by-vid!
%var filemap-delete-by-recepientid!

%use (hashmap-delete! hashmap-ref hashmap-set! make-hashmap) "./euphrates/ihashmap.scm"
%use (sharedinfo-recepientid sharedinfo-vid) "./sharedinfo.scm"

(define (filemap-make/empty)
  (cons (make-hashmap) (make-hashmap)))

(define (filemap-set! filemap/2 info)
  (define vid (sharedinfo-vid info))
  (define recepientid (sharedinfo-recepientid info))
  (define first (car filemap/2))
  (define second (cdr filemap/2))
  (hashmap-set! first vid info)
  (hashmap-set! second recepientid info))

(define (filemap-ref-by-vid filemap/2 id default)
  (define first (car filemap/2))
  (hashmap-ref first id default))

(define (filemap-ref-by-recepientid filemap/2 recepientid default)
  (define second (cdr filemap/2))
  (hashmap-ref second recepientid default))

(define (filemap-delete-by-vid! filemap/2 id)
  (define first (car filemap/2))
  (define second (cdr filemap/2))
  (define info (filemap-ref-by-vid filemap/2 id #f))
  (when info
    (let ((recepientid (sharedinfo-recepientid info)))
      (hashmap-delete! first id)
      (hashmap-delete! second recepientid))))

(define (filemap-delete-by-recepientid! filemap/2 recepientid)
  (define first (car filemap/2))
  (define second (cdr filemap/2))
  (define info (filemap-ref-by-recepientid filemap/2 recepientid #f))
  (when info
    (let ((id (sharedinfo-vid info)))
      (hashmap-delete! first id)
      (hashmap-delete! second recepientid))))
