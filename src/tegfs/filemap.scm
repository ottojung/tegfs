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
  (define-module (tegfs filemap)
    :export (filemap-set! filemap-ref-by-senderid filemap-ref-by-recepientid filemap-delete-by-senderid! filemap-delete-by-recepientid!)
    :use-module ((euphrates define-type9) :select (define-type9))
    :use-module ((euphrates hashmap) :select (hashmap-delete! hashmap-ref hashmap-set! make-hashmap))
    :use-module ((tegfs sharedinfo) :select (sharedinfo-recepientid sharedinfo-senderid sharedinfo?))
    )))


(define-type9 <recepient>
  (recepient-constructor id senderid) recepient?
  (id recepient-id)
  (senderid recepient-senderid)
  )

(define (filemap-set! filemap/2 info)
  (define vid (sharedinfo-senderid info))
  (define recepientid (sharedinfo-recepientid info))
  (define recepient (recepient-constructor recepientid vid))
  (hashmap-set! filemap/2 vid info)
  (hashmap-set! filemap/2 recepientid recepient))

(define (filemap-ref-by-senderid filemap/2 vid default)
  (define r (hashmap-ref filemap/2 vid #f))
  (if (sharedinfo? r) r default))

(define (filemap-ref-by-recepientid filemap/2 recepientid default)
  (define r (hashmap-ref filemap/2 recepientid #f))
  (if (recepient? r)
      (filemap-ref-by-senderid filemap/2 (recepient-id r) default)
      default))

(define (filemap-delete-by-senderid! filemap/2 vid)
  (define info (filemap-ref-by-senderid filemap/2 vid #f))
  (when info
    (let ((recepientid (sharedinfo-recepientid info)))
      (hashmap-delete! filemap/2 vid)
      (hashmap-delete! filemap/2 recepientid))))

(define (filemap-delete-by-recepientid! filemap/2 recepientid)
  (define info (filemap-ref-by-recepientid filemap/2 recepientid #f))
  (when info
    (let ((vid (sharedinfo-senderid info)))
      (hashmap-delete! filemap/2 vid)
      (hashmap-delete! filemap/2 recepientid))))
