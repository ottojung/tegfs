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

%var web-full

%use (hashmap-ref) "./euphrates/hashmap.scm"
%use (profune-communicator-handle) "./euphrates/profune-communicator.scm"
%use (~s) "./euphrates/tilda-s.scm"
%use (words->string) "./euphrates/words-to-string.scm"
%use (web-bad-request) "./web-bad-request.scm"
%use (web-basic-headers) "./web-basic-headers.scm"
%use (web-context/p) "./web-context-p.scm"
%use (web-get-query) "./web-get-query.scm"
%use (web-make-communicator) "./web-make-communicator.scm"
%use (web-return!) "./web-return-bang.scm"

%for (COMPILER "guile")

(use-modules (web response))

%end

(define (web-full)
  (define ctxq (web-get-query))
  (define senderid (hashmap-ref ctxq 'vid #f))

  (define result
    (profune-communicator-handle
     (web-make-communicator (web-context/p))
     `(whats
       (link-shared ,senderid L)
       )))

  (cond
   ((equal? 'error (car result))
    (web-bad-request "error: ~a" (words->string (map ~s (cadr result)))))
   ((and (equal? 'its (car result))
         (equal? '= (car (cadr result))))
    (let* ((word (cadr result))
           (location (list-ref word 2)))
      (web-return!
       (build-response
        #:code 301
        #:headers
        (append web-basic-headers
                `((Location . ,location)
                  (Cache-Control . "no-cache"))))
       #f)))
   (else
    (web-bad-request "internal error"))))
