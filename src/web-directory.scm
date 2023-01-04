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

%var web-directory

%use (hashmap-ref) "./euphrates/hashmap.scm"
%use (profune-communicator-handle) "./euphrates/profune-communicator.scm"
%use (default-full-sharing-time) "./default-full-sharing-time.scm"
%use (default-preview-sharing-time) "./default-preview-sharing-time.scm"
%use (web-bad-request) "./web-bad-request.scm"
%use (web-callcontext/p) "./web-callcontext-p.scm"
%use (callcontext-token) "./web-callcontext.scm"
%use (web-context/p) "./web-context-p.scm"
%use (web-get-query) "./web-get-query.scm"
%use (web-handle-profun-results) "./web-handle-profun-results.scm"
%use (web-make-communicator) "./web-make-communicator.scm"
%use (web-query-display-results) "./web-query-display-results.scm"

(define (web-directory)
  (define callctx (web-callcontext/p))
  (define ctxq (web-get-query))

  (define vid
    (or (hashmap-ref ctxq 'vid #f)
        (web-bad-request "Request query missing requiered 'd' argument")))

  (define result
    (profune-communicator-handle
     (web-make-communicator (web-context/p))
     `(whats
       (key ,(callcontext-token callctx))
       (shared-entry-contains ,vid E)
       (share-preview E ,default-preview-sharing-time _P)
       (share-full E ,default-full-sharing-time F)
       (link-shared _P PL)
       more (99999)
       )))

  (web-handle-profun-results result web-directory-handle))

(define (web-directory-handle equals)
  (web-query-display-results equals))
