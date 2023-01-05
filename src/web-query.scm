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

%var web-query

%use (hashmap-ref) "./euphrates/hashmap.scm"
%use (string->words) "./euphrates/string-to-words.scm"
%use (default-full-sharing-time) "./default-full-sharing-time.scm"
%use (default-preview-sharing-time) "./default-preview-sharing-time.scm"
%use (web-callcontext/p) "./web-callcontext-p.scm"
%use (callcontext-token) "./web-callcontext.scm"
%use (web-decode-query) "./web-decode-query.scm"
%use (web-get-query) "./web-get-query.scm"
%use (web-handle-profun-results) "./web-handle-profun-results.scm"
%use (web-query-display-results) "./web-query-display-results.scm"
%use (webcore::ask) "./webcore-ask.scm"

(define (web-query)
  (define callctx (web-callcontext/p))
  (define ctxq (web-get-query))

  (define query/encoded (hashmap-ref ctxq 'q ""))
  (define query (web-decode-query query/encoded))
  (define query/split (string->words query))

  (define result
    (webcore::ask
     `(whats
       (key ,(callcontext-token callctx))
       (query ,query/split)
       (entry E)
       (share-preview E ,default-preview-sharing-time _ATP _P)
       (share-full E ,default-full-sharing-time _ATF F)
       (link-shared _P PL)
       more (99999)
       )))

  (web-handle-profun-results result web-query-handle-results))

(define (web-query-handle-results equals)
  (web-query-display-results equals))
