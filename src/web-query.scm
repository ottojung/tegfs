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

%var web-query

%use (hashmap-ref) "./euphrates/ihashmap.scm"
%use (string->words) "./euphrates/string-to-words.scm"
%use (with-monad) "./euphrates/with-monad.scm"
%use (tegfs-query) "./tegfs-query.scm"
%use (web-callcontext/p) "./web-callcontext-p.scm"
%use (callcontext-request) "./web-callcontext.scm"
%use (web-context/p) "./web-context-p.scm"
%use (web-decode-query) "./web-decode-query.scm"
%use (web-display-entries) "./web-display-entries.scm"
%use (web-display-entry) "./web-display-entry.scm"
%use (web-get-query) "./web-get-query.scm"
%use (web-query-monad) "./web-query-monad.scm"
%use (web-respond) "./web-respond.scm"

(define (web-query)
  (define ctx (web-context/p))
  (define callctx (web-callcontext/p))
  (define request (callcontext-request callctx))
  (define ctxq (web-get-query))

  (define query/encoded (hashmap-ref ctxq 'q ""))
  (define query (web-decode-query query/encoded))
  (define query/split (string->words query))

  (web-respond
   (lambda _
     (web-display-entries
      (lambda _
        (with-monad
         (web-query-monad web-display-entry query/split)
         (tegfs-query)))))))
