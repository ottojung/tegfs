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

%var CLI-talk

%use (profun-make-handler) "./euphrates/profun-make-handler.scm"
%use (profun-op-divisible) "./euphrates/profun-op-divisible.scm"
%use (profun-op-equals) "./euphrates/profun-op-equals.scm"
%use (profun-op-false) "./euphrates/profun-op-false.scm"
%use (profun-op-less) "./euphrates/profun-op-less.scm"
%use (profun-op-modulo) "./euphrates/profun-op-modulo.scm"
%use (profun-op*) "./euphrates/profun-op-mult.scm"
%use (instantiate-profun-parameter) "./euphrates/profun-op-parameter.scm"
%use (profun-op+) "./euphrates/profun-op-plus.scm"
%use (profun-op-separate) "./euphrates/profun-op-separate.scm"
%use (profun-op-sqrt) "./euphrates/profun-op-sqrt.scm"
%use (profun-op-true) "./euphrates/profun-op-true.scm"
%use (profun-op-unify) "./euphrates/profun-op-unify.scm"
%use (profun-op-value) "./euphrates/profun-op-value.scm"
%use (profun-create-database) "./euphrates/profun.scm"
%use (make-profune-communicator profune-communicator-handle) "./euphrates/profune-communicator.scm"
%use (read-list) "./euphrates/read-list.scm"
%use (read-string-line) "./euphrates/read-string-line.scm"
%use (~s) "./euphrates/tilda-s.scm"
%use (words->string) "./euphrates/words-to-string.scm"
%use (entry-field-handler) "./entry-field-handler.scm"
%use (query-diropen?/p query-dirpreview?/p query-filemap/2/p query-permissions/p query-split/p) "./talk-parameters.scm"
%use (query-entry-handler) "./tegfs-query.scm"

(define tegfs-server-handler
  (profun-make-handler
   (= profun-op-unify)
   (!= profun-op-separate)
   (true profun-op-true)
   (false profun-op-false)
   (+ profun-op+)
   (* profun-op*)
   (modulo profun-op-modulo)
   (sqrt profun-op-sqrt)
   (< profun-op-less)
   (divisible profun-op-divisible)
   (equals profun-op-equals)

   (value (profun-op-value '() '()))

   (entry query-entry-handler)
   (entry-field entry-field-handler)

   (query (instantiate-profun-parameter query-split/p))
   (permissions (instantiate-profun-parameter query-permissions/p))
   (filemap/2 (instantiate-profun-parameter query-filemap/2/p))
   (diropen? (instantiate-profun-parameter query-diropen?/p))
   (dirpreview? (instantiate-profun-parameter query-dirpreview?/p))

   ))

(define (CLI-talk)
  (define (read-sentence)
    (define line
      (begin
        (display "[client] " (current-error-port))
        (read-string-line)))
    (define parsed
      (with-input-from-string
          line
        (lambda _ (read-list))))
    parsed)

  (define db
    (profun-create-database
     tegfs-server-handler
     '()))

  (define comm
    (make-profune-communicator db))

  (let loop ((sentence (read-sentence)))
    (define answer
      (profune-communicator-handle comm sentence))

    (display "[server] ")
    (display (words->string (map ~s answer)))
    (newline)

    (loop (read-sentence))))
