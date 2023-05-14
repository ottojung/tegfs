;;;; Copyright (C) 2023  Otto Jung
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
  (define-module (tegfs entry-card-name)
    :export (make-entry-card-name entry-card-name-id entry-card-name-type)
    :use-module ((euphrates raisu) :select (raisu))
    :use-module ((euphrates string-split-3) :select (string-split-3))
    :use-module ((tegfs web-callcontext) :select (callcontext-token))
    :use-module ((tegfs web-iterate-profun-results) :select (web::iterate-profun-results))
    :use-module ((tegfs webcore-ask) :select (webcore::ask))
    )))

(define (make-entry-card-name maybe-full-senderid id)
  (string-append (if id "i" "s")
                 ":"
                 (or id maybe-full-senderid)))

(define (entry-card-name-type name)
  (define-values (pref sep id-or-senderid)
    (string-split-3 ":" name))

  (cond
   ((string-null? sep) #f)
   ((equal? pref "s") 'entry-card-name-senderid)
   ((equal? pref "i") 'entry-card-name-id)
   (else (raisu 'unexpected-entry-card-name-type pref name))))

(define (entry-card-name-id callctx name)
  (define-values (pref sep id-or-senderid)
    (string-split-3 ":" name))

  (and (not (string-null? sep))
       (if (equal? pref "i") id-or-senderid
           (web::iterate-profun-results
            :onfalse (lambda _ #f)
            :results
            (webcore::ask
             `(whats
               (key ,(callcontext-token callctx))
               (senderid->entry ,id-or-senderid _E)
               (entry-field _E "id" id)))

            (id) id))))
