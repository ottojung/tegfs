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

%var query-entry-handler

%use (profun-accept profun-set profun-set-meta) "./euphrates/profun-accept.scm"
%use (profun-reject) "./euphrates/profun-reject.scm"
%use (profun-unbound-value?) "./euphrates/profun-value.scm"
%use (core-entry-handler/generic) "./core-entry-handler-generic.scm"
%use (entry-limit-fields) "./entry-limit-fields.scm"
%use (tegfs-permissions/p) "./talk-parameters.scm"
%use (tegfs-query/open) "./tegfs-query-open.scm"
%use (context-filemap/2) "./web-context.scm"

(define (query-entry-handler-get-iter web-context)
  (lambda (opening-properties query E-name)
    (define filemap/2 (context-filemap/2 web-context))
    (define perm (tegfs-permissions/p))
    (define iter0 (tegfs-query/open opening-properties query))
    (define (iter)
      (define-values (x full) (iter-values))
      (if x
          (profun-set
           (E-name <- x)
           (if full
               (profun-set-meta
                (E-name <- full))
               (profun-accept)))
          (profun-reject)))

    (define (iter-values)
      (define entry0 (iter0))
      (if entry0
          (limit entry0)
          (values #f #f)))

    (define (limit entry0)
      (define limited (entry-limit-fields filemap/2 perm entry0))
      (cond
       ((= (length entry0) (length limited))
        (values entry0 #f))
       ((not (null? limited))
        (values limited entry0))
       (else (iter-values))))

    (cond
     ((profun-unbound-value? perm) ;; NOTE: optimization
      (lambda _ (profun-reject)))
     (else iter))))

(define query-entry-handler
  (lambda (web-context)
    (core-entry-handler/generic (query-entry-handler-get-iter web-context))))
