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
%var query-entry-handler/fn

%use (appcomp comp) "./euphrates/comp.scm"
%use (curry-if) "./euphrates/curry-if.scm"
%use (make-profun-RFC) "./euphrates/profun-RFC.scm"
%use (profun-accept profun-accept? profun-ctx-set profun-set profun-set-meta) "./euphrates/profun-accept.scm"
%use (profun-default) "./euphrates/profun-default.scm"
%use (make-profun-error) "./euphrates/profun-error.scm"
%use (profun-op-envlambda) "./euphrates/profun-op-envlambda.scm"
%use (profun-reject) "./euphrates/profun-reject.scm"
%use (profun-bound-value? profun-unbound-value?) "./euphrates/profun-value.scm"
%use (entry-limit-fields) "./entry-limit-fields.scm"
%use (keyword-diropen) "./keyword-diropen.scm"
%use (keyword-dirpreview) "./keyword-dirpreview.scm"
%use (permission?) "./permission.scm"
%use (query-diropen?/p query-dirpreview?/p query-split/p tegfs-permissions/p) "./talk-parameters.scm"
%use (tegfs-query/open) "./tegfs-query-open.scm"
%use (context-filemap/2) "./web-context.scm"

(define (query-entry-handler-get-iter web-context opening-properties query E-name)
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
   (else iter)))

(define query-entry-handler
  (lambda (context)
    (profun-op-envlambda
     (ctx env (E-name))

     (if ctx (ctx)
         (let ()
            (define query (query-split/p))
            (define diropen? (profun-default (query-diropen?/p) #t))
            (define dirpreview? (profun-default (query-dirpreview?/p) #f))

            (define opening-properties
              (appcomp
               '()
               ((curry-if (const diropen?) (comp (cons keyword-diropen))))
               ((curry-if (const dirpreview?) (comp (cons keyword-dirpreview))))))

            (cond
             ((profun-unbound-value? query)
              (make-profun-RFC `((query _))))
             ((profun-bound-value? (env E-name))
              (make-profun-error 'query-is-a-generator 'cannot-check-if-element-already-exists))
             (else
              (let* ((iter (query-entry-handler-get-iter context opening-properties query E-name))
                     (val (iter)))
                (if (profun-accept? val)
                    (profun-ctx-set iter val)
                    val))))

            )))))
