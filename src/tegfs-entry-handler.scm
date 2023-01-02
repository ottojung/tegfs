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

%use (appcomp comp) "./euphrates/comp.scm"
%use (curry-if) "./euphrates/curry-if.scm"
%use (profun-accept profun-accept? profun-ctx-set profun-set profun-set-meta) "./euphrates/profun-accept.scm"
%use (make-profun-error) "./euphrates/profun-error.scm"
%use (profun-op-envlambda) "./euphrates/profun-op-envlambda.scm"
%use (profun-reject) "./euphrates/profun-reject.scm"
%use (profun-bound-value? profun-unbound-value?) "./euphrates/profun-value.scm"
%use (entry-limit-fields) "./entry-limit-fields.scm"
%use (keyword-diropen) "./keyword-diropen.scm"
%use (keyword-dirpreview) "./keyword-dirpreview.scm"
%use (query-diropen?/p query-dirpreview?/p query-split/p tegfs-permissions/p) "./talk-parameters.scm"
%use (tegfs-query/open) "./tegfs-query-open.scm"
%use (context-filemap/2) "./web-context.scm"

(define-syntax profun-default
  (syntax-rules ()
    ((_ value default)
     (let ((val value))
       (if (profun-unbound-value? val) default val)))))

(define query-entry-handler
  (lambda (context)
    (profun-op-envlambda
     (ctx env (E-name))

     (define (ret iter)
       (define-values (x full) (iter))
       (if x
           (profun-set
            (E-name <- x)
            (if full
                (profun-set-meta
                 (E-name <- full))
                (profun-accept)))
           (profun-reject)))

     (if ctx (ret ctx)
         (call-with-current-continuation
          (lambda (return)
            (define (error . args) (return (make-profun-error args)))
            (define-syntax define-param
              (syntax-rules ()
                ((_ name p)
                 (define-param name p (error 'missing-parameter (quote name))))
                ((_ name p default)
                 (define name (profun-default p default)))))

            (define-param perm (tegfs-permissions/p) #f)
            (define-param query (query-split/p))
            (define-param diropen? (query-diropen?/p) #t)
            (define-param dirpreview? (query-dirpreview?/p) #f)
            (define filemap/2 (context-filemap/2 context))

            (define opening-properties
              (appcomp
               '()
               ((curry-if (const diropen?) (comp (cons keyword-diropen))))
               ((curry-if (const dirpreview?) (comp (cons keyword-dirpreview))))))

            (define iter0 (tegfs-query/open opening-properties query))
            (define (iter)
              (define entry0 (iter0))
              (define limited (entry-limit-fields filemap/2 perm entry0))
              (cond
               ((equal? #f entry0)
                (values #f #f))
               ((= (length entry0) (length limited))
                (values entry0 #f))
               ((not (null? limited))
                (values limited entry0))
               (else (iter))))

            (cond
             ((profun-bound-value? (env E-name))
              (make-profun-error 'query-is-a-generator 'cannot-check-if-element-already-exists))
             (else
              (let ((val (ret iter)))
                (if (profun-accept? val)
                    (profun-ctx-set iter val)
                    val))))

            ))))))
