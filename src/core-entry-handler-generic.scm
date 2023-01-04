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

%var core-entry-handler/generic

%use (appcomp comp) "./euphrates/comp.scm"
%use (curry-if) "./euphrates/curry-if.scm"
%use (make-profun-RFC) "./euphrates/profun-RFC.scm"
%use (profun-accept? profun-ctx-set) "./euphrates/profun-accept.scm"
%use (profun-default) "./euphrates/profun-default.scm"
%use (make-profun-error) "./euphrates/profun-error.scm"
%use (profun-op-envlambda) "./euphrates/profun-op-envlambda.scm"
%use (profun-bound-value? profun-unbound-value?) "./euphrates/profun-value.scm"
%use (keyword-diropen) "./keyword-diropen.scm"
%use (keyword-dirpreview) "./keyword-dirpreview.scm"
%use (query-diropen?/p query-dirpreview?/p query-split/p) "./talk-parameters.scm"

(define core-entry-handler/generic
  (lambda (get-iter)
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
              (let* ((iter (get-iter opening-properties query E-name))
                     (val (iter)))
                (if (profun-accept? val)
                    (profun-ctx-set iter val)
                    val))))

            )))))
