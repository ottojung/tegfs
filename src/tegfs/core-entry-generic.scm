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

(cond-expand
 (guile
  (define-module (tegfs core-entry-generic)
    :export (core::entry/generic)
    :use-module ((euphrates comp) :select (appcomp comp))
    :use-module ((euphrates curry-if) :select (curry-if))
    :use-module ((euphrates profun-RFC) :select (make-profun-RFC))
    :use-module ((euphrates profun-accept) :select (profun-accept? profun-ctx-set))
    :use-module ((euphrates profun-default) :select (profun-default))
    :use-module ((euphrates profun-error) :select (make-profun-error))
    :use-module ((euphrates profun-op-lambda) :select (profun-op-lambda))
    :use-module ((euphrates profun-value) :select (profun-bound-value? profun-unbound-value?))
    :use-module ((tegfs core-paremeters) :select (core::diropen?/p core::dirpreview?/p core::query/p))
    :use-module ((tegfs keyword-diropen) :select (keyword-diropen))
    :use-module ((tegfs keyword-dirpreview) :select (keyword-dirpreview)))))



(define core::entry/generic
  (lambda (get-iter)
    (profun-op-lambda
     :with-env
     (ctx (entry/out) (E-name))

     (if ctx (ctx)
         (let ()
           (define query (profun-default (core::query/p) #f))
           (define diropen? (profun-default (core::diropen?/p) #t))
           (define dirpreview? (profun-default (core::dirpreview?/p) #f))

           (define opening-properties
             (appcomp
              '()
              ((curry-if (const diropen?) (comp (cons keyword-diropen))))
              ((curry-if (const dirpreview?) (comp (cons keyword-dirpreview))))))

           (cond
            ((profun-bound-value? entry/out)
             (make-profun-error 'type-error "Entry predicated is a generator, it should not be used to check if an element already exists"))
            (else
             (let* ((iter (get-iter opening-properties query E-name))
                    (val (iter)))
               (if (profun-accept? val)
                   (profun-ctx-set iter val)
                   val))))

           )))))
