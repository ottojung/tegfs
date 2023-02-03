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
  (define-module (tegfs web-create-temp-path)
    :export (web::create-temp-path)
    :use-module ((euphrates assoc-set-value) :select (assoc-set-value))
    :use-module ((tegfs add-tempentry) :select (add-tempentry))
    :use-module ((tegfs keyword-stime) :select (keyword-stime))
    :use-module ((tegfs update-tempentry) :select (update-tempentry))
    :use-module ((tegfs web-callcontext) :select (callcontext-token))
    :use-module ((tegfs web-iterate-profun-results) :select (web::iterate-profun-results))
    :use-module ((tegfs webcore-ask) :select (webcore::ask))
    )))



(define (web::create-temp-path callctx stime destination-fun)
  (define key (callcontext-token callctx))

  (define results
    (webcore::ask
     `(whats
       (key ,key)
       (add-tempentry ID ((,keyword-stime . ,stime))))))

  (web::iterate-profun-results
   :results results
   (ID)
   (define r2
     (webcore::ask
      `(whats
        (key ,key)
        (get-tempentry ,ID E))))
   (web::iterate-profun-results
    :results r2
    (E)
    (define new
      (assoc-set-value
       'destination (destination-fun ID)
       E))
    (define r3
      (webcore::ask
       `(whats
         (key ,key)
         (get-tempentry ,ID E)
         (update-tempentry E ,new))))
    r3)
   ID))
