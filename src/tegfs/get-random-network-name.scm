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
  (define-module (tegfs get-random-network-name)
    :export (get-random-network-name)
    :use-module ((euphrates alphanum-alphabet) :select (alphanum/alphabet))
    :use-module ((euphrates big-random-int) :select (big-random-int))
    )))

;; Short names for users to see.
;; Use this only if exporting over a slow network.
;; Case-sensitive!



(define get-random-network-name
  (let* ((a alphanum/alphabet)
         (size (vector-length a))
         (groupsize 4)
         (ngroups 4)
         (n (+ (* ngroups groupsize)
               (- ngroups 1))))
    (lambda _
      (define ret (make-string n))
      (let loop ((i n))
        (when (< 0 i)
          (string-set!
           ret (- i 1)
           (case (modulo i (+ 1 groupsize))
             ((0) #\-)
             (else (vector-ref a (big-random-int size)))))
          (loop (- i 1))))
      ret)))
