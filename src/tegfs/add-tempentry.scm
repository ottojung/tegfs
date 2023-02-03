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
  (define-module (tegfs add-tempentry)
    :export (add-tempentry)
    :use-module ((euphrates assoc-set-default) :select (assoc-set-default))
    :use-module ((euphrates assq-or) :select (assq-or))
    :use-module ((euphrates fn-pair) :select (fn-pair))
    :use-module ((euphrates hashmap) :select (hashmap-set!))
    :use-module ((euphrates raisu) :select (raisu))
    :use-module ((tegfs current-time-p) :select (current-time/p))
    :use-module ((tegfs custom-tempentry) :select (custom-tempentry-constructor))
    :use-module ((tegfs get-random-network-name) :select (get-random-network-name))
    :use-module ((tegfs keyword-date) :select (keyword-date))
    :use-module ((tegfs keyword-id) :select (keyword-id))
    :use-module ((tegfs keyword-stime) :select (keyword-stime))
    )))

(define (add-tempentry tempentries maybe-id entry0)
  (define id
    (or maybe-id (get-random-network-name)))
  (define stime
    (assq-or keyword-stime entry0 (raisu 'tempentry-must-have-stime-set)))

  (define (get-date)
    (current-time/p))

  (define entry1
    (assoc-set-default
     keyword-date (get-date)
     entry0))

  (define date
    (assq-or keyword-date entry1 (raisu 'impossible)))

  (define fields
    (filter
     (fn-pair
      (key val)
      (not (memq key (list keyword-id keyword-stime keyword-date))))
     entry1))

  (define c
    (custom-tempentry-constructor id date stime fields))

  (hashmap-set! tempentries id c)

  id)
