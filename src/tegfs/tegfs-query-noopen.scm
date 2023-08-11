;;;; Copyright (C) 2022, 2023  Otto Jung
;;;; This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU Affero General Public License for more details. You should have received a copy of the GNU Affero General Public License along with this program.  If not, see <https://www.gnu.org/licenses/>.

(define (tegfs-query/noopen <query...>)
  (define iter0 (entries-iterate))
  (if (and <query...> (not (null? <query...>)))
      ;; (tegfs-query/noopen/notall/prolog iter0 <query...>)
      (tegfs-query/noopen/notall/profun iter0 <query...>)
      iter0))

(define (tegfs-query/noopen/notall/profun iter0 <query...>)
  (define-values (parsed-query/1 variables) (prolog-query-parse <query...>))
  (define parsed-query
    (generify-dumped-term parsed-query/1))
  (define rules
    (cons `((%any X))
          (map inference->profun-rule
               (dump-rules/list))))
  (define db0
    (profun-create-falsy-database
     profun-standard-handler
     rules))

  (define (profun-filter entry)
    (define translated (translate-entry-tags entry))
    (define db (profun-database-extend db0 (map list translated)))
    (profun-eval-query/boolean db parsed-query))

  (define (iter)
    (let loop ()
      (define entry (iter0))
      (and entry
           (if (profun-filter entry)
               entry
               (loop)))))

  iter)
