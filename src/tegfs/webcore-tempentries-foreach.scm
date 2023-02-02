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
  (define-module (tegfs webcore::tempentries-foreach)
    :export (webcore::tempentries-foreach)
    :use-module ((euphrates hashmap) :select (hashmap-foreach))
    :use-module ((tegfs webcore-serialize-tempentry) :select (webcore::serialize-tempentry))
    )))

(define (webcore::tempentries-foreach tempentries fun)
  (hashmap-foreach
   (lambda (key value)
     (define s (webcore::serialize-tempentry key value))
     (fun s))
   tempentries))
