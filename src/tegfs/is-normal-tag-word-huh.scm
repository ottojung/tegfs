;;;; Copyright (C) 2023, 2024  Otto Jung
;;;; This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU Affero General Public License for more details. You should have received a copy of the GNU Affero General Public License along with this program.  If not, see <https://www.gnu.org/licenses/>.

(define word-grammar
  `( main = normal-word
     ,@tag-grammar
    ))

(define backend-parser
  (parselynn:simple
   `(:grammar ,word-grammar
     :on-conflict ,ignore
     :sync-to-disk "/tmp/parser-tegfs-is-normal-tag-var-huh.scm"
     )))

(define (is-normal-tag-word? tag-word/str)
  (define (errorp . error-args)
    (debugs error-args)
    #f)
  (parselynn:simple:run/with-error-handler
   backend-parser errorp tag-word/str))
