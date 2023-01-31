;;;; Copyright (C) 2022  Otto Jung
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
  (define-module (tegfs sha256sum)
    :export (sha256sum)
    :use-module ((euphrates comp) :select (appcomp))
    :use-module ((euphrates define-pair) :select (define-pair))
    :use-module ((euphrates raisu) :select (raisu))
    :use-module ((euphrates string-to-lines) :select (string->lines))
    :use-module ((euphrates string-to-words) :select (string->words))
    :use-module ((euphrates system-re) :select (system-re))
    )))



(define (sha256sum text)
  (define-pair (output exit-code)
    (system-re "printf '%s' ~a | sha256sum" text))

  (unless (equal? 0 exit-code)
    (raisu 'could-not-get-a-hash exit-code))

  (appcomp output string->lines car string->words car))
