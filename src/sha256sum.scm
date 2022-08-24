;;;; Copyright (C) 2022  Otto Jung
;;;;
;;;; This program is free software: you can redistribute it and/or modify
;;;; it under the terms of the GNU General Public License as published by
;;;; the Free Software Foundation; version 3 of the License.
;;;;
;;;; This program is distributed in the hope that it will be useful,
;;;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;;; GNU General Public License for more details.
;;;;
;;;; You should have received a copy of the GNU General Public License
;;;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

%run guile

%var sha256sum

%use (appcomp) "./euphrates/comp.scm"
%use (define-pair) "./euphrates/define-pair.scm"
%use (raisu) "./euphrates/raisu.scm"
%use (string->lines) "./euphrates/string-to-lines.scm"
%use (string->words) "./euphrates/string-to-words.scm"
%use (system-re) "./euphrates/system-re.scm"

(define (sha256sum text)
  (define-pair (output exit-code)
    (system-re "printf '%s' ~a | sha256sum" text))

  (unless (equal? 0 exit-code)
    (raisu 'could-not-get-a-hash exit-code))

  (appcomp output string->lines car string->words car))
