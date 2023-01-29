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

%run guile

%var categorization-split

%use (comp) "./euphrates/comp.scm"
%use (fn) "./euphrates/fn.scm"
%use (lines->string) "./euphrates/lines-to-string.scm"
%use (list-split-on) "./euphrates/list-split-on.scm"
%use (string-split/simple) "./euphrates/string-split-simple.scm"
%use (string-strip) "./euphrates/string-strip.scm"
%use (string->lines) "./euphrates/string-to-lines.scm"

(define (categorization-split text)
  (define lines (string->lines text))
  (define noncommented (map (comp ((fn string-split/simple % #\;)) car) lines))
  (define split1
    (map lines->string
         (list-split-on (comp string-strip (string-prefix? "----")) noncommented)))
  (define cfg-part (car split1))
  (define rules-part (if (< 1 (length split1)) (cadr split1) ""))
  (values cfg-part rules-part))
