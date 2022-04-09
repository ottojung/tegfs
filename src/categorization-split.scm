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

%var categorization-split

%use (comp) "./euphrates/comp.scm"
%use (fn) "./euphrates/fn.scm"
%use (string->lines) "./euphrates/string-to-lines.scm"
%use (string-strip) "./euphrates/string-strip.scm"
%use (string-split/simple) "./euphrates/string-split-simple.scm"
%use (list-split-on) "./euphrates/list-split-on.scm"
%use (define-tuple) "./euphrates/define-tuple.scm"
%use (lines->string) "./euphrates/lines-to-string.scm"
%use (list-map/flatten) "./euphrates/list-map-flatten.scm"
%use (curry-if) "./euphrates/curry-if.scm"
%use (CFG-CLI->CFG-AST) "./euphrates/parse-cfg-cli.scm"
%use (list-deduplicate) "./euphrates/list-deduplicate.scm"
%use (raisu) "./euphrates/raisu.scm"
%use (list-and-map) "./euphrates/list-and-map.scm"
%use (list-or-map) "./euphrates/list-or-map.scm"
%use (list-find-first) "./euphrates/list-find-first.scm"
%use (compose-under) "./euphrates/compose-under.scm"

(define (categorization-split text)
  (define lines (string->lines text))
  (define noncommented (map (comp ((fn string-split/simple % #\;)) car) lines))
  (define split1
    (map lines->string
         (list-split-on (comp string-strip (string-prefix? "----")) noncommented)))
  (define cfg-part (car split1))
  (define rules-part (if (< 1 (length split1)) (cadr split1) ""))
  (values cfg-part rules-part))
