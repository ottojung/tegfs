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

%var set-config

%use (open-file-port) "./euphrates/open-file-port.scm"
%use (raisu) "./euphrates/raisu.scm"
%use (appcomp comp) "./euphrates/comp.scm"
%use (append-posix-path) "./euphrates/append-posix-path.scm"
%use (assoc-set-value) "./euphrates/assoc-set-value.scm"

%use (get-config) "./get-config.scm"

%use (root/p) "./root-p.scm"

(define (set-config name value)
  (define path (append-posix-path (root/p) "config.tegfs.lisp"))
  (define existing (or (get-config) '()))
  (define new (assoc-set-value name value existing))
  (define p (open-file-port path "w"))
  (for-each (lambda (x) (write x p) (newline p)) new)
  (close-port p)
  (values))
