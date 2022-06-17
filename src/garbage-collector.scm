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

%var tegfs-garbage-collector/parse

%use (string->seconds) "./euphrates/string-to-seconds.scm"
%use (stringf) "./euphrates/stringf.scm"
%use (system-fmt) "./euphrates/system-fmt.scm"

%use (fatal) "./fatal.scm"
%use (get-config) "./get-config.scm"

(define (tegfs-garbage-collector/parse)
  (define config (get-config))
  (define sleep-time (string->seconds "30m"))
  (define port
    (cadr (or (assoc 'port config)
              (fatal "Variable 'port must be set in the config: ~s" config))))
  (define command
    (stringf "set -x ; while true ; do
      curl http://localhost:~a/collectgarbage ;
      sleep ~a ;
      done " port sleep-time))

%for (COMPILER "guile")
  (execl "/bin/sh" "/bin/sh" "-c" command)
%end
  (system-fmt command)
  )
