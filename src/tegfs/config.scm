;;;; Copyright (C) 2022, 2023  Otto Jung
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
  (define-module (tegfs config)
    :export (tegfs-config/parse)
    :use-module ((euphrates raisu) :select (raisu))
    :use-module ((tegfs config-get) :select (config-get))
    :use-module ((tegfs config-set) :select (config-set!))
    :use-module ((tegfs fatal) :select (fatal))
    :use-module ((tegfs get-config) :select (get-config/fatal))
    :use-module ((tegfs sha256sum) :select (sha256sum))
    )))


(define (tegfs-config/parse get set <key...> --password <value>)
  (define config (get-config/fatal))
  (define keylist (map string->symbol <key...>))

  (define value
    (if --password
        (sha256sum <value>)
        <value>))

  (unless config
    (fatal "Could not parse the config"))

  (cond
   (get
    (display (config-get keylist config (fatal "Not set")))
    (newline))
   (set
    (config-set! keylist config value)
    (display "Ok" (current-error-port))
    (newline (current-error-port)))
   (else
    (raisu 'impossible-case-72536123))))
