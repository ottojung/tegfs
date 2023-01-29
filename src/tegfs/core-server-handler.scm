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
  (define-module (tegfs core-server-handler)
    :export (core::make-server-handler)
    :use-module ((euphrates profun-handler) :select (profun-handler-extend))
    :use-module ((euphrates profun-op-parameter) :select (instantiate-profun-parameter))
    :use-module ((euphrates profun-op-value) :select (profun-op-value))
    :use-module ((euphrates profun-standard-handler) :select (profun-standard-handler))
    :use-module ((tegfs add-entry) :select (add-entry))
    :use-module ((tegfs core-add-entry) :select (core::add-entry))
    :use-module ((tegfs core-categorization) :select (core::categorization))
    :use-module ((tegfs core-entry-field) :select (core::entry-field))
    :use-module ((tegfs core-entry) :select (core::entry))
    :use-module ((tegfs core-paremeters) :select (core::diropen?/p core::dirpreview?/p core::query/p))
    :use-module ((tegfs core-set-categorization) :select (core::set-categorization))
    :use-module ((tegfs core-update-entry) :select (core::update-entry)))))



(define (core::make-server-handler)
  (profun-handler-extend
   profun-standard-handler

   (entry core::entry)
   (entry-field core::entry-field)
   (add-entry core::add-entry)
   (update-entry core::update-entry)
   (categorization core::categorization)
   (set-categorization core::set-categorization)

   (query (instantiate-profun-parameter core::query/p))
   (diropen? (instantiate-profun-parameter core::diropen?/p))
   (dirpreview? (instantiate-profun-parameter core::dirpreview?/p))

   (value (profun-op-value '() '()))

   ))
