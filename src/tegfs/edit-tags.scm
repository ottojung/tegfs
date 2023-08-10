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
  (define-module (tegfs edit-tags)
    :export (tegfs-edit-tags tegfs-process-categorization-text)
    :use-module ((euphrates raisu) :select (raisu))
    :use-module ((euphrates read-string-file) :select (read-string-file))
    :use-module ((tegfs categorization-complete-selection-parsed) :select (categorization-complete-selection/parsed))
    :use-module ((tegfs categorization-parse) :select (categorization-parse))
    :use-module ((tegfs categorization-starred-symbol-huh) :select (categorization-starred-symbol?))
    :use-module ((tegfs categorization-translate-choices) :select (categorization-translate-choices))
    :use-module ((tegfs make-tag-parser) :select (make-tag-parser))
    :use-module ((tegfs parsed-categorization-tags-get-all) :select (parsed-categorization-tags-get-all))
    :use-module ((tegfs texteditor-p) :select (texteditor/p))
    )))



(define (tegfs-edit-tags working-file)
  (unless working-file
    (raisu 'must-provide-working-file))

  (system* (texteditor/p) working-file)
  (tegfs-process-categorization-text
   (read-string-file working-file)))

(define (tegfs-process-categorization-text text)
  (define ast/flatten
    (categorization-parse text))

  (define all-tags
    (parsed-categorization-tags-get-all ast/flatten))

  (define starred
    (filter categorization-starred-symbol? all-tags))

  (define parser (make-tag-parser 0))
  (define translated-choices
    (categorization-translate-choices parser ast/flatten starred))

  (define continued
    (categorization-complete-selection/parsed ast/flatten translated-choices))

  (append continued
          (list (cons 'choices translated-choices))))
