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

%run guile

%var tegfs-edit-tags
%var tegfs-process-categorization-text

%use (raisu) "./euphrates/raisu.scm"
%use (read-string-file) "./euphrates/read-string-file.scm"
%use (system-fmt) "./euphrates/system-fmt.scm"
%use (categorization-complete-selection/cont) "./categorization-complete-selection-cont.scm"
%use (categorization-parse-tags) "./categorization-parse-tags.scm"
%use (categorization-starred-symbol?) "./categorization-starred-symbol-huh.scm"
%use (parsed-categorization-tags-get-all) "./parsed-categorization-tags-get-all.scm"

(define (tegfs-edit-tags working-file)
  (unless working-file
    (raisu 'must-provide-working-file))

  (system-fmt "$EDITOR ~a" working-file)
  (tegfs-process-categorization-text (read-string-file working-file)))

(define (tegfs-process-categorization-text text)
  (define ast/flatten
    (categorization-parse-tags text))

  (define all-tags
    (parsed-categorization-tags-get-all ast/flatten))

  (define starred
    (filter categorization-starred-symbol? all-tags))

  (categorization-complete-selection/cont ast/flatten all-tags starred))
