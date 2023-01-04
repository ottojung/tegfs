;;;; Copyright (C) 2023  Otto Jung
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

%var web-query-display-results

%use (assq-or) "./euphrates/assq-or.scm"
%use (raisu) "./euphrates/raisu.scm"
%use (web-display-entry) "./web-display-entry.scm"
%use (web-make-html-response) "./web-make-html-response.scm"

(define (web-query-display-results equals)
  (web-make-html-response
   (lambda _
     (display "<div class='cards'>")
     (for-each
      (lambda (bindings)
        (define entry
          (assq-or 'E bindings (raisu 'unexpected-result-from-backend bindings)))
        (define maybe-full-senderid
          (assq-or 'F bindings (raisu 'unexpected-result-from-backend bindings)))
        (define preview-link
          (assq-or 'PL bindings (raisu 'unexpected-result-from-backend bindings)))
        (web-display-entry entry maybe-full-senderid preview-link))
      equals)
     (display "</div>"))))

