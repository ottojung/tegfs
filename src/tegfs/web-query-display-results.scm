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

(cond-expand
 (guile
  (define-module (tegfs web-query-display-results)
    :export (web::query-display-results)
    :use-module ((euphrates fn-alist) :select (fn-alist))
    :use-module ((euphrates hashmap) :select (hashmap-copy hashmap-set!))
    :use-module ((euphrates raisu) :select (raisu))
    :use-module ((tegfs web-callcontext) :select (callcontext-path callcontext-query))
    :use-module ((tegfs web-display-entry) :select (web::display-entry))
    :use-module ((tegfs web-hashmap-to-query) :select (web::hashmap->query))
    )))

(define (web::query-display-results-header callctx initial? select? show-filter? maybe-value show-menu?)
  (define show-input? (or show-filter? select?))
  (define current-query (callcontext-query callctx))
  (define current-path (callcontext-path callctx))

  (when (and show-filter? select?)
    (raisu 'cannot-have-both-show-filter-and-select))
  (when (and initial? select?)
    (raisu 'cannot-have-both-show-initial-and-select))
  (when (and initial? (not show-filter?))
    (raisu 'cannot-have-initial-without-filter))

  (display "<div class='search-input")
  (display "'>\n")
  (display "<br/>\n")
  (display "<div class='tiled light smooth-edged'>\n")
  (display "<div class='split-container'>")

  (when show-input?
    (display "  <input class='split-left' ")
    (write maybe-value))

  (cond
   (select?
    (display " autofocus type='text' name='aditional' placeholder='Tags to add' />\n")
    (display " <button type='submit' class='")
    (when show-input?
      (display "split-right "))
    (display "selectbtn' title='Add tags'>Add tags</button>"))
   (show-filter?
    (display " autofocus type='text' name='q' placeholder='Filter by tags' />\n")
    (display " <input type='image' class='")
    (when show-input?
      (display "split-right "))
    (display "split-right' src='static/search.svg' title='Search' alt='Submit'/>")))

  (when show-menu?
    (display "<div class='")
    (when show-input?
      (display "split-right"))
    (display "'>\n")

    (let ((new-path "share")
          (new-query current-query))
      (display " <a href='")
      (display new-path)
      (display "?")
      (display (web::hashmap->query new-query))
      (display "'>\n")
      (display "   <img src='static/share-gray.svg' title='Share this query'/>\n")
      (display " </a>\n"))

    (let ((new-path current-path)
          (new-query
           (let ((q (hashmap-copy current-query)))
             (hashmap-set! q 'select "on")
             q)))
      (display " <a href='")
      (display new-path)
      (display "?")
      (display (web::hashmap->query new-query))
      (display "'>\n")
      (display "   <img src='static/select.svg' title='Select objects'/>\n")
      (display " </a>\n"))

    (display "</div>\n"))

  (display "</div>\n")
  (display "</div>\n")
  (display "</div>\n")
  (display "<br/>\n"))

(define (web::query-display-results callctx initial? select? show-filter? maybe-value show-menu? equals)
  (let ()
    (display "<form action=")
    (cond
     (select?
      (write "select")
      (display " enctype='multipart/form-data' method='post'"))
     (else
      (write "query")))
    (when initial?
      (display " class='centering-container'"))
    (display ">\n"))

  (web::query-display-results-header callctx initial? select? show-filter? maybe-value show-menu?)

  (unless initial?
    (display "<div class='cards'>")
    (for-each
     (fn-alist
      (E F PL)
      (define entry E)
      (define maybe-full-senderid F)
      (define preview-linkpath PL)
      (web::display-entry entry select? maybe-full-senderid preview-linkpath))
     equals)
    (display "</div>"))

  (display "</form>\n"))
