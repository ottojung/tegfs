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
  (define-module (tegfs web-not-found)
    :export (web::not-found)
    :use-module ((euphrates lines-to-string) :select (lines->string))
    :use-module ((euphrates random-choice) :select (random-choice))
    :use-module ((euphrates string-strip) :select (string-strip))
    :use-module ((euphrates string-to-lines) :select (string->lines))
    :use-module ((euphrates stringf) :select (stringf))
    :use-module ((euphrates with-randomizer-seed) :select (with-randomizer-seed))
    :use-module ((tegfs web-static-error-message) :select (web::static-error-message))
    )))



(define not-found-answers/0
  '("Oh dear, it seems the page you sought,
    has gone and got itself caught,
    But fear not, we'll help you find,
    the information you had in mind."

    "The page you seek is not in sight,
    it's flown away on wings of flight,
    But we'll help you search high and low,
    to find the knowledge you need to know."

    "The link you clicked, it led astray,
    the page you wanted's gone away,
    But don't despair, we'll guide your path,
    to a new page with information to bask."

    "The page you sought has gone missing,
    it's vanished, like a page dismissing,
    But don't you fret, we'll help you find,
    another page with knowledge to bind."

    "The resource you sought is out of reach,
    it's gone to a place beyond our beach,
    But don't you worry, we'll help you find,
    a new page that's easy on the mind."

    "The page you're looking for is not here,
    It's gone off to a place far and dear,
    But don't you worry, we'll help you find,
    Another page that's of a different kind."

    "The link you clicked, it led you wrong,
    The page you wanted's been gone for long,
    But don't despair, we'll help you find,
    Another page with information of a different kind."

    "The page you sought is lost in time,
    It's gone to a place where pages climb,
    But don't you fret, we'll help you find,
    Another page that's of a different kind."

    "The resource you're searching for is not here,
    It's gone to a place far and near,
    But don't you worry, we'll help you find,
    Another page that's of a different kind."

    "The page you're looking for has gone astray,
    It's gone to a place where pages play,
    But don't you fret, we'll help you find,
    Another page that's of a different kind."

    "Lost in cyberspace, a page astray,
    But fear not dear traveler, just click and play."

    "404 error, page not found,
    But don't let it get you down, just look around."

    "Gone astray, this page has flown,
    But don't be dismayed, just move along."

    "The page you seek, is not within,
    But don't let it bring you to a spin."

    "A page is missing, that much is true,
    But don't let it dampen your mood, there's still so much to do."
    ))

(define not-found-answers
  (list->vector
   (map
    (lambda (answer)
      (define lines (string->lines answer))
      (define new (map (lambda (line) (string-strip (string-append line "<br/>"))) lines))
      (lines->string new))
    not-found-answers/0)))

(define (get-random-answer)
  (with-randomizer-seed
   :random
   (car (random-choice 1 not-found-answers))))

(define (web::not-found)
  ((web::static-error-message
    404
    (stringf "<img width='100%' src='static/what.svg'/>
              <p>~a</p>"
             (get-random-answer)))))
