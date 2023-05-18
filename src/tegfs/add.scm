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
  (define-module (tegfs add)
    :export (tegfs-add tegfs-add-file tegfs-add/parse)
    :use-module ((euphrates catchu-case) :select (catchu-case))
    :use-module ((euphrates fn-cons) :select (fn-cons))
    :use-module ((euphrates list-zip) :select (list-zip))
    :use-module ((euphrates path-get-basename) :select (path-get-basename))
    :use-module ((euphrates tilda-a) :select (~a))
    :use-module ((tegfs add-entry) :select (add-entry))
    :use-module ((tegfs add-file-entry) :select (add-file-entry add-file-entry/link))
    :use-module ((tegfs entry-get-id) :select (entry-get-id))
    :use-module ((tegfs fatal) :select (fatal))
    :use-module ((tegfs keyword-date) :select (keyword-date))
    :use-module ((tegfs keyword-prev) :select (keyword-prev))
    :use-module ((tegfs keyword-tags) :select (keyword-tags))
    :use-module ((tegfs keyword-target) :select (keyword-target))
    :use-module ((tegfs keyword-title) :select (keyword-title))
    )))



(define (tegfs-add/parse
         <add-file> --link <target> <title> <tag...>
         --series <key...> <value...>
         <date>)
  (define key-value-pairs
    (list-zip (or <key...> '()) (or <value...> '())))

  (define tags (or <tag...> '()))

  (define entry
    (catchu-case

     (if <add-file>
         (tegfs-add-file
          <add-file> (path-get-basename <add-file>) --link
          <title> tags
          --series key-value-pairs
          <date>)
         (tegfs-add
          <target> <title> tags
          --series key-value-pairs
          <date>))

     (('entry-with-such-id-already-exists existing-id)
      (fatal "Want an original id, but this one (~a) already exists" existing-id))

     (('no-last-id-for-series)
      (fatal "Want series, but last-id file is not present"))

     (('target-absolute-but-should-relative target)
      (fatal "Target ~s must be a path relative to the registry file, not an absolute path" target))

     (('target-does-not-exist target)
      (fatal "Target ~s does not exist. Note that filepath must be relative to the registry file" target))))

  (display (entry-get-id entry))
  (newline))

(define (tegfs-add/make-entry
         <target> <title> tags
         series? key-value-pairs0
         <date>)

  (define (tosymbol x)
    (cond
     ((symbol? x) x)
     (else (string->symbol (~a x)))))

  (define key-value-pairs
    (map (fn-cons tosymbol identity)
         (if <target>
             (cons
              (cons keyword-target <target>)
              key-value-pairs0)
             key-value-pairs0)))

  (define entry
    (append
     (if (and <title> (not (string-null? <title>)))
         (list (cons keyword-title <title>))
         (list))
     (if <date>
         (list (cons keyword-date <date>))
         (list))
     (if (null? tags)
         (list)
         (list (cons keyword-tags tags)))
     (if series?
         (list (cons keyword-prev '%LAST-ID))
         (list))
     key-value-pairs))

  entry)

(define (tegfs-add
         <target> <title> tags
         series? key-value-pairs0
         <date>)
  (define entry
    (tegfs-add/make-entry
     <target> <title> tags
     series? key-value-pairs0
     <date>))
  (add-entry entry))

(define (tegfs-add-file
         full-filepath filename --link
         <title> tags
         series? key-value-pairs0
         <date>)
  (define entry
    (tegfs-add/make-entry
     #f <title> tags
     series? key-value-pairs0
     <date>))
  (if --link
      (add-file-entry/link (cons full-filepath filename) entry)
      (add-file-entry (cons full-filepath filename) entry)))
