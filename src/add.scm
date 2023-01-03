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

%var tegfs-add
%var tegfs-add/parse

%use (catchu-case) "./euphrates/catchu-case.scm"
%use (fn-cons) "./euphrates/fn-cons.scm"
%use (list-zip) "./euphrates/list-zip.scm"
%use (raisu) "./euphrates/raisu.scm"
%use (~a) "./euphrates/tilda-a.scm"
%use (add-entry) "./add-entry.scm"
%use (fatal) "./fatal.scm"
%use (keyword-date) "./keyword-date.scm"
%use (keyword-prev) "./keyword-prev.scm"
%use (keyword-tags) "./keyword-tags.scm"
%use (keyword-target) "./keyword-target.scm"
%use (keyword-title) "./keyword-title.scm"

(define (tegfs-add/parse
         <target> <title> <tag...>
         --series <key...> <value...>
         <registry-file> <date>)
  (define key-value-pairs
    (list-zip (or <key...> '()) (or <value...> '())))

  (define tags (or <tag...> '()))

  (catchu-case

   (tegfs-add
    <target> <title> tags
    --series key-value-pairs
    <registry-file> <date>)

   (('missing-registry-file)
    (fatal "Parameter <registry-file> is required, but it is not set"))

   (('no-last-id-for-series)
    (fatal "Want series, but last-id file is not present"))

   (('target-absolute-but-should-relative target)
    (fatal "Target ~s must be a path relative to the registry file, not an absolute path" target))

   (('target-does-not-exist target)
    (fatal "Target ~s does not exist. Note that filepath must be relative to the registry file" target)))

  (display "Added!\n"))

(define (tegfs-add
         <target> <title> tags
         series? key-value-pairs0
         <registry-file> <date>)

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
     (if <title>
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

  (unless <registry-file>
    (raisu 'missing-registry-file))

  (add-entry <registry-file> entry))
