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

%var tegfs-query

%use (appcomp comp) "./euphrates/comp.scm"
%use (curry-if) "./euphrates/curry-if.scm"
%use (monad-ask) "./euphrates/monad-ask.scm"
%use (monad-do) "./euphrates/monad-do.scm"
%use (has-access-for-entry-details? has-access-for-entry-target?) "./access.scm"
%use (keyword-diropen) "./keyword-diropen.scm"
%use (keyword-dirpreview) "./keyword-dirpreview.scm"
%use (keyword-entry-parent-directory) "./keyword-entry-parent-directory.scm"
%use (keyword-entry-registry-path) "./keyword-entry-registry-path.scm"
%use (keyword-target) "./keyword-target.scm"
%use (keyword-title) "./keyword-title.scm"
%use (tegfs-query/open) "./tegfs-query-open.scm"

;; Monad contract:
;; - type { 'ask }
;;   where type is:
;;   - 'query/split (REQUIRED)
;;   - 'permissions  (REQUIRED)
;;   - 'filemap/2  (REQUIRED)
;;   - 'diropen? (DEFAULT #f)
;;   - 'dirpreview? (DEFAULT #f)
;; - unfold-entry { 'unfold-entry, 'say, 'many } (OPTIONAL)
;;   unfolds to:
;;   - entry { 'entry, 'say } (OPTIONAL)
(define (tegfs-query)
  (monad-ask query/split)
  (monad-ask permissions)
  (monad-ask filemap/2)
  (monad-ask diropen? :default #f)
  (monad-ask dirpreview? :default #f)

  (define opening-properties
    (appcomp
     '()
     ((curry-if (const diropen?) (comp (cons keyword-diropen))))
     ((curry-if (const dirpreview?) (comp (cons keyword-dirpreview))))))

  (define (for-each-fn entry0)
    (define entry
      (cond
       ((has-access-for-entry-details? filemap/2 permissions entry0)
        (lambda _ entry0))
       ((has-access-for-entry-target? filemap/2 permissions entry0)
        (lambda _
          (filter (lambda (p) (memq (car p) target-fields)) entry0)))
       (else #f)))

    (define (unfold-entry)
      (monad-do (entry) 'entry 'say)
      )

    (when entry
      (monad-do unfold-entry 'unfold-entry 'say 'many)))

  (tegfs-query/open opening-properties query/split for-each-fn))

(define target-fields
  (list keyword-target
        keyword-title
        keyword-entry-parent-directory
        keyword-entry-registry-path))
