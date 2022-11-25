;;;; Copyright (C) 2022  Otto Jung
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

%var tegfs-query
%var tegfs-query/talk

%use (appcomp comp) "./euphrates/comp.scm"
%use (curry-if) "./euphrates/curry-if.scm"
%use (debug) "./euphrates/debug.scm"
%use (memconst) "./euphrates/memconst.scm"
%use (monad-ask) "./euphrates/monad-ask.scm"
%use (monad-do) "./euphrates/monad-do.scm"
%use (profun-accept profun-ctx-set profun-set) "./euphrates/profun-accept.scm"
%use (profun-make-handler) "./euphrates/profun-make-handler.scm"
%use (profun-op-divisible) "./euphrates/profun-op-divisible.scm"
%use (profun-op-envlambda) "./euphrates/profun-op-envlambda.scm"
%use (profun-op-equals) "./euphrates/profun-op-equals.scm"
%use (profun-op-false) "./euphrates/profun-op-false.scm"
%use (profun-op-less) "./euphrates/profun-op-less.scm"
%use (profun-op-modulo) "./euphrates/profun-op-modulo.scm"
%use (profun-op*) "./euphrates/profun-op-mult.scm"
%use (profun-op+) "./euphrates/profun-op-plus.scm"
%use (profun-op-separate) "./euphrates/profun-op-separate.scm"
%use (profun-op-sqrt) "./euphrates/profun-op-sqrt.scm"
%use (profun-op-true) "./euphrates/profun-op-true.scm"
%use (profun-op-unify) "./euphrates/profun-op-unify.scm"
%use (profun-op-value) "./euphrates/profun-op-value.scm"
%use (profun-reject) "./euphrates/profun-reject.scm"
%use (profun-create-database) "./euphrates/profun.scm"
%use (make-profune-communicator profune-communicator-handle) "./euphrates/profune-communicator.scm"
%use (~s) "./euphrates/tilda-s.scm"
%use (words->string) "./euphrates/words-to-string.scm"
%use (has-access-for-entry-details? has-access-for-entry-target?) "./access.scm"
%use (default-preview-sharing-time) "./default-preview-sharing-time.scm"
%use (entry-target-fullpath) "./entry-target-fullpath.scm"
%use (get-preview-path) "./get-preview-path.scm"
%use (keyword-diropen) "./keyword-diropen.scm"
%use (keyword-dirpreview) "./keyword-dirpreview.scm"
%use (keyword-entry-parent-directory) "./keyword-entry-parent-directory.scm"
%use (keyword-entry-registry-path) "./keyword-entry-registry-path.scm"
%use (keyword-target) "./keyword-target.scm"
%use (keyword-title) "./keyword-title.scm"
%use (tegfs-query/open) "./tegfs-query-open.scm"
%use (web-share-file) "./web-share-file.scm"

(define get-tegfs-query-server-handler
  (memconst
   (profun-make-handler
    (= profun-op-unify)
    (!= profun-op-separate)
    (true profun-op-true)
    (false profun-op-false)
    (+ profun-op+)
    (* profun-op*)
    (modulo profun-op-modulo)
    (sqrt profun-op-sqrt)
    (< profun-op-less)
    (divisible profun-op-divisible)
    (equals profun-op-equals)

    (value (profun-op-value '() '()))

    (entry query-entry-handler)

    )))

(define query-entry-handler
  (profun-op-envlambda
   (ctx env (query/split-name permissions-name filemap/2-name diropen?-name dirpreview?-name E-name))

   ;; FIXME: ensure that they are bound
   (define query/split (env query/split-name))
   (define diropen? (env diropen?-name))
   (define dirpreview? (env dirpreview?-name))

   (define opening-properties
     (appcomp
      '()
      ((curry-if (const diropen?) (comp (cons keyword-diropen))))
      ((curry-if (const dirpreview?) (comp (cons keyword-dirpreview))))))

   (define iter
     (or ctx (tegfs-query/open opening-properties query/split)))

   (define x (iter))
   (if x
       (profun-set
        (E-name <- x)
        (if ctx
            (profun-accept)
            (profun-ctx-set iter)))
       (profun-reject))))

(define (tegfs-query/talk read-sentence)
  (define db
    (profun-create-database
     (get-tegfs-query-server-handler)
     '()))

  (define comm
    (make-profune-communicator db))

  (let loop ((sentence (read-sentence)))
    (define answer
      (profune-communicator-handle comm sentence))

    (debug "[server] ~a" (words->string (map ~s answer)))

    (loop (read-sentence))))

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
;;   - share-full { 'share-full, 'say } (OPTIONAL)
;;   - share-preview { 'share-preview, 'say } (OPTIONAL)

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

    (define (share-full)
      0)

    (define (share-preview)
      (define target-fullpath (entry-target-fullpath entry0))
      (define preview-fullpath (get-preview-path target-fullpath))
      (web-share-file preview-fullpath default-preview-sharing-time))

    (define (unfold-entry)
      (monad-do (entry) 'entry 'say)
      (monad-do (share-full) 'share-full 'say)
      (monad-do (share-preview) 'share-preview 'say)
      )

    (when entry
      (monad-do unfold-entry 'unfold-entry 'say 'many)))

  (define iter
    (tegfs-query/open opening-properties query/split))

  (let loop ()
    (define x (iter))
    (when x
      (for-each-fn x)
      (loop))))

(define target-fields
  (list keyword-target
        keyword-title
        keyword-entry-parent-directory
        keyword-entry-registry-path))
