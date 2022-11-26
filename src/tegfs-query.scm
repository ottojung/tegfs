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
%var query-entry-handler

%use (appcomp comp) "./euphrates/comp.scm"
%use (curry-if) "./euphrates/curry-if.scm"
%use (debug) "./euphrates/debug.scm"
%use (monad-ask) "./euphrates/monad-ask.scm"
%use (monad-do) "./euphrates/monad-do.scm"
%use (profun-accept profun-ctx-set profun-set) "./euphrates/profun-accept.scm"
%use (make-profun-error) "./euphrates/profun-error.scm"
%use (profun-op-envlambda) "./euphrates/profun-op-envlambda.scm"
%use (profun-reject) "./euphrates/profun-reject.scm"
%use (profun-unbound-value?) "./euphrates/profun-value.scm"
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
%use (query-split/p) "./talk-parameters.scm"
%use (tegfs-query/open) "./tegfs-query-open.scm"
%use (web-share-file) "./web-share-file.scm"

(define-syntax profun-default
  (syntax-rules ()
    ((_ value default)
     (let ((val value))
       (if (profun-unbound-value? val) default val)))))

(define query-entry-handler
  (profun-op-envlambda
   ;; (ctx env (query/split-name permissions-name filemap/2-name diropen?-name dirpreview?-name E-name))
   (ctx env (E-name))

   (call-with-current-continuation
    (lambda (return)
      (define (error . args) (return (make-profun-error args)))

      ;; FIXME: ensure that they are bound
      ;; (define query/split (env query/split-name))
      (define query/split
        (profun-default (query-split/p) (error 'missing-parameter 'query)))

      ;; (define diropen? (env diropen?-name))
      ;; (define dirpreview? (env dirpreview?-name))
      (define diropen? #f)
      (define dirpreview? #f)

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

   ))

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
