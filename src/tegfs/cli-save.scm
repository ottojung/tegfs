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
  (define-module (tegfs cli-save)
    :export (CLI::save)
    :use-module ((euphrates append-posix-path) :select (append-posix-path))
    :use-module ((euphrates assq-or) :select (assq-or))
    :use-module ((euphrates dprintln) :select (dprintln))
    :use-module ((euphrates file-delete) :select (file-delete))
    :use-module ((euphrates file-or-directory-exists-q) :select (file-or-directory-exists?))
    :use-module ((euphrates make-directories) :select (make-directories))
    :use-module ((euphrates path-get-basename) :select (path-get-basename))
    :use-module ((euphrates path-normalize) :select (path-normalize))
    :use-module ((euphrates raisu) :select (raisu))
    :use-module ((euphrates read-string-file) :select (read-string-file))
    :use-module ((euphrates run-syncproc) :select (run-syncproc))
    :use-module ((euphrates string-split-3) :select (string-split-3))
    :use-module ((euphrates stringf) :select (stringf))
    :use-module ((euphrates system-environment) :select (system-environment-get))
    :use-module ((euphrates tilda-s) :select (~s))
    :use-module ((euphrates write-string-file) :select (write-string-file))
    :use-module ((tegfs a-weblink-q) :select (a-weblink?))
    :use-module ((tegfs add) :select (tegfs-add tegfs-add-file))
    :use-module ((tegfs cli-save-loop) :select (CLI::save::loop))
    :use-module ((tegfs clipboard) :select (classify-clipboard-text-content))
    :use-module ((tegfs default-db-path) :select (default-db-path))
    :use-module ((tegfs dump-clipboard) :select (tegfs-dump-clipboard tegfs-dump-clipboard/pasta))
    :use-module ((tegfs fatal) :select (fatal))
    :use-module ((tegfs get-random-basename) :select (get-random-basename))
    :use-module ((tegfs get-root) :select (get-root))
    :use-module ((tegfs keyword-id) :select (keyword-id))
    :use-module ((tegfs make-temporary-filename-local) :select (make-temporary-filename/local))
    )))



(define working-file/p
  (make-parameter #f))

(define (send-state state)
  (define title (cdr (assoc 'title state)))
  (define note (cdr (assoc 'note state)))
  (define tags (cdr (assoc 'tags state)))
  (define target-extension (cdr (assoc 'target-extension state)))
  (define target-basename (cdr (assoc 'target-basename state)))
  (define mimetype (cdr (assoc 'mimetype state)))
  (define kind (cdr (assoc 'kind state)))
  (define series (cdr (assoc 'series state)))
  (define series? (case series ((yes) #t) ((no) #f) (else (fatal "Bad value for series ~s" series))))
  (define link? (case (cdr (assoc 'link? state)) ((yes) #t) ((no) #f) (else (fatal "Bad value for link? ~s" (cdr (assoc 'link? state))))))
  (define -temporary-file (cdr (assoc '-temporary-file state)))
  (define -text-content (cdr (assoc '-text-content state)))
  (define registry-dir (append-posix-path (get-root) default-db-path))
  (define source (and (a-weblink? -text-content) -temporary-file -text-content))
  (define key-value-pairs
    (append
     (if source (list (cons "source" source)) (list))
     (if (string? note) (list (cons "note" note)) (list))
     (if mimetype (list (cons "mimetype" mimetype)) (list))
     ))

  (define <date> #f)
  (define _11
    (unless (file-or-directory-exists? registry-dir)
      (make-directories registry-dir)))
  (define filename
    (and (string? target-extension)
         (string-append target-basename target-extension)))

  (define _delete-result
    (file-delete (working-file/p)))

  (define entry
    (if -temporary-file
        (tegfs-add-file
         -temporary-file filename title tags
         series? key-value-pairs
         <date>)
        (let ((<target> (if (equal? kind 'pasta) #f -text-content)))
          (tegfs-add
           <target> title tags
           series? key-value-pairs
           <date>))))

  (assq-or keyword-id entry (raisu 'impossible)))

(define (CLI::save/no-remote --link <savetext>)
  (define id
    (parameterize ((working-file/p (make-temporary-filename/local)))
      (let ((state (CLI::save::loop --link <savetext>)))
        (send-state state))))

  (dprintln "Saved as ~a" id))

(define (CLI::save/remote <remote> <savetext>)
  (define savetext
    (and <savetext>
         (case (classify-clipboard-text-content <savetext>)
           ((data) (raisu 'savetext-cannot-be-data <savetext>))
           ((link localfile) <savetext>)
           ((pasta) (tegfs-dump-clipboard/pasta <savetext>))
           (else (raisu 'unexpected-kind <savetext>)))))
  (define working-text
    (or savetext
        (tegfs-dump-clipboard)))
  (define <remote-id>
    (get-random-basename))

  (define kind (classify-clipboard-text-content working-text))

  (define remote-name
    (case kind
      ((localfile)
       (unless (file-or-directory-exists? working-text)
         (raisu 'file-must-have-been-created working-text))
       (let ((normalized (path-normalize working-text)))
         (unless (= 0 (run-syncproc "rsync" "--info=progress2" "--mkpath" "--partial" "--recursive" "--chmod=u=rwX,g=rX,o=" normalized (stringf "~a:tegfs-remote-hub/" <remote>)))
           (fatal "Syncing to remote failed"))
         (append-posix-path "tegfs-remote-hub" (path-get-basename normalized))))
      ((link) working-text)
      ((data pasta) (raisu 'impossible-kind kind working-text))
      (else (raisu 'unhandled-kind kind working-text))))

  (define temp-file (get-random-basename))
  (define temp-file-content (string-append (~s kind) ":" remote-name))
  (write-string-file temp-file temp-file-content)

  (unless (= 0 (run-syncproc "scp" "--" temp-file (stringf "~a:tegfs-remote-hub/~a" <remote> <remote-id>)))
    (file-delete temp-file)
    (fatal "Something went wrong on the other side"))
  (file-delete temp-file)

  (let ((s (system* "ssh" "-o" "SendEnv LANG" "-t" <remote> (stringf "exec /bin/sh -l -c \"exec tegfs save --from-remote ~a\"" <remote-id>))))
    (unless (= 0 (status:exit-val s))
      (fatal "Something went wrong on the other side")))

  (dprintln "Saved!"))

(define (CLI::save/from-remote <remote-id>)
  (define HOME (system-environment-get "HOME"))
  (define temp-file (append-posix-path HOME "tegfs-remote-hub" <remote-id>))
  (define temp-file-content
    (read-string-file temp-file))
  (define-values (kind/string col remote-name)
    (string-split-3 #\: temp-file-content))
  (define _12737123
    (when (string-null? col)
      (fatal "Client sent bad tegfs-remote-name")))
  (define kind (string->symbol kind/string))
  (define <savetext>
    (case kind
      ((localfile) (append-posix-path HOME remote-name))
      ((link) remote-name)
      ((data pasta) (fatal "Impossible real type: ~s" kind))
      (else (raisu 'unhandled-kind-in-server kind))))
  (dprintln "Remote file content: ~s" <savetext>)
  (CLI::save/no-remote #f <savetext>)
  (file-delete temp-file))

(define (CLI::save <remote> --from-remote <remote-id> --link <savetext>)
  (cond
   (<remote>
    (CLI::save/remote <remote> <savetext>))
   (--from-remote
    (CLI::save/from-remote <remote-id>))
   (else
    (CLI::save/no-remote --link <savetext>))))
