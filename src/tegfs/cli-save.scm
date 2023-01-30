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
    :use-module ((euphrates dprintln) :select (dprintln))
    :use-module ((euphrates file-delete) :select (file-delete))
    :use-module ((euphrates file-or-directory-exists-q) :select (file-or-directory-exists?))
    :use-module ((euphrates make-directories) :select (make-directories))
    :use-module ((euphrates path-get-basename) :select (path-get-basename))
    :use-module ((euphrates path-normalize) :select (path-normalize))
    :use-module ((euphrates raisu) :select (raisu))
    :use-module ((euphrates read-string-file) :select (read-string-file))
    :use-module ((euphrates string-split-3) :select (string-split-3))
    :use-module ((euphrates system-environment) :select (system-environment-get))
    :use-module ((euphrates system-fmt) :select (system-fmt))
    :use-module ((euphrates tilda-s) :select (~s))
    :use-module ((euphrates write-string-file) :select (write-string-file))
    :use-module ((tegfs a-weblink-q) :select (a-weblink?))
    :use-module ((tegfs add) :select (tegfs-add))
    :use-module ((tegfs clipboard) :select (classify-clipboard-text-content))
    :use-module ((tegfs dump-clipboard) :select (tegfs-dump-clipboard tegfs-dump-clipboard/pasta))
    :use-module ((tegfs fatal) :select (fatal))
    :use-module ((tegfs get-random-basename) :select (get-random-basename))
    :use-module ((tegfs get-root) :select (get-root))
    :use-module ((tegfs make-temporary-filename-local) :select (make-temporary-filename/local))
    :use-module ((tegfs cli-save-loop) :select (CLI::save::loop)))))



(define working-file/p
  (make-parameter #f))

(define (send-state state)
  (define title (cdr (assoc 'title state)))
  (define note (cdr (assoc 'note state)))
  (define tags (cdr (assoc 'tags state)))
  (define target-extension (cdr (assoc 'target-extension state)))
  (define target-basename (cdr (assoc 'target-basename state)))
  (define mimetype (cdr (assoc 'mimetype state)))
  (define real-type (cdr (assoc 'real-type state)))
  (define series (cdr (assoc 'series state)))
  (define series? (case series ((yes) #t) ((no) #f) (else (fatal "Bad value for series ~s" series))))
  (define link? (case (cdr (assoc 'link? state)) ((yes) #t) ((no) #f) (else (fatal "Bad value for link? ~s" (cdr (assoc 'link? state))))))
  (define registry-file (cdr (assoc 'registry-file state)))
  (define -temporary-file (cdr (assoc '-temporary-file state)))
  (define -text-content (cdr (assoc '-text-content state)))
  (define registry-dir (append-posix-path (get-root) (dirname registry-file)))
  (define source (and (a-weblink? -text-content) -temporary-file -text-content))
  (define key-value-pairs
    (append
     (if source (list (cons "source" source)) (list))
     (if (string? note) (list (cons "note" note)) (list))
     ))

  (define <date> #f)
  (define _11
    (unless (file-or-directory-exists? registry-dir)
      (make-directories registry-dir)))
  (define <target>
    (cond
     (-temporary-file
      (let* ((target-name0 (string-append target-basename target-extension))
             (target-name (if (file-or-directory-exists?
                               (append-posix-path registry-dir target-name0))
                              (string-append target-basename "-" (get-random-basename) target-extension)
                              target-name0))
             (target-fullname (append-posix-path registry-dir target-name)))
        (rename-file -temporary-file target-fullname)
        (when link?
          (symlink target-fullname -temporary-file))
        target-name))
     ((equal? real-type 'pasta) #f)
     (else -text-content)))

  (file-delete (working-file/p))

  (tegfs-add
   <target> title tags
   series? key-value-pairs
   registry-file <date>))

(define (CLI::save/no-remote --link <savetext>)
  (parameterize ((working-file/p (make-temporary-filename/local)))
    (let ((state (CLI::save::loop --link <savetext>)))
      (send-state state)))

  (dprintln "Saved!"))

(define (CLI::save/remote <remote> <savetext>)
  (define savetext
    (and <savetext>
         (case (classify-clipboard-text-content <savetext>)
           ((data) (raisu 'savetext-cannot-be-data <savetext>))
           ((link localfile) <savetext>)
           ((pasta) (tegfs-dump-clipboard/pasta <savetext>))
           (else (raisu 'unexpected-real-type <savetext>)))))
  (define working-text
    (or savetext
        (tegfs-dump-clipboard)))
  (define <remote-id>
    (get-random-basename))

  (define real-type (classify-clipboard-text-content working-text))

  (define remote-name
    (case real-type
      ((localfile)
       (unless (file-or-directory-exists? working-text)
         (raisu 'file-must-have-been-created working-text))
       (let ((normalized (path-normalize working-text)))
         (unless (= 0 (system-fmt "rsync --info=progress2 --mkpath --partial --recursive --chmod=u=rwX,g=rX,o= ~a ~a:tegfs-remote-hub/" normalized <remote>))
           (fatal "Syncing to remote failed"))
         (append-posix-path "tegfs-remote-hub" (path-get-basename normalized))))
      ((link) working-text)
      ((data pasta) (raisu 'impossible-real-type real-type working-text))
      (else (raisu 'unhandled-real-type real-type working-text))))

  (define temp-file (get-random-basename))
  (define temp-file-content (string-append (~s real-type) ":" remote-name))
  (write-string-file temp-file temp-file-content)

  (unless (= 0 (system-fmt "exec scp ~a ~a:tegfs-remote-hub/~a" temp-file <remote> <remote-id>))
    (file-delete temp-file)
    (fatal "Something went wrong on the other side"))
  (file-delete temp-file)

  (unless (= 0 (system-fmt "exec ssh -t ~a \"exec /bin/sh -l -c \\\"exec tegfs save --from-remote ~a\\\"\"" <remote> <remote-id>))
    (fatal "Something went wrong on the other side"))

  (dprintln "Saved!"))

(define (CLI::save/from-remote <remote-id>)
  (define HOME (system-environment-get "HOME"))
  (define temp-file (append-posix-path HOME "tegfs-remote-hub" <remote-id>))
  (define temp-file-content
    (read-string-file temp-file))
  (define-values (real-type/string col remote-name)
    (string-split-3 #\: temp-file-content))
  (define _12737123
    (when (string-null? col)
      (fatal "Client sent bad tegfs-remote-name")))
  (define real-type (string->symbol real-type/string))
  (define <savetext>
    (case real-type
      ((localfile) (append-posix-path HOME remote-name))
      ((link) remote-name)
      ((data pasta) (fatal "Impossible real type: ~s" real-type))
      (else (raisu 'unhandled-real-type-in-server real-type))))
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
