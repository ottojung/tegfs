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
    :use-module ((euphrates file-delete) :select (file-delete))
    :use-module ((euphrates file-or-directory-exists-q) :select (file-or-directory-exists?))
    :use-module ((euphrates get-command-line-arguments) :select (get-command-line-arguments))
    :use-module ((euphrates make-directories) :select (make-directories))
    :use-module ((euphrates path-get-basename) :select (path-get-basename))
    :use-module ((euphrates path-normalize) :select (path-normalize))
    :use-module ((euphrates raisu) :select (raisu))
    :use-module ((euphrates run-syncproc) :select (run-syncproc))
    :use-module ((euphrates stringf) :select (stringf))
    :use-module ((tegfs add) :select (tegfs-add tegfs-add-file))
    :use-module ((tegfs cli-remote) :select (CLI::remote))
    :use-module ((tegfs cli-save-loop) :select (CLI::save::loop))
    :use-module ((tegfs cli-save-working-file-p) :select (CLI::save-working-file/p))
    :use-module ((tegfs cli-share) :select (CLI::share))
    :use-module ((tegfs clipboard) :select (classify-clipboard-text-content))
    :use-module ((tegfs default-db-path) :select (default-db-path))
    :use-module ((tegfs dump-clipboard) :select (tegfs-dump-clipboard tegfs-dump-clipboard/pasta))
    :use-module ((tegfs fatal) :select (fatal))
    :use-module ((tegfs get-random-basename) :select (get-random-basename))
    :use-module ((tegfs get-root) :select (get-root))
    :use-module ((tegfs keyword-id) :select (keyword-id))
    :use-module ((tegfs make-temporary-filename-local) :select (make-temporary-filename/local))
    )))

(define (send-state state)
  (define title/0 (cdr (assoc 'title state)))
  (define title (if (equal? title/0 'none) #f title/0))
  (define selected (assq-or 'tags state '()))
  (define inferred-tags/0 (assq-or 'inferred-tags state '()))
  (define inferred-tags (if (equal? 'none inferred-tags/0) '() inferred-tags/0))
  (define tags (append selected inferred-tags))
  (define note (cdr (assoc 'note state)))
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
  (define source/0 (cdr (assoc 'source state)))
  (define source (if (equal? 'none source/0) #f source/0))
  (define additional-properties (cdr (assoc 'additional-properties state)))
  (define key-value-pairs
    (append
     (list (cons "selected" selected))
     (if source (list (cons "source" source)) (list))
     (if (string? note) (list (cons "note" note)) (list))
     (if mimetype (list (cons "mimetype" mimetype)) (list))
     additional-properties
     ))

  (define <date> #f)
  (define _11
    (unless (file-or-directory-exists? registry-dir)
      (make-directories registry-dir)))
  (define filename
    (and (string? target-extension)
         (string-append target-basename target-extension)))

  (define _delete-result
    (file-delete (CLI::save-working-file/p)))

  (define entry
    (if -temporary-file
        (tegfs-add-file
         -temporary-file filename link?
         title tags
         series? key-value-pairs
         <date>)
        (let ((<target> (if (equal? kind 'pasta) #f -text-content)))
          (tegfs-add
           <target> title tags
           series? key-value-pairs
           <date>))))

  (assq-or keyword-id entry (raisu 'impossible)))

(define (CLI::save

         --content <content>
         --kind <kind>
         --interactive
         --no-interactive
         --title <title>
         --tag <tag...>
         --series
         --no-series
         --diropen
         --no-diropen
         --dirpreview
         --no-dirpreview
         --download
         --no-download
         --source <source>
         --no-source
         --unsure-if-download
         --target <add-target>
         --mimetype <mimetype>
         --note <note>
         --link
         --share <share-duration>
         --remote <remote>
         --no-remote
         --date <date>
         --key <key...> <value...>

         )
  (define (CLI::save/no-remote)
    (define id
      (parameterize ((CLI::save-working-file/p (make-temporary-filename/local)))
        (let ()
          (define state
            (CLI::save::loop

             --content <content>
             --kind <kind>
             --interactive
             --no-interactive
             --title <title>
             --tag <tag...>
             --series
             --no-series
             --diropen
             --no-diropen
             --dirpreview
             --no-dirpreview
             --download
             --no-download
             --source <source>
             --no-source
             --unsure-if-download
             --target <add-target>
             --mimetype <mimetype>
             --note <note>
             --link
             --share <share-duration>
             --remote <remote>
             --no-remote
             --date <date>
             --key <key...> <value...>

             ))

          (send-state state))))

    (if --share
        (display (CLI::share <share-duration> id))
        (display id))
    (newline))

  (define (CLI::save/remote)
    (define savetext
      (and <content>
           (case (classify-clipboard-text-content <content>)
             ((data) (raisu 'savetext-cannot-be-data <content>))
             ((link localfile) <content>)
             ((pasta) (tegfs-dump-clipboard/pasta <content>))
             (else (raisu 'unexpected-kind <content>)))))
    (define working-text
      (or savetext
          (tegfs-dump-clipboard)))
    (define <remote-id>
      (get-random-basename))

    (define kind
      (or <kind>
          (classify-clipboard-text-content working-text)))

    (define remote-content
      (case kind
        ((localfile)
         (unless (file-or-directory-exists? working-text)
           (raisu 'file-must-have-been-created working-text))
         (let ((normalized (path-normalize working-text)))
           (unless (= 0 (run-syncproc "rsync" "--info=progress2" "--mkpath" "--partial" "--recursive" "--chmod=u=rwX,g=rX,o=" normalized (stringf "~a:tegfs-remote-hub/" <remote>)))
             (fatal "Syncing to remote failed"))
           (append-posix-path "tegfs-remote-hub" (path-get-basename normalized))))
        ((link) working-text)
        ((data pasta) (raisu 'unexpected-save-kind kind working-text))
        (else (raisu 'unhandled-kind kind working-text))))

    (define new-args
      (append
       (get-command-line-arguments)
       (list
        "--no-remote"
        "--content" remote-content)))

    (CLI::remote <remote> new-args))

  (cond
   (--remote
    (CLI::save/remote))
   (else
    (CLI::save/no-remote))))
