;;;; Copyright (C) 2022, 2023  Otto Jung
;;;; This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU Affero General Public License for more details. You should have received a copy of the GNU Affero General Public License along with this program.  If not, see <https://www.gnu.org/licenses/>.

(define (send-state state)
  (define title/0 (cdr (assoc 'title state)))
  (define title (if (equal? title/0 'none) #f title/0))
  (define choices (assq-or 'tags-choices state '()))
  (define selected-tags (assq-or 'tags state '()))
  (define inferred-tags/0 (assq-or 'inferred-tags state '()))
  (define inferred-tags (if (equal? 'none inferred-tags/0) '() inferred-tags/0))
  (define tags inferred-tags)
  (define note (cdr (assoc 'note state)))
  (define date (assq-or 'date state #f))
  (define target-extension (cdr (assoc 'target-extension state)))
  (define target-basename (cdr (assoc 'target-basename state)))
  (define mimetype (cdr (assoc 'mimetype state)))
  (define kind (cdr (assoc 'kind state)))
  (define series (cdr (assoc 'series state)))
  (define series? (case series ((yes) #t) ((no) #f) (else (fatal "Bad value for series ~s" series))))
  (define link? (case (cdr (assoc 'link? state)) ((yes) #t) ((no) #f) (else (fatal "Bad value for link? ~s" (cdr (assoc 'link? state))))))
  (define dirpreview?/0 (assq-or 'dirpreview? state #f))
  (define dirpreview? (case dirpreview?/0 ((yes) #t) ((no) #f) (else (fatal "Bad value for dirpreview? ~s" dirpreview?/0))))
  (define diropen?/0 (assq-or 'diropen? state #f))
  (define diropen? (case diropen?/0 ((yes) #t) ((no) #f) (else (fatal "Bad value for diropen? ~s" diropen?/0))))
  (define -temporary-file (cdr (assoc '-temporary-file state)))
  (define -text-content (cdr (assoc '-text-content state)))
  (define registry-dir (append-posix-path (get-root) default-db-path))
  (define source/0 (cdr (assoc 'source state)))
  (define source (if (equal? 'none source/0) #f source/0))
  (define additional-properties (cdr (assoc 'additional-properties state)))
  (define key-value-pairs
    (append
     (list (cons "choices" choices))
     (if source (list (cons "source" source)) (list))
     (if (string? note) (list (cons "note" note)) (list))
     (if mimetype (list (cons "mimetype" mimetype)) (list))
     (if dirpreview? (list (list "dirpreview")) (list))
     (if diropen? (list (list "diropen")) (list))
     additional-properties
     ))

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
         date)
        (let ((<target> (if (equal? kind 'pasta) #f -text-content)))
          (tegfs-add
           <target> title tags
           series? key-value-pairs
           date))))

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
         --unsure-if-diropen
         --dirpreview
         --no-dirpreview
         --download
         --no-download
         --unsure-if-download
         --source <source>
         --no-source
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
             --unsure-if-diropen
             --dirpreview
             --no-dirpreview
             --download
             --no-download
             --unsure-if-download
             --source <source>
             --no-source
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
