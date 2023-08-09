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
  (define-module (tegfs tegfs)
    :use-module ((euphrates current-program-path-p) :select (current-program-path/p))
    :use-module ((euphrates define-cli) :select (define-cli:show-help with-cli))
    :use-module ((euphrates stringf) :select (stringf))
    :use-module ((euphrates with-randomizer-seed) :select (with-randomizer-seed))
    :use-module ((tegfs categorize) :select (tegfs-categorize/parse))
    :use-module ((tegfs cli-delete) :select (CLI::delete))
    :use-module ((tegfs cli-edit) :select (CLI::edit))
    :use-module ((tegfs cli-print) :select (CLI::print))
    :use-module ((tegfs cli-query) :select (CLI::query))
    :use-module ((tegfs cli-remote) :select (CLI::remote/parse))
    :use-module ((tegfs cli-save) :select (CLI::save))
    :use-module ((tegfs cli-share) :select (CLI::share/parse))
    :use-module ((tegfs cli-show-license) :select (CLI::show-license))
    :use-module ((tegfs cli-show-warranty) :select (CLI::show-warranty))
    :use-module ((tegfs cli-talk) :select (CLI::talk))
    :use-module ((tegfs config) :select (tegfs-config/parse))
    :use-module ((tegfs dump-clipboard) :select (tegfs-dump-clipboard/parse))
    :use-module ((tegfs get-root) :select (get-root/default))
    :use-module ((tegfs get-texteditor) :select (get-texteditor/default))
    :use-module ((tegfs get) :select (tegfs-get/parse))
    :use-module ((tegfs keyword-diropen) :select (keyword-diropen))
    :use-module ((tegfs keyword-dirpreview) :select (keyword-dirpreview))
    :use-module ((tegfs keyword-target) :select (keyword-target))
    :use-module ((tegfs make-thumbnails) :select (tegfs-make-thumbnails/parse))
    :use-module ((tegfs prolog) :select (tegfs-prolog/parse))
    :use-module ((tegfs root-p) :select (root/p))
    :use-module ((tegfs tegfs-version) :select (tegfs-version))
    :use-module ((tegfs texteditor-p) :select (texteditor/p))
    :use-module ((tegfs web-server) :select (tegfs-serve/parse))
    )))


(define (main)
  (parameterize ((current-program-path/p "tegfs"))
    (with-cli
     (MAIN
      MAIN : OPT* COMMAND
      /      --help
      /      --version
      /      license
      /      warranty

      COMMAND
      :      add ADDOPT+
      /      get GETARGS
      /      delete DELETEARGS
      /      print PRINTARGS
      /      edit EDITARGS
      /      share SHARETAGS
      /      query QUERYARGS
      /      config CONFIGOPT
      /      serve
      /      categorize
      /      remote <remote> MAIN
      /      talk TALKOPTS*
      /      prolog
      /      make-thumbnails THUMBOPT
      /      dump-clipboard

      ADDOPT : --content <content>
      /        --title <title>
      /        --tag <tag...>
      /        --series
      /        --no-series
      /        --kind <kind>
      /        --interactive
      /        --no-interactive
      /        --diropen
      /        --no-diropen
      /        --dirpreview
      /        --no-dirpreview
      /        --download
      /        --no-download
      /        --source <source>
      /        --no-source
      /        --unsure-if-download
      /        --target <add-target>
      /        --mimetype <mimetype>
      /        --note <note>
      /        --link
      /        --remote <remote>
      /        --share SHAREOPT?
      /        --no-remote
      /        --date <date>
      /        --key <key...> <value...>
      QUERYARGS : QUERYOPT* FIN? QUERYQ*
      QUERYOPT : --diropen
      /          --no-diropen
      /          --dirpreview
      /          --no-dirpreview
      /          FORMAT
      FORMAT : --format <format> / --sexp-format
      QUERYQ : <query...>
      GETARGS : FORMAT? <entry-id>
      DELETEARGS : MAYBEKEEPFILES? <entry-id>
      PRINTARGS : <entry-id>
      EDITARGS : <entry-id>
      SHARETAGS : SHAREOPT* <entry-id>
      SHAREOPT : --for-duration <share-duration>
      MAYBEKEEPFILES : --keep-files / --no-keep-files
      TALKOPTS : --web
      THUMBOPT : FIN? <target> <output>
      CONFIGOPT  : CONFIGFORK
      CONFIGFORK : get CONFIGKEY*
      /            set CONFIGKEY* CONFIGVAL
      CONFIGKEY : <key...>
      CONFIGVAL : --password <value> / FIN? <value>
      OPT : --root <root>
      /     --texteditor <texteditor>
      /     --seed <seed>
      FIN : --
      )

     :default (<root> (get-root/default))
     :default (<texteditor> (get-texteditor/default))

     :synonym (--version -v version)
     :synonym (license copying)

     :default (<share-duration> "30 minutes")
     :help (<share-duration> "For how long chosen entry should be shared.")

     :default (--no-series #t)
     :exclusive (--no-series --series)

     :default (--no-interactive #t)
     :exclusive (--no-interactive --interactive)

     :default (--sexp-format #t)
     :exclusive (--sexp-format --format)

     :default (--diropen #t)
     :exclusive (--diropen --no-diropen)

     :default (--unsure-if-download #t)
     :exclusive (--unsure-if-download --download --no-download)

     :default (--no-source #t)
     :exclusive (--no-source --source)

     :default (--no-dirpreview #t)
     :exclusive (--no-dirpreview --dirpreview)

     :default (--no-remote #t)
     :exclusive (--no-remote --remote)

     :default (--no-share #t)
     :exclusive (--no-share --share)

     :default (--no-link #t)
     :exclusive (--no-link --link)

     :default (--keep-files #t)
     :exclusive (--keep-files --no-keep-files)
     :synonym (--no-keep-files --delete-files-too)

     :synonym (get read)
     :synonym (get-user read-user)
     :synonym (set update)
     :synonym (set-user update-user)

     :type (<kind> '(localfile link pasta data))

     :type (<seed> 'number)

     :help (print (stringf "Print the contents of entry's ~a, or nothing if entry is not a file entry" keyword-target))
     :help (edit (stringf "Edit the contents of entry's ~a, or exit with an error if entry is not a file entry" keyword-target))
     :help (<remote> "A remote address like 'user1@example.com'.")
     :help (--diropen (stringf "Acknowledge <~a> property by treating elements of the ~a directory as entries having the same tags as the original entry." keyword-diropen keyword-target))
     :help (--dirpreview (stringf "Acknowledge <~a> property by treating elements of the ~a directory as entries having the same tags as the original entry." keyword-dirpreview keyword-target))
     :help (<content> "This could be a filename, a URL, or simply text. TegFS will try to figure out the type.")
     :help (<kind> (stringf "This forces ~s to be recognized as of certain type." (quote <content>)))
     :help (--share (stringf "Instead of returning the id, returns the url to saved file"))

     (with-randomizer-seed
      (or <seed> 'random)
      (parameterize ((root/p <root>) (texteditor/p <texteditor>))
        (cond
         (remote (CLI::remote/parse <remote>))
         (--version (display tegfs-version) (newline))
         (--help (define-cli:show-help))
         (add (CLI::save

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
         (categorize (tegfs-categorize/parse))
         (serve (tegfs-serve/parse))
         (prolog (tegfs-prolog/parse))
         (query (CLI::query --diropen --dirpreview --sexp-format <format> <query...>))
         ((and get <entry-id>) (tegfs-get/parse <format> <entry-id>))
         (delete (CLI::delete <entry-id> --keep-files))
         (print (CLI::print <entry-id>))
         (edit (CLI::edit <entry-id>))
         (share (CLI::share/parse <share-duration> <entry-id>))
         (talk (CLI::talk --web))
         (make-thumbnails (tegfs-make-thumbnails/parse <target> <output>))
         (config (tegfs-config/parse get set (or <key...> '()) --password <value>))
         (dump-clipboard (tegfs-dump-clipboard/parse))
         (license (CLI::show-license))
         (warranty (CLI::show-warranty))
         (else
          (display "Impossible") (newline)
          (exit 1))))))))

(main)
