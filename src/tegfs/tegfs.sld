
(define-library
  (tegfs tegfs)
  (import
    (only (euphrates current-program-path-p)
          current-program-path/p))
  (import
    (only (euphrates define-cli)
          define-cli:show-help
          with-cli))
  (import (only (euphrates stringf) stringf))
  (import (only (euphrates tilda-a) ~a))
  (import
    (only (euphrates with-randomizer-seed)
          with-randomizer-seed))
  (import
    (only (tegfs CLI-categorization-list-categories)
          CLI::categorization-list-categories))
  (import
    (only (tegfs CLI-categorization-show)
          CLI::categorization-show))
  (import
    (only (tegfs CLI-prolog-print-file)
          CLI::prolog-print-file))
  (import
    (only (tegfs CLI-prolog-repl) CLI::prolog-repl))
  (import
    (only (tegfs categorize) tegfs-categorize/parse))
  (import (only (tegfs cli-delete) CLI::delete))
  (import (only (tegfs cli-edit) CLI::edit))
  (import (only (tegfs cli-print) CLI::print))
  (import (only (tegfs cli-query) CLI::query))
  (import
    (only (tegfs cli-remote) CLI::remote/parse))
  (import (only (tegfs cli-save) CLI::save))
  (import
    (only (tegfs cli-share) CLI::share/parse))
  (import
    (only (tegfs cli-show-license) CLI::show-license))
  (import
    (only (tegfs cli-show-warranty)
          CLI::show-warranty))
  (import (only (tegfs cli-talk) CLI::talk))
  (import (only (tegfs config) tegfs-config/parse))
  (import
    (only (tegfs dump-clipboard)
          tegfs-dump-clipboard/parse))
  (import (only (tegfs fatal) fatal))
  (import (only (tegfs get-root) get-root/default))
  (import
    (only (tegfs get-texteditor)
          get-texteditor/default))
  (import (only (tegfs get) tegfs-get/parse))
  (import
    (only (tegfs keyword-target) keyword-target))
  (import
    (only (tegfs make-thumbnails)
          tegfs-make-thumbnails/parse))
  (import (only (tegfs root-p) root/p))
  (import
    (only (tegfs tegfs-version) tegfs-version))
  (import (only (tegfs texteditor-p) texteditor/p))
  (import
    (only (tegfs verbosity-level-p)
          verbosity-level/p))
  (import
    (only (tegfs web-server) tegfs-serve/parse))
  (import
    (only (scheme base)
          /
          and
          begin
          cond
          define
          else
          newline
          or
          parameterize
          quote))
  (import (only (scheme read) read))
  (import (only (scheme write) display))
  (cond-expand
    (guile (import (only (srfi srfi-1) delete)))
    (else (import (only (srfi 1) delete))))
  (cond-expand
    (guile (import (only (srfi srfi-42) :)))
    (else (import (only (srfi 42) :))))
  (cond-expand
    (guile (import (only (guile) include-from-path))
           (begin (include-from-path "tegfs/tegfs.scm")))
    (else (include "tegfs.scm"))))
