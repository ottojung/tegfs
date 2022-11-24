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

%var tegfs-edit-tags
%var tegfs-process-categorization-text

%use (comp) "./euphrates/comp.scm"
%use (compose-under) "./euphrates/compose-under.scm"
%use (curry-if) "./euphrates/curry-if.scm"
%use (fn) "./euphrates/fn.scm"
%use (list-deduplicate) "./euphrates/list-deduplicate.scm"
%use (list-find-first) "./euphrates/list-find-first.scm"
%use (list-last) "./euphrates/list-last.scm"
%use (list-map/flatten) "./euphrates/list-map-flatten.scm"
%use (list-or-map) "./euphrates/list-or-map.scm"
%use (list-singleton?) "./euphrates/list-singleton-q.scm"
%use (list->multiset multiset->list multiset-filter) "./euphrates/multiset.scm"
%use (CFG-CLI->CFG-AST) "./euphrates/parse-cfg-cli.scm"
%use (raisu) "./euphrates/raisu.scm"
%use (read-list) "./euphrates/read-list.scm"
%use (read-string-file) "./euphrates/read-string-file.scm"
%use (system-fmt) "./euphrates/system-fmt.scm"
%use (categorization-split) "./categorization-split.scm"
%use (tag-structure-sep1) "./tag-structure-sep1.scm"

(define (tegfs-edit-tags working-file)
  (unless working-file
    (raisu 'must-provide-working-file))

  (system-fmt "$EDITOR ~a" working-file)
  (tegfs-process-categorization-text (read-string-file working-file)))

(define unstar-symbol
  (comp symbol->string
        string->list
        (filter (negate (comp (equal? #\*))))
        list->string
        string->symbol))

(define type-symbol?
  (comp unstar-symbol
        symbol->string
        (string-suffix? ">")))

(define starred-symbol?
  (comp symbol->string string->list
        ((compose-under
          and
          (comp (list-or-map (lambda (c) (member c `(,#\* ,tag-structure-sep1)))))
          (comp list-last (equal? tag-structure-sep1) not)))))

;; transitive over type-symbols, but not a general transitive closure
(define (get-parents ast/flatten tag/starred)
  (let loop ((tag/starred tag/starred) (buf '()))
    (when (member tag/starred buf)
      (raisu 'cycle-detected! tag/starred buf))

    (apply
     append
     (filter
      identity
      (map
       (lambda (production)
         (and (member tag/starred (cdr production))
              (let ((immediate (car production)))
                (if (type-symbol? immediate)
                    (loop immediate (cons tag/starred buf))
                    (list immediate)))))
       ast/flatten)))))

(define (list-singleton? lst)
  (and (not (null? lst))
       (null? (cdr lst))))

;; transitive over symbols that have single parent
(define (get-parents/transitive/reflexive ast/flatten ast/flatten/unstarred closed tag/starred)
  (define (get initial)
    (let loop ((tag/starred initial) (ast ast/flatten) (buf '()))
      (when (member tag/starred buf)
        (raisu 'cycle-detected! tag/starred buf))

      (let* ((immediate-parents
              (filter
               identity
               (map
                (lambda (production)
                  (and (member tag/starred (cdr production))
                       (car production)))
                ast)))
             (parent/starred
              (if (list-singleton? immediate-parents)
                  (car immediate-parents)
                  (list-find-first (fn member % closed) #f immediate-parents))))
        (if parent/starred
            (let ((parent (unstar-symbol parent/starred)))
              (cons parent (loop parent ast/flatten/unstarred (cons tag/starred buf))))
            '()))))

  (define ret
    ((curry-if null? (const (get (unstar-symbol tag/starred))))
     (get tag/starred)))
  (cons tag/starred ret))

;; Returns `((ok ,list-of-chosen-tags)
;;           (ambiguous ,list-of-ambiguous-tags-with-parents)
;;           (duplicates ,list-of-duplicates))
;; But ambiguous or duplicates may not be present.
(define (tegfs-process-categorization-text text)
  ;; FIXME: dont actually split - the rules are in a separate file
  ;; TODO: allow to edit rules as well
  (define-values (cfg-part rules-part)
    (categorization-split text))

  (define words
    (with-input-from-string cfg-part
      (lambda _
        (read-list (current-input-port)))))

  (define words-flat
    (list-map/flatten (curry-if pair? identity list) words))

  (define ast
    (CFG-CLI->CFG-AST words-flat))

  (define ast/flatten
    (map (lambda (production)
           (cons (car production)
                 (list-deduplicate
                  (apply append (cdr production)))))
         ast))

  (define starred
    (filter starred-symbol? words-flat))

  (define starred/unstarred
    (map unstar-symbol starred))

  (define starred-productions
    (filter starred-symbol? (map car ast)))

  (define starred-non-productions
    (apply
     append
     (map (lambda (production)
            (filter starred-symbol? (cdr production)))
          ast/flatten)))

  (define ast/flatten/unstarred
    (map (lambda (production)
           (cons
            (unstar-symbol (car production))
            (list-deduplicate
             (map unstar-symbol (cdr production)))))
         ast/flatten))

  (define main-production
    (car (car ast/flatten)))

  (define (closure closed tag/starred)
    (get-parents/transitive/reflexive ast/flatten ast/flatten/unstarred closed tag/starred))

  (define transitive-closures
    (map (comp (closure '())) starred))

  (define closed
    (list-deduplicate
     (map unstar-symbol
          (apply append
                 (filter
                  (comp (member main-production))
                  transitive-closures)))))

  (define (ambiguous? tag/starred)
    (define cl (closure closed tag/starred))
    (not (member main-production cl)))

  (define ambiguous
    (filter ambiguous? starred))

  (define ok-tags
    (filter
     (negate type-symbol?)
     (filter
      (negate (comp (equal? main-production)))
      (list-deduplicate
       (map unstar-symbol (apply append transitive-closures))))))

  (define ambiguous-tags
    (and (not (null? ambiguous))
         (map
          (lambda (amb)
            (define unstarred (unstar-symbol amb))
            (cons unstarred
                  (map unstar-symbol (get-parents ast/flatten/unstarred unstarred))))
          ambiguous)))

  (define duplicates
    (let* ((S (list->multiset starred/unstarred))
           (S* (multiset-filter
                S (lambda (key value) (> value 1))))
           (L (multiset->list S*)))
      (and (not (null? L))
           L)))

  (append
   `((ok . ,ok-tags))
   (if ambiguous-tags
       (list (cons 'ambiguous ambiguous-tags))
       (list))
   (if duplicates
       (list (cons 'duplicates duplicates))
       (list))))
