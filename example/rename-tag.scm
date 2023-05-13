;; This is an example that renames tag "image" to "picture"
;;  by making modifications in every entry of every registry

(cond-expand
 (guile
  (define-module (example rename-tag)
    :use-module ((euphrates assoc-or) :select (assoc-or))
    :use-module ((euphrates assoc-set-value) :select (assoc-set-value))
    :use-module ((euphrates curry-if) :select (curry-if))
    :use-module ((euphrates comp) :select (comp))
    :use-module ((tegfs entries-map-bang) :select (entries-map!)))))

(define from 'image)
(define to 'picture)

(entries-map!
 (lambda (entry)
   (define tags (assoc-or 'tags entry '()))
   (define new-tags
     (map (curry-if (comp (equal? from)) (const to)) tags))
   (assoc-set-value 'tags new-tags entry)))
