(define-library (is-normal-tag-word?:parser:compiler) (import (tegfs is-normal-tag-word-huh-parser-definition)) (import (parser-compiler-generic)) (import (only (scheme base) begin quote)) (begin (parser-compiler/generic is-normal-tag-word?:parser:definition "is-normal-tag-word-huh-parser-implementation" (quote is-normal-tag-word?:parser:implementation))))