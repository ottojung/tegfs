
(let ()
  (define-values (tree rules)
    (categorization-parse
     "audio video image text"))

  (assert= tree '((EUPHRATES-CFG-CLI-MAIN audio video image text)))
  (assert= rules '()))






(let ()
  (define-values (tree rules)
    (categorization-parse
     "
		 audio video image text
		 audio : song recording
		 video : clip recording lecture
		 image : photo meme drawing
		 photo : selfie
		 text : book pasta
	"))

  (assert=
   tree
   '((EUPHRATES-CFG-CLI-MAIN audio video image text)
     (audio song recording)
     (video clip recording lecture)
     (image photo meme drawing)
     (photo selfie)
     (text book pasta)))
  (assert= rules '()))




(let ()
  (define-values (tree rules)
    (categorization-parse
     "
		 audio video image text audio : song recording video : clip recording lecture image : photo meme drawing
	"))

  (assert=
   tree
   '((EUPHRATES-CFG-CLI-MAIN audio video image text)
     (audio song recording)
     (video clip recording lecture)
     (image photo meme drawing)))

  (assert= rules '()))




(let ()
  (define-values (tree rules)
    (categorization-parse ""))

  (assert= tree '())
  (assert= rules '()))
