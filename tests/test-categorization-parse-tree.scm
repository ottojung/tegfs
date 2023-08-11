
(let ()
  (define tree
    (categorization-parse-tree
     "audio video image text"))

  (assert=
   tree
   '((EUPHRATES-CFG-CLI-MAIN audio video image text))))




(let ()
  (define tree
    (categorization-parse-tree
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
     (text book pasta))))




(let ()
  (define tree
    (categorization-parse-tree
     "
		 audio video image text audio : song recording video : clip recording lecture image : photo meme drawing
	"))

  (assert=
   tree
   '((EUPHRATES-CFG-CLI-MAIN audio video image text)
     (audio song recording)
     (video clip recording lecture)
     (image photo meme drawing))))




(let ()
  (define tree
    (categorization-parse-tree ""))

  (assert= tree '()))
