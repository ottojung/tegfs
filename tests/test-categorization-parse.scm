
(assert=
 (categorization-parse
  "audio video image text")

 '((EUPHRATES-CFG-CLI-MAIN audio video image text)))

(assert=
 (categorization-parse
  "
		 audio video image text
		 audio : song recording
		 video : clip recording lecture
		 image : photo meme drawing
		 photo : selfie
		 text : book pasta
")

 '((EUPHRATES-CFG-CLI-MAIN audio video image text)
   (audio song recording)
   (video clip recording lecture)
   (image photo meme drawing)
   (photo selfie)
   (text book pasta)))

(assert=
 (categorization-parse
  "
		 audio video image text audio : song recording video : clip recording lecture image : photo meme drawing
")

 '((EUPHRATES-CFG-CLI-MAIN audio video image text)
   (audio song recording)
   (video clip recording lecture)
   (image photo meme drawing)))

(assert=
 (categorization-parse
  "")

 '())
