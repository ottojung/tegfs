
(assert=
 '((ok meme image clip video))

 (categorization-complete-selection
  "
		 audio video image text
		 audio : song recording
		 video : clip recording lecture
		 image : photo meme drawing
		 photo : selfie
		 text : book pasta
"

  '(meme clip)))

(assert=
 '((ok meme image recording)
   (ambiguous (recording audio video)))

 (categorization-complete-selection
  "
		 audio video image text
		 audio : song recording
		 video : clip recording lecture
		 image : photo meme drawing
		 photo : selfie
		 text : book pasta
"

  '(meme recording)))

(assert=
 '((ok meme image recording)
   (ambiguous (recording audio video))
   (duplicates meme))

 (categorization-complete-selection
  "
		 audio video image text
		 audio : song recording
		 video : clip recording lecture
		 image : photo meme drawing
		 photo : selfie
		 text : book pasta
"

  '(meme meme recording)))
