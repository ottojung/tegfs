
(assert=
 '((ok video audio))

 (categorization-complete-selection
  "audio video image text"
  '(audio video)))

(assert=
 '((ok clip meme))

 (categorization-complete-selection
  "audio video image text"
  '(meme clip)))

(assert=
 '((ok video image clip meme))

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
 '(image recording meme)

 (assq-or
  'ok
  (categorization-complete-selection
   "
		 audio video image text
		 audio : song recording
		 video : clip recording lecture
		 image : photo meme drawing
		 photo : selfie
		 text : book pasta
"

   '(meme recording))
  (raisu 'type-error "Expected alist with 'ok")))

(assert=
 '((ok image recording meme)
   (ambiguous (recording video audio)))

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
 '(image recording meme)

 (assq-or
  'ok
  (categorization-complete-selection
   "
		 audio video image text
		 audio : song recording
		 video : clip recording lecture
		 image : photo meme drawing
		 photo : selfie
		 text : book pasta
"

   '(meme meme recording))
  (raisu 'type-error "Expected alist with 'ok")))

(assert=
 '((ok image recording meme)
   (ambiguous (recording video audio))
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
