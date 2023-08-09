
(assert=
 '((ok audio video))

 (categorization-complete-selection
  "audio video image text"
  '((%choice "audio" "audio" "$")
    (%choice "video" "video" "$"))))



(assert=
 '((ok meme clip))

 (categorization-complete-selection
  "audio video image text"
  '((%choice "meme" "meme" "$")
    (%choice "clip" "clip" "$"))))




(assert=
 '((ok video image meme clip))

 (categorization-complete-selection
  "
		 audio video image text
		 audio : song recording
		 video : clip recording lecture
		 image : photo meme drawing
		 photo : selfie
		 text : book pasta
"

  '((%choice "meme" "meme" "$")
    (%choice "clip" "clip" "$"))))





(assert=
 '(image meme recording)

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

   '((%choice "meme" "meme" "$")
     (%choice "recording" "recording" "$")))
  (raisu 'type-error "Expected alist with 'ok")))





(assert=
 '((ok image meme recording)
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

  '((%choice "meme" "meme" "$")
    (%choice "recording" "recording" "$"))))





(assert=
 '(image meme recording)

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


  '((%choice "meme" "meme" "$")
    (%choice "meme" "meme" "$")
    (%choice "recording" "recording" "$")))
  (raisu 'type-error "Expected alist with 'ok")))





(assert=
 '((ok image meme recording)
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

  '((%choice "meme" "meme" "$")
    (%choice "meme" "meme" "$")
    (%choice "recording" "recording" "$"))))






(assert=
 '((ok image meme recording video))

 (categorization-complete-selection
  "
		 audio video image text
		 audio : song recording
		 video : clip recording lecture
		 image : photo meme drawing
		 photo : selfie
		 text : book pasta
"


  '((%choice "meme" "meme" "$")
    (%choice "recording" "recording" "$")
    (%choice "video" "video" "$"))))






(assert=
 '((ok video image meme clip))

 (categorization-complete-selection
  "
		 audio video image text
		 audio : song recording
		 video : cl*ip recording lecture
		 image : photo meme* drawing
		 photo : selfie
		 text : book pasta
"

  '((%choice "meme" "meme" "$")
    (%choice "clip" "clip" "$"))))







(assert=
 '((ok video image meme clip))

 (categorization-complete-selection
  "
		 audio video i*mage text
		 audio : song* recording*
		 video : cl*ip recording* lecture*
		 image : photo meme* drawing
		 photo : selfie*
		 text : book* pas*ta
"

  '((%choice "meme" "meme" "$")
    (%choice "clip" "clip" "$"))))
