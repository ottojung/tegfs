
(assert=
 '((%choice "audio" "audio" "$")
   (%choice "image" "image" "$"))

 (categorization-translate-choices
  (make-tag-parser 0)
  (categorization-parse
   "audio* video image*")
  '(audio* image*)))


(assert=
 '((%choice "audio" "recording" "$")
   (%choice "video" "clip" "$"))


 (categorization-translate-choices
  (make-tag-parser 0)
  (categorization-parse
   "
     audio video
     audio : song recording*
     video : clip* recording lecture
    ")
  '(recording* clip*)))




(assert=
 '((%choice "audio" "recording" "$")
   (%choice "video" "clip" "$")
   (%choice "video" "lecture" "$"))

 (categorization-translate-choices
  (make-tag-parser 0)
  (categorization-parse
   "
     audio video
     audio : song recording*
     video : clip* recording lecture*
    ")
  '(recording* clip* lecture*)))




(assert=
 '((%choice "audio" "song" "$")
   (%choice "audio" "recording" "$")
   (%choice "video" "clip" "$"))

 (categorization-translate-choices
  (make-tag-parser 0)
  (categorization-parse
   "
     audio video
     audio : song* recording*
     video : clip* recording lecture
    ")
  '(song* recording* clip*)))



(assert=
 '((%choice "audio" "track" "$")
   (%choice "video" "movie" "$"))

 (categorization-translate-choices
  (make-tag-parser 0)
  (categorization-parse
   "
     audio video
     audio : song track*
     video : clip movie*
    ")
  '(track* movie*)))



(assert=
 '((%choice "audio" "song" "$")
   (%choice "audio" "track" "$")
   (%choice "video" "clip" "$"))

 (categorization-translate-choices
  (make-tag-parser 0)
  (categorization-parse
   "
     audio video
     audio : song* track*
     video : clip* movie
    ")
  '(song* track* clip*)))



(assert=
 '((%choice "audio" "song" "$")
   (%choice "audio" "track" "$")
   (%choice "video" "clip" "$")
   (%choice "video" "movie" "$"))

 (categorization-translate-choices
  (make-tag-parser 0)
  (categorization-parse
   "
     audio video
     audio : song* track*
     video : clip* movie*
    ")
  '(song* track* clip* movie*)))

(assert=
 '((%choice "audio" "audio" "$")
   (%choice "image" "image" "$"))

 (categorization-translate-choices
  (make-tag-parser 0)
  (categorization-parse
   "audio video image*")
  '(audio image*)))

(assert=
 '((%choice "image" "image" "$"))

 (categorization-translate-choices
  (make-tag-parser 0)
  (categorization-parse
   "audio video image*")
  '(audio* image*)))
