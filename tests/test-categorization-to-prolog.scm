
(assert=
 '(((audio X) (%choice "audio" ANYSYM X))
   ((video X) (%choice "video" ANYSYM X))
   ((image X) (%choice "image" ANYSYM X))
   ((text X) (%choice "text" ANYSYM X)))

 (categorization->prolog
  (categorization-parse-tree
   "audio video image text")))

(assert=
 '(((audio X) (%choice "audio" ANYSYM X))
   ((audio X) (song X))
   ((song X) (%choice "song" ANYSYM X))
   ((video X) (%choice "video" ANYSYM X))
   ((video X) (clip X))
   ((clip X) (%choice "clip" ANYSYM X))
   ((recording X) (%choice "recording" ANYSYM X))
   ((video X) (lecture X))
   ((lecture X) (%choice "lecture" ANYSYM X)))

 (categorization->prolog
  (categorization-parse-tree
   "
		 audio video
		 audio : song recording
		 video : clip recording lecture
")))

(assert=
 '(((audio X) (%choice "audio" ANYSYM X))
   ((audio X) (song X))
   ((song X) (%choice "song" ANYSYM X))
   ((video X) (%choice "video" ANYSYM X))
   ((video X) (clip X))
   ((clip X) (%choice "clip" ANYSYM X))
   ((recording X) (%choice "recording" ANYSYM X))
   ((video X) (lecture X))
   ((lecture X) (%choice "lecture" ANYSYM X))
   ((image X) (%choice "image" ANYSYM X))
   ((image X) (photo X))
   ((image X) (meme X))
   ((meme X) (%choice "meme" ANYSYM X))
   ((image X) (drawing X))
   ((drawing X) (%choice "drawing" ANYSYM X))
   ((photo X) (%choice "photo" ANYSYM X))
   ((photo X) (selfie X))
   ((selfie X) (%choice "selfie" ANYSYM X))
   ((text X) (%choice "text" ANYSYM X))
   ((text X) (book X))
   ((book X) (%choice "book" ANYSYM X))
   ((text X) (pasta X))
   ((pasta X) (%choice "pasta" ANYSYM X)))

 (categorization->prolog
  (categorization-parse-tree
   "
		 audio video image text
		 audio : song recording
		 video : clip recording lecture
		 image : photo meme drawing
		 photo : selfie
		 text : book pasta
")))

(assert=
 '() (categorization->prolog
      (categorization-parse-tree "")))
