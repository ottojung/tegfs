
(assert=
 '(((EUPHRATES-CFG-CLI-MAIN X) (EUPHRATES-CFG-CLI-MAIN/text X))
   ((EUPHRATES-CFG-CLI-MAIN X) (text X))
   ((EUPHRATES-CFG-CLI-MAIN X) (EUPHRATES-CFG-CLI-MAIN/image X))
   ((EUPHRATES-CFG-CLI-MAIN X) (image X))
   ((EUPHRATES-CFG-CLI-MAIN X) (EUPHRATES-CFG-CLI-MAIN/video X))
   ((EUPHRATES-CFG-CLI-MAIN X) (video X))
   ((EUPHRATES-CFG-CLI-MAIN X) (EUPHRATES-CFG-CLI-MAIN/audio X))
   ((EUPHRATES-CFG-CLI-MAIN X) (audio X)))
 (categorization->prolog
  (categorization-parse
   "audio video image text")))

(assert=
 '(((video X) (video/lecture X))
   ((video X) (lecture X))
   ((video X) (video/recording X))
   ((video X) (video/clip X))
   ((video X) (clip X))
   ((audio X) (audio/recording X))
   ((audio X) (audio/song X))
   ((audio X) (song X))
   ((EUPHRATES-CFG-CLI-MAIN X) (EUPHRATES-CFG-CLI-MAIN/video X))
   ((EUPHRATES-CFG-CLI-MAIN X) (video X))
   ((EUPHRATES-CFG-CLI-MAIN X) (EUPHRATES-CFG-CLI-MAIN/audio X))
   ((EUPHRATES-CFG-CLI-MAIN X) (audio X)))
 (categorization->prolog
  (categorization-parse
   "
		 audio video
		 audio : song recording
		 video : clip recording lecture
")))

(assert=
 '(((text X) (text/pasta X))
   ((text X) (pasta X))
   ((text X) (text/book X))
   ((text X) (book X))
   ((photo X) (photo/selfie X))
   ((photo X) (selfie X))
   ((image X) (image/drawing X))
   ((image X) (drawing X))
   ((image X) (image/meme X))
   ((image X) (meme X))
   ((image X) (image/photo X))
   ((image X) (photo X))
   ((video X) (video/lecture X))
   ((video X) (lecture X))
   ((video X) (video/recording X))
   ((video X) (video/clip X))
   ((video X) (clip X))
   ((audio X) (audio/recording X))
   ((audio X) (audio/song X))
   ((audio X) (song X))
   ((EUPHRATES-CFG-CLI-MAIN X) (EUPHRATES-CFG-CLI-MAIN/text X))
   ((EUPHRATES-CFG-CLI-MAIN X) (text X))
   ((EUPHRATES-CFG-CLI-MAIN X) (EUPHRATES-CFG-CLI-MAIN/image X))
   ((EUPHRATES-CFG-CLI-MAIN X) (image X))
   ((EUPHRATES-CFG-CLI-MAIN X) (EUPHRATES-CFG-CLI-MAIN/video X))
   ((EUPHRATES-CFG-CLI-MAIN X) (video X))
   ((EUPHRATES-CFG-CLI-MAIN X) (EUPHRATES-CFG-CLI-MAIN/audio X))
   ((EUPHRATES-CFG-CLI-MAIN X) (audio X)))
 (categorization->prolog
  (categorization-parse
   "
		 audio video image text
		 audio : song recording
		 video : clip recording lecture
		 image : photo meme drawing
		 photo : selfie
		 text : book pasta
")))
