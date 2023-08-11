
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

	"))

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





(let ()
  (define-values (tree rules)
    (categorization-parse
     "
	audio video image text

	----------------------

	rule song video => clip

	"))

  (assert= tree '((EUPHRATES-CFG-CLI-MAIN audio video image text)))
  (assert= rules '(((clip This) (song This) (video This)))))






(let ()
  (define-values (tree rules)
    (categorization-parse
     "
	audio video image text

	----------------------

	rule song video => clip
	rule music => song

	"))

  (assert= tree '((EUPHRATES-CFG-CLI-MAIN audio video image text)))
  (assert= rules '(((clip This) (song This) (video This))
                   ((song This) (music This)))))





(let ()
  (define-values (tree rules)
    (categorization-parse
     "
	audio video image text

	----------------------

	rule song video => clip
	rule song video => clip

	"))

  (assert= tree '((EUPHRATES-CFG-CLI-MAIN audio video image text)))
  (assert= rules '(((clip This) (song This) (video This))
                   ((clip This) (song This) (video This)))))






(let ()
  (define-values (tree rules)
    (categorization-parse
     "
	audio video image text

	----------------------

	rule song video => clip

	rule music => song

	"))

  (assert= tree '((EUPHRATES-CFG-CLI-MAIN audio video image text)))
  (assert= rules '(((clip This) (song This) (video This))
                   ((song This) (music This)))))





(let ()
  (define-values (tree rules)
    (categorization-parse
     "
	audio video image text

	----------------------

	rule song video => clip

	rule music => song		

	"))

  (assert= tree '((EUPHRATES-CFG-CLI-MAIN audio video image text)))
  (assert= rules '(((clip This) (song This) (video This))
                   ((song This) (music This)))))






(let ()
  (define-values (tree rules)
    (categorization-parse
     "
	audio video image text

	----------------------

	rule song video => clip
	rule music => song
	rule movie => video

	"))

  (assert= tree '((EUPHRATES-CFG-CLI-MAIN audio video image text)))
  (assert= rules '(((clip This) (song This) (video This))
                   ((song This) (music This))
                   ((video This) (movie This)))))






(let ()
  (define-values (tree rules)
    (categorization-parse
     "
	audio video image text

	----------------------

	rule song=Y video=Y => clip=Y

	"))

  (assert= tree '((EUPHRATES-CFG-CLI-MAIN audio video image text)))
  (assert= rules '(((clip Y) (song Y) (video Y)))))








(let ()
  (define-values (tree rules)
    (categorization-parse
     "
	audio video image text

	----------------------

	rule song video => clip movie

	"))

  (assert= tree '((EUPHRATES-CFG-CLI-MAIN audio video image text)))
  (assert= rules '(((clip This) (song This) (video This))
                   ((movie This) (song This) (video This)))))







(let ()
  (define-values (tree rules)
    (categorization-parse
     "
	audio video image text

	----------------------

	rule song video => clip movie
	rule music => song

	"))

  (assert= tree '((EUPHRATES-CFG-CLI-MAIN audio video image text)))
  (assert= rules '(((clip This) (song This) (video This))
                   ((movie This) (song This) (video This))
                   ((song This) (music This)))))





(let ()
  (define-values (tree rules)
    (categorization-parse
     "
	audio video image text

	----------------------

	rule song=X video=Y => clip=Z movie=M

	"))

  (assert= tree '((EUPHRATES-CFG-CLI-MAIN audio video image text)))
  (assert= rules '(((clip Z) (song X) (video Y))
                   ((movie M) (song X) (video Y)))))





(let ()
  (define-values (tree rules)
    (categorization-parse
     "
	audio video image text

	----------------------

	rule song=X video=Y => clip=Z movie=M
	rule music => song

	"))

  (assert= tree '((EUPHRATES-CFG-CLI-MAIN audio video image text)))
  (assert= rules '(((clip Z) (song X) (video Y))
                   ((movie M) (song X) (video Y))
                   ((song This) (music This)))))





(let ()
  (define-values (tree rules)
    (categorization-parse
     "
	audio video image text

	----------------------

	rule song=Y video=Y,M => clip=Y,Y,Z,W

	"))

  (assert= tree '((EUPHRATES-CFG-CLI-MAIN audio video image text)))
  (assert= rules '(((clip Y Y Z W) (song Y) (video Y M)))))





(let ()
  (define-values (tree rules)
    (categorization-parse
     "
	audio video image text

	----------------------

	"))

  (assert= tree '((EUPHRATES-CFG-CLI-MAIN audio video image text)))
  (assert= rules '()))

