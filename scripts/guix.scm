
(use-modules
 (guix packages)
 (guix build utils)
 (guix build-system gnu)
 (guix gexp)
 (guix profiles)
 (guix git-download)
 ((guix licenses) #:prefix license:)
 (gnu packages version-control)
 (gnu packages guile)
 (gnu packages wget)
 ((gnu packages xdisorg) #:select (xclip))
 (gnu packages file)
 ((gnu packages terminals) #:select (fzf))
 (gnu packages rsync)
 ((gnu packages prolog) #:select (swi-prolog))
 ((gnu packages video) #:select (ffmpeg))
 (gnu packages imagemagick)
 (gnu packages entr)
 ((gnu packages web) #:select (pup))
 ((gnu packages photo) #:select (perl-image-exiftool))
 ((gnu packages base) #:select (tar))
 (ice-9 popen)
 (ice-9 rdelim))

(define-public tegfs
  (let* ((source-dir (dirname (dirname (current-filename)))))
    (package
      (name "tegfs")
      (version "0.1")
      (source (local-file source-dir #:recursive? #t #:select? (const #t)))
      (propagated-inputs
       (list git guile-3.0 wget
             xclip file fzf rsync
             swi-prolog ffmpeg
             imagemagick entr pup tar))
      (license license:agpl3+)
      (home-page "https://git.vau.place/tegfs.git")
      (synopsis "TegFS virtual file system")
      (description "TegFS is a virtual tag-based file system with higher order tags. It is used for file categorization, as an alternative for classical directory-based categorization.")
      (build-system gnu-build-system)
      (arguments
       `(#:tests? #f
         #:make-flags
         (list (string-append "PREFIX=" %output))
         #:parallel-build? #f
         #:phases
         (modify-phases
             %standard-phases
           (delete 'check)
           (delete 'build)
           (delete 'configure)))))))

tegfs

