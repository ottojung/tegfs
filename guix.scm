
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
 (ice-9 popen)
 (ice-9 rdelim))

(define-public tegfs
  (let* ((source-dir (dirname (current-filename))))
    (package
      (name "tegfs")
      (version "0.1")
      (source (local-file source-dir #:recursive? #t #:select? (const #t)))
      (propagated-inputs (list git guile-3.0))
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
           (delete 'configure)))))))

tegfs

