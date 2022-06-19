
# What is this?

TegFS is a virtual tag-based file system with higher order tags.

It is a perfect tool for categorizing files.
And is a replacement for classical directory-based categorization.

This program also provides a web-based file-browser
 with advanced sharing capabilities.

# Dependencies

- Main program
  - `GNU guile` version >1.8 for running the main program
  - `curl` for saving stuff from the internet
  - `xclip` for dumping clipboard content
  - `xdg-mime` for determining file types
  - `fzf` for making choices during `tegfs save`
  - `rsync` for sending files to remote servers if using `tegfs save` with `--remote`
  - `prolog` for query capabilities (TODO: REMOVE THIS DEPENDENCY)
- Web server
  - `ffmpeg` for video previews production
  - `imagemagick` for image previews production
  - `entr` for checking if new previews need to be made
  - third-party file server, such as `Nginx`

# TODO

- [ ] remove prolog dependency
- [ ] web: make /upload tagging interactive
- [ ] web: make buttons for logging-out
- [ ] web: make buttons for sharing
- [ ] web: more granular users permissions
- [ ] document on how to run this thing
