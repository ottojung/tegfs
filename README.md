
# What is this?

TegFS is a virtual tag-based file system with higher order tags.

It is used for file categorization,
 as an alternative for classical directory-based categorization.

This program also provides a web-based file-browser
 with advanced sharing capabilities.

# Dependencies

- Main program
  - `GNU guile` version >1.8 for running the main program
  - `wget` for saving stuff from the internet
  - `xclip` for dumping clipboard content
  - `file` for determining file types
  - `fzf` for making choices during `tegfs save`
  - `rsync` for sending files to remote servers if using `tegfs save` with `--remote`
  - `prolog` for query capabilities
- Web server
  - `ffmpeg` for video previews production
  - `imagemagick` for image previews production
  - `entr` for checking if new previews need to be made
  - third-party file server, such as `Nginx`
  - [pup](https://github.com/ericchiang/pup) for generating weblink thumbnails

# Note on query

Along with usual tags,
 query provides special tags that are auto generated.
The tags are:

- `%any` - true for all arguments (arity = 1)
- `%diff` - true if two objects are different (arity = 2)
- `%remote` - true if target is a web link (arity = 1)
- `%local` - true if target is not a web link (arity = 1)
- `%notarget` - true if the object has no target (arity = 1)
- `%unsorted` - true if the object has no other user-defined tags (arity = 1)

So, for example, doing `tegfs query %any` will return
 every object there is in the database.

# TODO

- [ ] remove `prolog` dependency
- [ ] remove `pup` dependency and use guile's sxml instead
- [*] web: make /upload tagging interactive
- [*] web: make buttons for logging-out
- [ ] web: make buttons for sharing
- [ ] web: more granular users permissions
- [ ] document on how to run this thing
- [ ] web: implement pagination
- [*] web: implement directory sharing
- [*] web: escape all links
- [ ] finish renaming
- [ ] initialize files mimetypes on `add-entry`
  - [ ] add directory preview svg
  - [ ] change default file preview unknown
- [*] implement /home
- [*] implement /logout
- [*] implement /tags
- [ ] implement /note
- [ ] fix web::not-found
