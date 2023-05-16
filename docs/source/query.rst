
Query
===========

Along with usual tags,
 query provides special tags that are auto generated.
The tags are:

- `%any` - true for all arguments (arity = 1)
- `%diff` - true if two objects are different (arity = 2)
- `%remote` - true if target is a web link (arity = 1)
- `%local` - true if target is not a web link (arity = 1)
- `%notarget` - true if the object has no target (arity = 1)
- `%unsorted` - true if the object has no other user-defined tags (arity = 1)
- `%upload` - true if the object was uploaded through the web interface (arity = 1)


So, for example, doing ``tegfs query %any`` will return
 every object there is in the database.
