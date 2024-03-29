
Query
===========

The following table shows the special tags that are auto-generated by TegFS:

+---------------+-----------------------------------------+--------+
| Tag           | Description                             | Arity  |
+===============+=========================================+========+
| ``%any``      | true for all arguments                  | 1      |
+---------------+-----------------------------------------+--------+
| ``%diff``     | true if two objects are different       | 2      |
+---------------+-----------------------------------------+--------+
| ``%remote``   | true if target is a web link            | 1      |
+---------------+-----------------------------------------+--------+
| ``%local``    | true if target is not a web link        | 1      |
+---------------+-----------------------------------------+--------+
| ``%notarget`` | true if the object has no target        | 1      |
+---------------+-----------------------------------------+--------+
| ``%unsorted`` | true if the object has no other         | 1      |
|               | user-defined tags                       |        |
+---------------+-----------------------------------------+--------+
| ``%upload``   | true if the object was uploaded through | 1      |
|               | the web interface                       |        |
+---------------+-----------------------------------------+--------+

For example, doing ``tegfs query %any`` will return every object there is in the database.
