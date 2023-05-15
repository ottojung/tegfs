
Installation
============

To install TegFS, follow these steps:

1. Install the dependencies listed below.

2. Clone the TegFS repository:

   .. code-block:: bash

      $ git clone https://codeberg.org/otto/tegfs

3. Change into the ``tegfs`` directory:

   .. code-block:: bash

      $ cd tegfs

4. Run the installation script:

   .. code-block:: bash

      $ make install PREFIX="$HOME/.local"

Dependencies
------------

The following dependencies are required to install and use TegFS:

+-------------------+------------------------------------------------+
| Dependency        | Description                                    |
+===================+================================================+
| GNU guile         | version >1.8 for running the main              |
|                   | program  **(MUST HAVE)**                       |
+-------------------+------------------------------------------------+
| wget              | for saving stuff from the internet             |
+-------------------+------------------------------------------------+
| xclip             | for dumping clipboard content                  |
+-------------------+------------------------------------------------+
| file              | for determining file types                     |
+-------------------+------------------------------------------------+
| fzf               | for making choices during ``tegfs              |
|                   | save`` **(MUST HAVE)**                         |
+-------------------+------------------------------------------------+
| rsync, ssh        | for sending files to remote servers            |
|                   | if using ``tegfs save``                        |
|                   | with ``--remote``                              |
+-------------------+------------------------------------------------+
| swi-prolog        | for ``tegfs prolog`` command                   |
+-------------------+------------------------------------------------+
| ffmpeg            | for video previews production                  |
+-------------------+------------------------------------------------+
| imagemagick       | for image previews production                  |
+-------------------+------------------------------------------------+
| entr              | for checking if new previews need              |
|                   | to be made **(MUST HAVE)**                     |
+-------------------+------------------------------------------------+
| third-party file  | server, such as ``Nginx``, if using            |
| server            | ``--offload-filesharing`` option for           |
|                   | ``tegfs serve``                                |
+-------------------+------------------------------------------------+
| xdg-open          | if not using                                   |
|                   | ``--offload-filesharing``                      |
|                   | option for ``tegfs serve``                     |
+-------------------+------------------------------------------------+
| pup               | for generating weblink thumbnails              |
+-------------------+------------------------------------------------+

Note that the dependencies listed as **MUST HAVE** are required for basic TegFS functionality, while the others are only needed for certain features.

If you encounter any issues during the installation process, please consult the TegFS documentation or open an issue on the project's Codeberg repository.