
Installation
============

To install TegFS, follow these steps:

1. Install the dependencies listed below.

2. Clone the TegFS repository:

.. code-block:: bash

   git clone https://codeberg.org/otto/tegfs

3. Change into the TegFS directory:

.. code-block:: bash

   cd tegfs

4. Run the installation script:

.. code-block:: bash

   make install PREFIX="$HOME/.local"

Dependencies
------------

The following dependencies are required to install and use TegFS:

+-------------------+------------------------------------------------+
| Dependency        | Description                                    |
+===================+================================================+
| GNU guile         | version >=3.0.8 for running the main           |
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
| third-party web   | for serving files in the web UI, unless        |
| server            | the ``fileserver`` config option is            |
| (ex. ``Nginx``)   | set to "xdg-open://"                           |
+-------------------+------------------------------------------------+
| xdg-open          | for serving files in the web UI when           |
|                   | the ``fileserver`` config option is            |
|                   | set to "xdg-open://"                           |
+-------------------+------------------------------------------------+
| pup               | for generating thumbnails from URLs            |
+-------------------+------------------------------------------------+

.. note::

   Dependencies listed as **MUST HAVE** are required for basic TegFS functionality, while the others are only needed for certain features.

If you encounter any issues during the installation process, please consult the TegFS documentation or open an issue on the project's Codeberg repository.

Docker
------

TegFS can also be run inside a Docker container. To get started, follow these steps:

1. Install Docker on your machine by following the official instructions for your operating system.
2. Clone the TegFS repository:

.. code-block:: bash

   git clone https://codeberg.org/otto/tegfs

3. Navigate to the TegFS directory:

.. code-block:: bash

   cd tegfs

4. Build the Docker image:

.. code-block:: bash

   export DOCKER_BUILDKIT=1
   docker build -f scripts/Dockerfile -t tegfs .

5. Run the container:

.. code-block:: bash

   docker run --rm -p 33470:80 --name tegfs tegfs

6. Open ``http://localhost:33470`` in your web browser to access TegFS.


.. important::

    The password for TegFS in a docker container is randomly generated.
    Look for ``--password`` string in the docker logs to get it.


Alternatively, you can use the provided Makefile to run the Docker container:

1. Navigate to the TegFS directory:

.. code-block:: bash

   cd tegfs

2. Use the Makefile:

.. code-block:: bash

   make rundocker

3. Open ``http://localhost:33470`` in your web browser to access TegFS.

.. note::

    Docker container is configured to use port ``33470`` by default. If you need to use a different port, you can specify it when running the container, e.g. ``docker run -p 8080:80 tegfs``.

To access TegFS command line interface, run

.. code-block:: bash

   docker exec -it tegfs bash -l

Then you will have access to the running TegFS instance which you can test by executing the following command:

.. code-block:: bash

   tegfs config get port

Guix
----

If you're using the Guix package manager, there is a TegFS package definition available in the ``scripts/guix.scm`` file included in the source code. You can install TegFS by running the following commands:

.. code-block:: bash

   git clone https://codeberg.org/otto/tegfs
   cd tegfs
   guix package -f scripts/guix.scm

Alternatively it can be loaded in a temporary environment:

.. code-block:: bash

   guix environment --ad-hoc --load=scripts/guix.scm
