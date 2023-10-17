======================================
How to change a JSON interface safely?
======================================

Adding a field to a record or object is often necessary. When is it
safe to do so without breaking compatibility with existing
implementations?

It depends on whether older implementations will consume newer JSON data or
vice-versa, whether newer implementations will consumer older JSON
data. In the former case, we worry about forward compatibility. In the
latter, we worry about backward compatibility. This can get
complicated when some types are requests made by a client while other
types are server responses. On top of that, you'll find that the logic
for product types (records/objects) is the inverse of sum types
(e.g. enums). For example, a server upgrade resulting in a response
object with a new field will not break older clients. However, a
server response that contains an enum cannot add a new case without
breaking older clients. Conversely, a client upgrade that reads a
response from an older server can support more enum cases than the
server will emit, while this client can't require a new field to be
added to the older server's response. If your head isn't spinning yet,
this is great for you. For the rest of us, there is ``atddiff``.

``atddiff`` is a command provided by the ``atd`` Opam package. It normally
comes pre-installed with ``atdcat``. If you don't have it, you can
install it with

.. code-block:: bash

  opam update
  opam install atd

Like the Unix utility ``diff``, ``atddiff`` takes two versions of the same
file, compares them, and reports differences. Atddiff knows about the
ATD language, so it will report only meaningful differences and
explain how they matter. Let's take the example of adding a record
field. The old version of our file is ``example_old.atd``:

.. code-block:: ocaml

  type response = {
    payload: string;
  }

The newer version of this interface adds a new ``id`` field which is
required. The new file is ``example_new.atd``:

.. code-block:: ocaml

  type response = {
    id: string;
    payload: string;
  }

Atddiff shows the differences between these two files as follows:

.. code-block::

  $ atddiff example_old.atd example_new.atd
  Backward incompatibility:
  File "example_new.atd", line 2, characters 2-12
  Required field 'id' is new.
  The following types are affected:
    response

``atddiff --help`` explains the difference between a backward
incompatibility and forward incompatibility. In this case, it means
it's fine to upgrade the server before the clients but a client cannot
be upgraded before the server. A solution is to be careful about
what to upgrade first. Another solution might be to make the new field
optional using e.g.

.. code-block:: ocaml

  type response = {
    ?id: string option;
    payload: string;
  }

Various types of changes in definitions will be reported, including when a
field becomes optional, when an enum case is added, or when certain elements
get renamed.

In practice, we usually don't have two versions of the same source
file checked out as separate files but they exist in ``git`` or some
other tracking system. With git, comparing two versions of the same
file can be done with the following invocation that replaces
``git diff example.atd``:

.. code-block::

  $ git difftool -x atddiff example.atd

It supports the same options as ``git diff`` allowing you to select the
two revisions to compare.
