Internal documentation
==

This is a collection of guides to help contributors to the ATD
project.

Whenever possible, we prefer to automate things over having people
read documents and follow rules.

Releasing `atd`
--

We use `topkg` to help with the release process. The release process
involves assigning a [version ID](https://semver.org/) to a git
commit, building a tar.gz archive, and publishing the opam packages
that use this archive.

1. Install the command-line interface with `opam install topkg-care`.
2. Run `topkg help release` then run the steps suggested there.
