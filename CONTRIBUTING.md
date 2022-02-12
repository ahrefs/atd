ATD Contribution Guidelines
==

This is a collection of guides to help contributors to the ATD
project.

Whenever possible, we prefer to automate things over having people
read documents and follow rules.

Releasing `atd`
--

We use `topkg` to help with the release process. The release process
involves assigning a [version ID](https://semver.org/), tagging a git
commit with this version ID, building an archive,
and publishing the opam packages that use this archive.

1. Install the command-line interface with `opam install topkg-care`.
2. Update `CHANGES.md`.
3. Tag the commit with `git tag <VERSION>`.
4. Create an archive with `topkg distrib`.
5. Upload the archive to Github with `topkg publish distrib`.
6. Run `topkg opam pkg` and grab the `url` file `_build/atd.<VERSION>/url`.
7. Update the opam packages in
   [opam-repository](https://github.com/ocaml/opam-repository).

Details and troubleshooting notes:
* (4) the archive is created in `_build`. The version string in
  the file `atdgen-runtime/src/version.ml` of the archive
  should no longer be the `%%VERSION%%` placeholder.
* (5) if two-factor authentication is enabled for your account,
  you may get a 401 Unauthorized error.
  You may follow the [instructions on Github and create an access
  token](https://help.github.com/articles/creating-a-personal-access-token-for-the-command-line/)
  with the scope `public_repo`. Then arrange to put your access token
  in the `TOPKG_GITHUB_AUTH` environment variable when running
  `topkg publish distrib`. If everything works, there will be a new
  release in the "releases" tab/section on Github.
* (7) at this time, it doesn't seem that topkg understands that we
  have four opam packages and not just one, so we don't run
  `topkg opam submit`. Instead, we fork
  [`opam-repository`](https://github.com/ocaml/opam-repository) and
  copy `packages/atd/atd.<PREVIOUS>` to `packages/atd/atd.<NEW>`. Then
  we edit the dependencies in the `opam` file as needed and we replace
  the `url` file with the one generated at step (6). Repeat for
  `atdgen-runtime`, `atdgen`, and `atdj`. Then make a pull request
  to have your fork/branch merged into the original
  `opam-repository`.

Contributing to a specific subproject
--

Each subproject has its own README:

* [atdgen](atdgen): targets OCaml, Bucklescript
* [atdj](atdj): targets Java
* [atdpy](atdpy): targets Python
* [atds](atds): targets Scala

Updating the documentation
--

### Documentation setup

The user documentation is published at https://atd.readthedocs.io/.
It's automatically published from the main branch of the GitHub repo
from the files found in `/doc`.

The [format of the documentation is Restructured
Text](https://thomas-cokelaer.info/tutorials/sphinx/rest_syntax.html#restructured-text-rest-and-sphinx-cheatsheet)
because it's more expressive that Markdown and it's not that hard to
pick up.

You can either edit the `.rst` files and hope that everything will
turn out fine or you can preview it by running Sphinx locally. The
latter is recommended for large edits. Try this:

Install sphinx and the read-the-docs theme:
```
pip install sphinx sphinx_rtd_theme
```

Compile the documentation and run a local
[HTTP server on port 8888](http://0.0.0.0:8888):
```
make livedoc
```

### Writing good documentation

Don't assume that our existing documentation is already good (!)

Good documentation separates concerns. This not only makes it easy
to read but also easier to write.
Daniele Procida has a wonderful presentation about the four kinds of
documentation and why they're best kept separate:

* [website](https://documentation.divio.com/)
* [30-min presentation](https://www.youtube.com/watch?v=t4vKPhjcMZg)

![the four kinds of documentation](https://documentation.divio.com/_images/overview.png)
