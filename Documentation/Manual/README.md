# Building the Manual

## Install `pandoc` and `pandoc-crossref`
It is required to install the latest version of `pandoc` and
`pandoc-crossref` to avoid any problems. Fortunately both programs are
available as statically-linked binaries. You have to download apropriate
package from the `releases` page, uncompress it and put these binaries
in `$PATH`. 
1) Get `pandoc` from https://github.com/jgm/pandoc/releases .
2) Get `pandoc-crossref` from https://github.com/lierdakil/pandoc-crossref/releases .
3) Uncompress it to get the binaries. then on Linux you can install them by copying
them into somewhere in `$PATH` such as `/usr/local/bin/`.
4) Test it by running `pandoc --version`.

You can now build the `html` and `epub` version of the manual by running 
`make html` and `make epub` respectively.

## Install `texlive` and `texlive-fonts-extra`
You need to install `texlive` and `texlive-fonts-extra` package to build
the pdf version. Similarly to build `info` version, you need to install `texinfo`
package.
 - For pdf run `make pdf`.
 - For info run `make info`.

