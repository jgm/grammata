# grammata

Well-typed system for generating documents in multiple formats.

To try it:

    stack install
    export GHC_PACKAGE_PATH=`stack path --snapshot-pkg-db`:
    grammata LaTeX test.gram
    grammata Html test.gram
    grammata PDF test.gram # requires xetex

Take a look at `test.gram`, the source.

Then look at `Grammata/Format/Html.hs` and
`Grammata/Format/LaTeX.hs`, and `Grammata/Format/PDF.hs`.

You can see how typed Haskell definitions map on to
commands in the tex-like syntax of `test.gram`.

So this is like TeX, but with all the power of a typed,
general-purpose programming language, and an easy way of
generalizing to multiple output formats.

To see how the types work, try changing `heading{2}` in
`test.gram` to `heading{22.3}`.  Or add
`\para{\emph{\para{a}}}`.

Note that, with the exception of things like `today`,
the definitions are mostly polymorphic; they
can be run in the IO monad, if you're trusting and need
your document generation to do IO, or in another monad (like
Maybe), if you want security guarantees.  Eventually I'll add a
`--safe` mode to `grammata`.

Fuller description and examples TBD.

