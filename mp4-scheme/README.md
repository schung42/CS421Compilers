Using Stack
===========

You can use `stack` to run your submission, as well as run tests. It may ask you
to run `stack setup` first to setup a local version of GHC.

Running your code
-----------------

Type the command (in terminal):

```bash
% stack ghci mp4-scheme:exe:scheme
```

This will open GHCi with your solution (in `Main.hs`) loaded. You can then start
your `repl` with the call to:

```haskell
*Main> repl runtime
```

Testing your code
-----------------

Type the command:

```bash
% stack ghci mp4-scheme:test:test
```

This will open GHCi with your solution and the test-set loaded. You can run all
the tests by making the call:

```haskell
*Main> runTests allTests
```

If you want to modify which tests are run, look at the file `tests/Spec.hs`. You
will see a list `allTests :: [TestSuite]` in there. Create another constant
`myTests :: [TestSuite]` which contains just the tests you want to run. Then
re-run the `stack` command above, and at the GHCi prompt run:

```haskell
*Main> runTests myTests
```
