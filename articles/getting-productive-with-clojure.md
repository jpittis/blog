# Getting Productive With Clojure

**Dec 30, 2018**

I recently wrote my first real program in Clojure! ([A simple HTTP based write
though key value cache built on top of
Zookeeper.](https://github.com/jpittis/zookeyper))

What do I mean by a real program? In the past I've read books and written small toy
scripts but I've never tackled problems similar to ones I might be solving at work.

The goal of this post is to share some things that got me from an empty directory to
useful program in the hope that others might try the same. This isn't best practice or
expert advice, but it worked for me!

## Install Clojure

Clojure has a [Getting Started](https://clojure.org/guides/getting_started) guide for
installation.

## Use Leiningen

Leiningen is a program that takes care of creating and automating Clojure projects. There
are other ways to get started with Clojure, but this worked really well for me! I
installed Leiningen using my package manager but there are also [installation
instructions](https://leiningen.org/#install) on the project's website.

I wanted to write an executable so `lein new app <project-name>` got me started. I
proceeded to delete all the files that didn't seem relevant to me. (This will likely make
some people cringe!)

```bash
$ lein new app my-project
$ cd my-project
$ rm CHANGELOG.md
$ rm LICENSE
$ rm -rf doc/
$ rm -rf resources/
$ rm README.md

```

I also went into the `project.clj` file and deleted the description, url and license
lines.

```
  :description "FIXME: write description"
  :url "http://example.com/FIXME"
  :license {:name "EPL-2.0 OR GPL-2.0-or-later WITH Classpath-exception-2.0"
            :url "https://www.eclipse.org/legal/epl-2.0/"}
```

Now we have a really simple directory structure that's easy to undertand and
expain!

```bash
$ tree
.
├── project.clj
├── src
│   └── my-project
│       └── core.clj
└── test
    └── my-project
        └── core_test.clj
```

`core.clj` contains your executable's main function. `core_test.clj` contains
your tests. You can run them respectively with `lein run` and `lein test`.

```bash
$ lein run
Hello, World!
$ lein test

lein test my-project.core-test

lein test :only my-project.core-test/a-test

FAIL in (a-test) (core_test.clj:7)
FIXME, I fail.
expected: (= 0 1)
  actual: (not (= 0 1))

Ran 1 tests containing 1 assertions.
1 failures, 0 errors.
Tests failed.
```

## Better test output

I found that the default test output to be hard to follow for complex failures.

I created the following file in `~/.lein/profiles.clj`.

```clojure
{:user {:dependencies [[pjstadig/humane-test-output "0.8.3"]]
        :injections [(require 'pjstadig.humane-test-output)
                     (pjstadig.humane-test-output/activate!)]}}
```

Now the test output includes a diff:

```
$ lein test

lein test my-project.core-test

lein test :only my-project.core-test/a-test

FAIL in (a-test) (core_test.clj:7)
FIXME, I fail.
expected: 0
  actual: 1
    diff: - 0
          + 1

Ran 1 tests containing 1 assertions.
1 failures, 0 errors.
Tests failed.
```

## Running a single test

Kinda verbose but whatever.

```bash
$ lein test :only my-project.core-test/a-test
```

## No you don't need to learn Emacs or setup a fancy REPL

There's lots of talk in the Clojure and Lisp community of fancy REPLs, using Emacs and
SLIME. ([I've used SLIME before and it's very
cool!](/posts/macro-expansion-with-slime.html))

You don't need it to get started!

My development flow looks something like this:

- Open Clojure files in neovim with syntax highlighting and paren matching.
  (Most editors should have these by default.)
- Have a terminal open where I run `lein test` and `lein run` when needed.
- Sometimes run `lein repl` to experiment interactively.

I want to explore more fancy REPL stuff in the future, but this got me
productive!

## Multiple files

I wanted to factor my code into multiple files and was having trouble figuring
out how to create multiple namespaces and all this fancy stuff. Here's
something super simple to get started!

- Create a file beside `core.clj` for example `app.clj`.
- Add a namespace definition to the top of `app.clj` just like in `core.clj`.

```clojure
(ns my-project.core)
```

- Load `app.clj` from `core.clj`.

```clojure
(load "app")
```

Multiple namespaces are probably the way to go for larger projects, but this
worked great for me!

## Using Libaries

Let's add the Ring HTTP library as an example.

Start by adding the dependency to your `project.clj` file.

```
  :dependencies [[org.clojure/clojure "1.10.0"]
                 [ring/ring-core "1.7.1"]]
```

Then download the dependency. (Running `lein test` or `lein run` should do the
the same thing.)

```
$ lein deps
```

We can then require the functions we need in our file's namespace definition.

```clojure
(ns my-project.core
  (:require [ring.middleware.json :as ring :refer [wrap-json-body]]))
```

You can can use the function with `ring/wrap-json-body`.

## Useful Libraries

Here are some useful libraries. I've used them all in [this
project](https://github.com/jpittis/zookeyper) so that may be a good place to
see how they're used.

### HTTP

- `ring/ring-core` for HTTP handlers.
- `ring/ring-json` for JSON HTTP handlers.
- `ring/ring-mock` to test HTTP handlers.
- `ring/ring-jetty-adapter` for a ring compatible HTTP server.
- `compojure` for HTTP routes.

### Command line parsing

- `org.clojure/tools.cli` for general purpose option parsing.

### JSON

- `cheshire` for JSON parsing.

### Zookeeper

Likely not relevant but I used `zookeeper-clj` as a Zookeeper client.

## Useful Features, Macros and Functions

- `->` and `->>` to avoid nested function calls.
- Atoms for synchronization between threads.
- `try`, `catch` and `finally` for exception handling and resource cleanup.

## Documentation

Use `(doc <function-name>)` from the REPL for quick documentation lookup.

## Experience

A few things I don't like:

- The error messages are often much longer than they ought to be!
- I'm less a fan of the JVM because of memory usage, startup time and because I'm so used
  to accessing unix APIs.

Things that were great:

- I've written a bit of Lisp before so parentheses don't scare me. My previous functional
  programming experience made me productive from the start.
- All libraries use the same base immutable datastructures which makes data consistent and
  easy to manipulate.
- Tooling is pretty solid. (I've seen better but I've also seen a lot worse!)

I'm generally pleased with the experience!
