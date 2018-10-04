% Nix Pills
% Rich Wallace
% Oct 3, 2018

# Intro

https://nixos.org/nixos/nix-pills/index.html

# Covers

- why?

- installation

- language

- derivations

- garbage collection

- design patterns

- nixpkgs

- nix store

- stdenv

# Pill 1 - Why?

- purely functional => immutable

- `/nix/store` contains derivations, `/nix/store/hash-name`

```
$ which bash
/nix/store/czx8vkrb9jdgjyz8qfksh10vrnqa723l-bash-4.4-p23/bin/bash
$ ldd $(which bash)
...
```

- paths are hardcoded => upgrading libs means rebuilding packages that use it

# Pill 2 - Installation

```
$ curl https://nixos.org/nix/install | sh
```
- creates `/nix/store`

- creates nix sqlite db, `/nix/var/nix/db`

# Pill 2 - User profile

- creates a user profile, `~/.nix-profile`

- profiles compose installed packages

```
$ ls -l ~/.nix-profile/
```

- `~/.nix-profile` itself is a symbolic link to `/nix/var/nix/profiles/default`

```
$ ls -l ~/.nix-profile
```

- follow the links, eventually get to `/nix/store/{hash}-user-environment`

# Pill 2 - nixpkgs

- nix expressions are used to describe derivations/packages

- nixpkgs contains nix expressions for loads of things

```
$ ls -l ~/.nix-defexpr/channels
```

- follow links, eventually get to `/nix/store/{hash}-nixpkgs-{version}`

# Pill 2 - Environment changes

- adds `~/.nix-profile/etc/profile.d/nix.sh` to `~/.profile`

- add `~/.nix-profile/bin` to `PATH`

- sets `~/.nix-defexpr/channels/nixpkgs` in the `NIX_PATH`

# Pill 3 - Nix environment

- Set it up

```
$ source ~/.nix-profile/etc/profile.d/nix.sh
```

- Install something

```
$ nix-env -i nix-prefetch-github
```

- created a new generation

```
$ nix-env --list-generations
```

- list installed derivations

```
$ nix-env -q
```

# Pill 3 - revisiting our profile

```
$ ls -l ~/.nix-profile/

$ ls -l ~/.nix-profile/bin/
```

- when a derivation is installed, links to each file in the bin/ are linked to

- rollback with `nix-env --rollback`

- go to specific generation with `nix-env -G 3`

# Pill 3 - the Nix store

- query runtime dependencies

```
$ nix-store -q --references $(which nix-prefetch-github)
```

- reverse dependnecies

```
$ nix-store -q --referrers $(which nix-prefetch-github)
```

# Pill 3 - Closures

- all recursive dependencies needed to use derivation

```
$ nix-store -qR $(which nix-prefetch-github)
```

- tree view

```
nix-store -q --tree $(which nix-prefetch-github)
```

- see what's installed in our profile

```
nix-store -q --tree ~/.nix-profile
```

# Pill 3 - Channels

- channel are where we get packages from

```
$ nix-channel --list

$ nix-channel --update
```

- unpacked and linked to `~/.nix-defexpr/channels`

# Pill 4 - Language basics

- purely functional

    - no statements

    - immutable

- non-strict, i.e. lazy

- integer, floating point, string, path, boolean and null simple types

- basic arithmetic operations: +, -, and *

    - where's division?

```
nix-repl> 2 / 3
```

- other operators are ||, && and ! for booleans, and relational operators such as !=, ==, <, >, <=, >=

- you cannot mix strings and integers, you must first do the conversion

# Pill 4 - Language basics cont.

- identifiers can contain `-`, i.e `a-b` is not the same as `a - b`

- strings defined using `"` or `''`

```
nix-repl> "foo"
nix-repl> ''foo''
```

- interpolation is done with `${}`

```
nix-repl> foo = "str"
nix-repl> "${foo}"
nix-repl> "$(1 + 2}"
```

- escape in double quoted string with `\\`

```
nix-repl> "test \" test"
nix-repl> "\${foo}"
```

- escape `''` in double single quoted string with `''`

```
nix-repl> ''test ''${foo} test''
```

# Pill 4 - Language basics cont.

- lists are homogenous

```
nix-repl> [ 1 "str" (2 + 3) ]
```

- attribute sets are key/value pairs, keys are strings

```
nix-repl> s = { foo = "bar"; a-b = "baz"; "123" = "num"; }
nix-repl> s.a-b
nix-repl> s."123"
```

- can't refer to other attributes unless you make it recursive

```
nix-repl> { a = 3; b = a+4; }
nix-repl> rec { a = 3; b = a+4; }
```

# Pill 4 - Language basics cont.

- if is an expression, not a statement

```
nix-repl> if 3 > 4 then "yes" else "no"
```

- let expressions define local values inside expressions

```
nix-repl> let a = 5; in a
nix-repl> let a = 3; b = 4; in a + b
```

- can be nested

```
nix-repl> let a = 3; in let b = 4; in a + b
```

- allows shadowing, BEWARE!

```
nix-repl> let a = 3; in let a = 8; in a
```

# Pill 4 - Language basics cont.

- with expressions are sort of like imports, but they are still expressions

```
nix-repl>  longName = { a = 3; b = 4; }
nix-repl> longName.a + longName.b
nix-repl> with longName; a + b
```

- does not shadow

```
nix-repl> let a = 10; in with longName; a + b
nix-repl> let a = 10; in with longName; longName.a + b
```

- lazy means evaluated only when needed

```
nix-repl> let a = builtins.div 4 0; b = 6; in b
```

# Pill 5 - Functions

- anonymous lambdas with a single argument

```
nix-repl> x: x*2
```

- applying a value

```
nix-repl> double = x: x*2
nix-repl> double 3
```

- "multiple" arguments are nested lambdas

```
nix-repl> mul = a: b: a * b
nix-repl> mul
nix-repl> mul 2
nix-repl> mul 2 3
nix-repl> mul (2+3) (3+4)
```

# Pill 5 - Functions cont.

- argument sets can be pattern matched

```
nix-repl> mul = s: s.a*s.b
nix-repl> mul { a = 3; b = 4; }
nix-repl> mul = { a, b }: a*b
nix-repl> mul { a = 3; b = 4; }
```

- default values

```
nix-repl> mul = { a, b ? 2 }: a*b
nix-repl> mul { a = 3; }
```

- variadic

```
nix-repl> mul = { a, b, ... }: a*b
nix-repl> mul { a = 3; b = 4; c = 2; }
```

- @-pattern

```
nix-repl> mul = s@{ a, b, ... }: a*b*s.c
nix-repl> mul { a = 3; b = 4; c = 2; }
```

# Pill 5 - Imports

- put nix expressions in files
```
$ cat a.nix
$ cat b.nix
$ cat mul.nix
```

- compose them with `import`

```
nix-repl> a = import ./a.nix
nix-repl> b = import ./b.nix
nix-repl> mul = import ./mul.nix
nix-repl> mul a b
nix-repl> (import ./mul.nix) (import ./a.nix) (import ./b.nix)
```

# Pill 5 - Imports

- more complex example using attribute sets

```
$ cat test.nix
```

```
nix-repl> test = import ./test.nix
nix-repl> import ./test.nix { a = 5; trueMsg = "ok"; }
```

# Pill 6 - First derivation

- `derivation` function requires at least 3 arguments: name, system, builder

- what's our system?

```
nix-repl> builtins.currentSystem
```

- bogus derivation

```
nix-repl> d = derivation {
  name = "myname";
  builder = "mybuilder";
  system = "mysystem";
}
nix-repl> d
«derivation /nix/store/z3hhlxbckx4g3n9sw91nnvlkjvyw754p-myname.drv»
```

# Pill 6 - First derivation cont.

- `.drv` file is to a nix derivation expression as a `.o` file is to a `.c` file

```
$ cat /nix/store/z3hhlxbckx4g3n9sw91nnvlkjvyw754p-myname.drv
$ nix show-derivation /nix/store/z3hhlxbckx4g3n9sw91nnvlkjvyw754p-myname.drv
```

- derivation contains: output paths, input derivations, system and builder, environment variables passed to buildersystem and builder
- out path doesn't yet exist, it's where things will be built

- to build

```
nix-repl> :b d
```

- `:b` is special to the repl, can also be done with `nix-store -r /nix/store/z3hhlxbckx4g3n9sw91nnvlkjvyw754p-myname.drv` which tries to"realize" the derivation

- examine derivation

```
nix-repl> builtins.attrNames d
nix-repl> d.drvAttrs
nix-repl> (d == d.out)
nix-repl> d.drvPath
```

# Pill 6 - First derivation cont.

- nix provides an automatic conversion for derivations

```
nix-repl> d.outPath
nix-repl> builtins.toString d
```

- makes it easy to construct paths to dependencies

```
nix-repl> :l <nixpkgs>
nix-repl> coreutils
nix-repl> builtins.toString coreutils
nix-repl> "${coreutils}"
nix-repl> "${coreutils}/bin/true"
```

# Pill 6 - First derivation cont.

- add a builder to our derivation

```
nix-repl> :l <nixpkgs>
nix-repl> d = derivation {
  name = "myname";
  builder = "${coreutils}/bin/true";
  system = builtins.currentSystem;
}
```

- building with `:b d` fails because we don't create the path in the nix store

- examine derivation with `nix show-derivation` to show dependency on coreutils derivation

- instantiation/evaluation is to realizing/building as compiling is to linking

# Pill 7 - A working derivation

- we need to create a builder to put something at the output path

```
$ cat builder.sh
```

- can create a file or a directory, doesn't matter

- we don't put `/usr/bin/bash` or `/usr/bin/env bash`, would lead to an impure environment

- `derivation` function supports passing arguments to the builder

```
nix-repl> :l <nixpkgs>
nix-repl> "${bash}"
nix-repl> d = derivation {
  name = "foo";
  builder = "${bash}/bin/bash";
  args = [ ./builder.sh ];
  system = builtins.currentSystem;
}
nix-repl> :b d
```

- note we used `./builder.sh`, which is a path

# Pill 7 - A working derivation cont.

- look at produced file and env variables

- `HOME` is not your home directory, and `/homeless-shelter` doesn't exist at all

- `PATH` is the same

- `NIX_BUILD_CORES` and `NIX_STORE` are nix configuration options

- `PWD` and `TMP` clearly show that nix created a temporary build directory

- `builder`, `name`, `out`, and `system` are variables set due to the .drv file's contents

# Pill 7 - A working derivation cont.

- look at the derivation file again

```
$ nix show-derivation /nix/store/{hash}-foo.drv
```

- new `args` attribute, which points to `builder.sh` copied to `/nix/store`

# Pill 7 - A working derivation cont.

- derivation for a simple C program

```
$ cat simple.c
$ cat simple_builder.sh
```

```
nix-repl> :l <nixpkgs>
nix-repl> simple = derivation {
  name = "simple";
  builder = "${bash}/bin/bash";
  args = [ ./simple_builder.sh ];
  gcc = gcc;
  coreutils = coreutils;
  src = ./simple.c;
  system = builtins.currentSystem;
}
nix-repl> :b simple
```

```
$ nix show-derivation /nix/store/{hash}-simple.drv
```

# Pill 7 - A working derivation cont.

- done with the repl for now

```
$ cat simple.nix
```

- build it

```
$ nix-build simple.nix
```

- same as using `nix-instantiate simple.nix` then `nix-store -r`

- creates a `result` link in cwd to built nix store path

- note: value returned by `(import <nixpkgs> {})` is a set of derivations, using `with` we bring them all into scope

- `inherit x y` keyword is short hand for `x = x; y = y;`

# Pill 8 - Generic builders

- common linux packages use `tar -xzf $src; ./configure --prefix=$out; make; make install`, we can write a script that does that and a wrapper function around `derivation`

```
$ cat gbuilder.sh
$ cat autotools.nix
```

- nix converts the list of `baseInputs` and `buildInputs` to strings

```
nix-repl> builtins.toString 123
nix-repl> builtins.toString [ 123 456 ]
```

- we combine the default attribute set and the provided attribute set using the `//` operator

```
nix-repl> { a = "b"; } // { c = "d"; }
nix-repl> { a = "b"; } // { a = "c"; }
```

- can build autotools packages easily

```
$ cat hello.nix
```

# Pill 9 - Runtime dependencies

- look at build time dependencies

```
$ nix-store -q --references $(nix-instantiate hello.nix)
```

- those are all the things our derivation uses for building hello

- look at runtime dependencies

```
$ nix-store -q --references $(nix-store -r $(nix-instantiate hello.nix))
```

- it's smaller than the build time deps, but we didn't specify anything

- nix uses some magic to determine runtime deps, but why the dependency on gcc?

- some debugging information and some other nix specific ld rpath stuff you mostly don't need to worry about

- can be fixed using `strip` and `patchelf`

# Pill 10 - Developing with nix-shell

- purpose: to allow us to hack on derivations in the necessary environment

```
$ nix-shell hello.nix
[nix-shell]$ make
bash: make: command not found
[nix-shell]$ echo $baseInputs
/nix/store/jff4a6zqi0yrladx3kwy4v6844s3swpc-gnutar-1.27.1 [...]
```

- we have the environment variables that we set in the derivation, like `$baseInputs`, `$buildInputs`, `$src` and so on.

- we can run the builder

```
[nix-shell]$ source builder.sh
```

- nix-shell drops us in a shell with the same (or almost) environment used to run the builder

# Pill 11 - Garbage collector

- nix stores GC roots in `/nix/var/nix/gcroots`

- Nix first moves dead store paths to /nix/store/trash which is an atomic operation. Afterwards, the trash is emptied.

```
$ nix-collect-garbage
```

# Pill 11 - Garbage collector cont.

- install a package to play with

```
$ nix-env -iA nixpkgs.bsdgames
$ readlink -f `which fortune`
$ nix-store -q --roots `which fortune`
$ nix-env --list-generations
```

- now remove it, but it will still be in the nix store after garbage collecting

```
$ nix-env -e bsd-games
$ nix-collect-garbage
$ ls /nix/store/*bsd-games-2.17
```

- old generation is still in the nix store because it's a GC root

# Pill 11 - Garbage collector cont.

- all profiles and their generations are GC roots

```
$ rm /nix/var/nix/profiles/default-$GENERATION-link
$ nix-env --list-generations
$ nix-collect-garbage
$ ls /nix/store/*bsd-games-2.17
```

- `/nix/var/nix/gcroots/profiles` is a symlink to `/nix/var/nix/profiles`

# Pill 11 - Garbage collector cont.

- indirect roots

```
$ ls -l /nix/var/nix/gcroots/auto/
```

- links to `result` links from `nix-build`

- when we remove the `result` link, it dangles and `nix-garbage-collect` can clean up after us

# Pill 11 - Garbage collector cont.

- clean up everything!

```
$ nix-channel --update
$ nix-env -u --always
$ rm /nix/var/nix/gcroots/auto/*
$ nix-collect-garbage -d
```

- be careful! old generations are removed, you can't roll back


