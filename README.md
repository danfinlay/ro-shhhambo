# Guile Hoot Game Jam Template

This repository is the quickest way to get started building games in
Scheme that run in web browsers with Guile Hoot!

It has everything you need:

* A simple Breakout clone to use as a starting point.

* HTML and JavaScript boilerplate for running the game in a web page.

* DOM bindings for events, images, audio, and more.

* HTML5 canvas bindings for rendering.

* Some simple but useful game math modules.

* A `Makefile` for compiling the game to WebAssembly, running a
  development web server, and generating zip bundles for publishing to
  itch.io.

* A Guix `manifest.scm` file for creating a development environment
  with `guix shell`.

## Tutorial

The fastest way to get everything you need is to use [GNU
Guix](https://guix.gnu.org), a wonderful package manager written in
Scheme.

Once you have Guix, the development environment with all necessary
dependencies can be created:

```
guix shell
```

To build the game, run:

```
make
```

To launch a development web server, run:

```
make serve
```

To check if the program works, visit https://localhost:8088 in your
web browser.  We recommend using Mozilla Firefox or Google Chrome.
Hoot is not supported on Safari at this time.

When it's time to publish the game to itch.io, run:

```
make bundle
```

Upload the resulting zip file to your itch.io game page and share your
game with others!  Have fun!

## Getting help

If you have questions or need some help, visit the [Spritely
Institute's forum](https://community.spritely.institute/) or connect
to the `#spritely` channel on the Libera.Chat IRC network.
