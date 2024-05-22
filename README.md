# Ro-Shhhambo: The Private RPS Tournament

Welcome to Ro-Shhhambo, an innovative rock-paper-scissors (RPS) tournament that combines the power of Spritely's HOOT, Endo, and Lurk to create a unique, privacy-focused gaming experience.

## Overview

In Ro-Shhhambo, players compete by writing deterministic RPS strategies in a custom subset of LISP that is compatible with both the Spritely and Lurk programming environments. The game starts with each player submitting a "bias" first move, and then subsequent moves are determined by observing the full game history.

Spritely's HOOT (Higher-Order Object Oriented Templates) is used for providing the game's user interface, while Endo is used for establishing game sessions with other players and exchanging proofs. Lurk's zero-knowledge proof capabilities allow players to prove their moves are valid without revealing their strategy source code, ensuring a private and fair tournament.

## Features

- Custom LISP subset for writing RPS strategies
- Deterministic gameplay based on initial "bias" move and game history
- Spritely's HOOT for the game's user interface
- Endo for establishing game sessions and exchanging proofs
- Lurk zero-knowledge proofs for validating moves without revealing strategies
- Potential for ongoing "king of the hill" style tournaments with entry fees and prize pools

## Getting Started

To get started with Ro-Shhhambo, you'll need to have the following installed:

- [Spritely's HOOT](https://spritely.institute/news/make-a-game-with-hoot-for-the-lisp-game-jam.html)
- [Endo](https://github.com/endojs/endo)
- [Lurk zero-knowledge proof system](https://blog.lurk-lang.org/posts/prog-intro/) (See Functional Commitments)

Clone the repository and follow the installation instructions in the [INSTALL.md](./INSTALL.md) file.

## Contributing

We welcome contributions from the community! If you'd like to contribute to Ro-Shhhambo, please read our [CONTRIBUTING.md](./CONTRIBUTING.md) file for guidelines on how to submit pull requests and report issues.

## License

Ro-Shhhambo is released under the [MIT License](./LICENSE).

## Contact

If you have any questions or suggestions, feel free to open an issue or reach out to the project maintainers at [email address or social media handles].

Let's revolutionize the world of RPS tournaments together with Ro-Shhhambo!

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
