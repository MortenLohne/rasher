# Overview

Rasher is a simple UCI chess engine written in Rust. It is *not* a standalone chess program, and needs a UCI-compatible chess GUI to be used comfortably, like Scid vs PC, Fritz, Tarrasch, Chessbase, or others.

I created this as a pet project in 2015 to learn Rust and experiment with computer chess. The engine plays at roughly 1200 ELO strength, slightly stronger than a beginner human player, but any serious player can easily defeat it.

Any feedback is welcome.

# Build

Note: Pre-compiled Windows binaries are available for download in the [release section](https://github.com/MortenLohne/rasher/releases) on Github. They should be reasonably up to date.

Building the project from source requires the Rust compiler and Cargo (Rust's package manager) installed, both included in the [Rust downloads.](https://www.rust-lang.org/en-US/downloads.html)

To build and run:
```
cargo build --release
cargo run --release
```

The engine uses the [UCI interface](https://en.wikipedia.org/wiki/Universal_Chess_Interface), and should be run from a GUI, but it can also be (inconveniently) run in a terminal. To evaluate the starting position, run the program and enter:

```
uci
position startpos
go infinite
```

To evaluate the position after 1.e4 e5:
```
uci
position startpos moves e2e4 e7e5
go infinite
```

# Tests

Use `cargo test` to run tests, `cargo test --release` to run without debugging checks, and `cargo test --release -- --ignored` to run the most time consuming perf tests.