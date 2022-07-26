### cljf

cljf is a simple formatter for Clojure source code. It is similar to Joker's [format mode](https://github.com/candid82/joker#format-mode), but much smaller and faster as it only does one thing: formats code.

### Building

```
gcc main.c -o cljf -O3
```

or

```
./build.sh
```

Tested on macOS. Should work on Linux.

### Usage

```
Usage: cljf [<input file or directory>] [-o <output file>]
Examples:
cljf                    - read source code from stdin and write formatted code to stdout
cljf foo.clj            - format file foo.clj (override its content with formatted code)
cljf foo.clj -o bar.clj - read source code from file foo.clj and write formatted code to file bar.clj
cljf src                - format all Clojure files (files with extensions *.clj, *.cljs, *.cljc, *.joke) in src directory
```
