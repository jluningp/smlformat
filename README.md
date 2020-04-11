# SMLFormat

SMLFormat is an autoformatter for the Standard ML programing language. It indents, keeps lines under 80 characters, and applies a set of heuristics to prettify code.

The source code of SMLFormat is formatted with SMLFormat.


## Installation

1. Install the [SML/NJ compiler](https://www.smlnj.org).
2. Clone this git repo: `git clone https://github.com/jluningp/smlformat.git SMLFORMAT_DIR`.
3. Open `SMLFORMAT_DIR/smlformat` and modify the variable `BASE_DIR` to be your smlformat directory.
3. Follow the instructions below for editor installation.

## Usage

SMLFormat has two modes: stdin and file. In stdin mode, SMLFormat reads SML code from StdIn and outputs formatted code to StdOut. In file mode, SMLFormat reads SML code from an input file and outputs formatted code to an output file.

StdIn mode:
```
$ echo "val {} = {}" | SMLFORMAT_DIR/smlformat -i
val () = ()
```

File mode:
```
$ echo "val {} = {}" > input.sml
$ SMLFORMAT_DIR/smlformat input.sml output.sml
$ cat output.sml
val () = ()
```
To format a file in place, make input and output the same file:
```
$ SMLFORMAT_DIR/smlformat code.sml code.sml
```

If an input fails to parse, SMLFormat cannot format it and will return the original file contents.
```
$ echo "val x" | SMLFORMAT_DIR/smlformat -i 
val x
```


You can also manually run SMLFormat in the SML/NJ REPL by running
```
sml -m SMLFORMAT_DIR/sources.cm
- SmlFormat.format "input.sml";
val () = ()
-
```

## Editors
This is all very rough right now. I'm working on improving it, setting up format-on-save, etc.
### Emacs
#### Setup
1. Copy the code in `editors/emacs` into your `.emacs` (sorry).
2. Change the smlformat path on the first line (`(defcustom smlformat-command "/home/...`) to be your SMLFormat path.
3. Reopen emacs

#### Usage
1. Save the file you're working on
2. `M-x smlformat`
3. If your file doesn't parse when you run SMLFormat, it will not be formatted.

### Vim
#### Setup
1. Install the [Neoformat](https://github.com/sbdchd/neoformat) vim plugin
2. Copy the code in `editors/vim` into Neoformat's formatters directory as `sml.vim`:
```
cp SMLFORMAT_DIR/formatters/vim ~/.vim/plugged/neoformat/autoload/neoformat/formatters/sml.vim
```
3. In sml.vim, replace `exe : '/home/...` with the path to your SMLFormat command.

#### Usage
1. `: Neoformat`
2. If your file doesn't parse when you run SMLFormat, it will not be formatted.

## Known Issues
1. Types have too many parentheses around them
2. Functor signatures get deleted
3. Indentation with constructors + newlines is weird (maybe an extra indent?)
