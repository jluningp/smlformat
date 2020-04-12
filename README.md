# SMLFormat

SMLFormat is an autoformatter for the Standard ML programing language. It indents, keeps lines under 80 characters, and applies a set of heuristics to prettify code.

The source code of SMLFormat is formatted with SMLFormat.


## Installation

1. Install the [SML/NJ compiler](https://www.smlnj.org).
2. Clone this git repo: `git clone https://github.com/jluningp/smlformat.git SMLFORMAT_DIR`.
3. Add SMLFormat to your path (replacing SMLFORMAT_DIR with your SMLFormat directory): 
```
$ echo "PATH=\"\$PATH:SMLFORMAT_DIR\"; export PATH" >> ~/.bashrc
$ source ~/.bashrc
```
4. Follow the instructions below for editor installation.

## Usage

SMLFormat can read from stdin and write to stdout, or it can take files as arguments. 

```
Usage: smlformat [-i inputfile] [-o outputfile]
  -i infile    read code from file instead of stdin
  -o outfile   write formatted code to file instead of stdout
  -h           print this help message
```

### Examples

Read from file:
```
$ echo "fun foo x =     10" > test.sml
$ smlformat -i test.sml 
fun foo x = 10
```

Write to file:
```
$ echo "fun foo x =    10" | smlformat -o test_output.sml
$ cat test_output.sml
fun foo x = 10
```

Read and write files:
```
$ echo "fun foo x =     10" > test.sml
$ smlformat -i test.sml -o test_output.sml
$ cat test_output.sml
fun foo x = 10
```

Read from stdin and write to stdout:
```
$ echo "fun foo x =    10" | smlformat
fun foo x = 10
```

Format a file "in-place":
```
$ echo "fun foo x =     10" > test.sml
$ smlformat -i test.sml -o test.sml
$ cat test.sml 
fun foo x = 10
```

If an input fails to parse, SMLFormat cannot format it and will return the original input.
```
$ echo "val x" | smlformat 
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
### Emacs
#### Setup
1. If you want to format on save, uncomment the bottom line of `editors/emacs/smlformat.el`.  
3. Add the following line to your `.emacs`, with `SMLFORMAT_DIR` replaced by your SMLFormat directory:
```
(load "SMLFORMAT_DIR/editors/emacs/smlformat.el")
```
4. Reopen emacs

#### Usage
1. Save the file you're working on
2. `M-x smlformat`
3. If your file doesn't parse when you run SMLFormat, it will not be formatted.

### Vim
#### Setup
1. Install the [Neoformat](https://github.com/sbdchd/neoformat) vim plugin
2. Copy the files in `editors/vim` into Neoformat's formatters directory<sup>1</sup>.
```
cp editors/vim/* ~/.vim/plugged/neoformat/autoload/neoformat/formatters/
```
3. To enable formatting on save, add the following to your .vimrc:
```
autocmd BufWritePre *.sml,*.sig Neoformat
```
<sup>1</sup> Why is there `sml.vim` and `lprolog.vim`? Vim thinks `.sig` files have filetype `lprolog`

#### Usage
1. `: Neoformat`
2. If your file doesn't parse when you run SMLFormat, it will not be formatted.

## Known Issues
1. Types have too many parentheses around them
2. Functor signatures get deleted
3. Indentation with constructors + newlines is weird (maybe an extra indent?)
