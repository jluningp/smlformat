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

SMLFormat has two modes: stdin and file. In stdin mode, SMLFormat reads SML code from StdIn and outputs formatted code to StdOut. In file mode, SMLFormat reads SML code from an input file and outputs formatted code to an output file.

StdIn mode:
```
$ echo "val {} = {}" | smlformat -i
val () = ()
```

File mode:
```
$ echo "val {} = {}" > input.sml
$ smlformat input.sml output.sml
$ cat output.sml
val () = ()
```
To format a file in place, make input and output the same file:
```
$ smlformat code.sml code.sml
```

If an input fails to parse, SMLFormat cannot format it and will return the original file contents.
```
$ echo "val x" | smlformat -i 
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
4. To enable formatting on save, add the following to your .vimrc:
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
