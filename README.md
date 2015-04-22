# tmpl [![Build Status](https://travis-ci.org/michelk/tmpl.svg?branch=master)](https://travis-ci.org/michelk/tmpl) [![Hackage version](https://img.shields.io/hackage/v/tmpl.svg?style=flat)](https://hackage.haskell.org/package/tmpl)
Simple command-line tool for string-template-replacement, based on Johan
Tibbell's [template](https://github.com/tibbe/template)-library.

The program could be run in three ways:

```bash
cat tt.tmpl
# The quick $color fox jumps over the $adj dog.
```
```bash
cat dd.conf
# color = brown
# adj = lazy
```

1. `tmpl color=brown:adj=lazy < tt.tmpl`
2. `tmpl tt.tmpl < dd.conf`
3. `tmpl tt.tmpl dd.conf`
