Simple command-line tool for string-template-replacement, based on Johan
Tibbel's [template](https://github.com/tibbe/template)-library.

The program could be run in three ways:

    cat tt.tmpl
    The quick $color fox jumps over the $adj dog.

    cat dd.conf
    color = brown
    adj = lazy

1. `tmpl color=brown:adj=lazy < tt.tmpl`
2. `tmpl tt.tmpl < dd.conf`
3. `tmpl tt.tmpl dd.conf`
