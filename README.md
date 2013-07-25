Simple command-line tool for string-template-replacement, based on Johan
Tibbel's [template](https://github.com/tibbe/template)-library.

    cat dd.tmpl
    The quick $color fox jumps over the $adj dog.

    tmpl color=brown:adj=lazy < dd.tmpl
    The quick brown fox jumps over the lazy dog.
