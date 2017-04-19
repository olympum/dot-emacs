# dot-emacs

My Emacs 24 configuration files with support for:

* Javascript
* Scala
* Clojure
* Markdown
* Golang

## Go (Golang)

I am using `godef` to jump to definition, `gocode` for completition
and `goflymake` for on-the-fly syntax check (`gofmt` will only check
static syntax errors). To install these packages, ensure your `GOPATH`
and `GOROOT` environment variables are correctly setup. This is what I
have in my `~/.profile` file:

    # for terminal apps
    export GOPATH=$HOME/Projects/golang
    export PATH=$PATH:$GOPATH/bin
    export GOROOT=`go env GOROOT`
    export PATH=$PATH:$GOROOT/bin

    # for non-terminal apps
    launchctl setenv GOPATH $GOPATH
    launchctl setenv GOROOT $GOROOT
    launchctl setenv PATH $PATH

To install the packages:

    go get -u github.com/dougm/goflymake
    go get -u github.com/rogpeppe/godef
    go get -u github.com/nsf/gocode

I have setup `$HOME/Projects/golang` to be my home for Go projects, so
in the emacs `init.el` file you'll find:

    (setenv "GOPATH" (concat (getenv "HOME") "/Projects/golang"))
    (setenv "PATH" (concat (getenv "PATH") ":" (concat (getenv "HOME") "/Projects/golang/bin")))
    (setq exec-path (append exec-path (list (expand-file-name (concat (getenv "HOME") "/Projects/golang/bin")))))

