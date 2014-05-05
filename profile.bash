export PS1="\[\033]0;\u@\h:\w\007\]\u@\h:\W\\$ "

alias edit="emacsclient -t"
# not doing nedit, use ec instead
# ln -s /Applications/Emacs.app/Contents/MacOS/bin/emacsclient /usr/local/bin
#alias nedit="emacsclient -nc"
export ALTERNATE_EDITOR= EDITOR=emacsclient VISUAL=emacsclient
