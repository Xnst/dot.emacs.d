# my own .emacs.d

clone into ~/ and rename to .emacs.d/

copy dot.pylintrc into ~/.pylintrc

pylintrc is needed to elliminate some disturbing warnings

usually I do write python to do some quick
calculations and use a more direct format than PEP8 assumes.

To switch to -nw mode when loggin in with ssh add the following to the  .bashrc

if [ -n "$SSH_CLIENT" ] || [ -n "$SSH_TTY" ]; then
    alias emacs='emacs -nw -q -l ~/.emacs.d/initNW.el'
fi 
