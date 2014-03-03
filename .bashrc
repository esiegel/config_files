#Useful Aliases
alias ls="ls -G"
alias hg="history | grep $1"
alias psg="ps aux | grep $1"

#Work Aliases
alias codeos='pushd /Users/eric/Documents/workspace/os/src/nachos; vim /Users/eric/Documents/workspace/os/src/nachos'
alias codeclone="pushd /Users/eric/Documents/workspace/os_git/src/nachos; vim /Users/eric/Documents/workspace/os_git/src/nachos"

alias sharethis="python -m SimpleHTTPServer 8080"

#google api
googedit(){
    google docs edit --title $1 --editor vim 
}
alias gfind="find . | xargs grep $1"

export EDITOR="/usr/bin/vim -f -O"

#bash prompt
export PS1="\w: "

#araxis diffing
export PATH=$PATH:/usr/bin/araxis

#bash completion
if [ -f `brew --prefix`/etc/bash_completion ]; then
    . `brew --prefix`/etc/bash_completion
fi
