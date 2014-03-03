# Path to your oh-my-zsh configuration.
ZSH=$HOME/.oh-my-zsh

# Set name of the theme to load.
# Look in ~/.oh-my-zsh/themes/
# Optionally, if you set this to "random", it'll load a random theme each
# time that oh-my-zsh is loaded.
ZSH_THEME="robbyrussell"
#ZSH_THEME="sunrise"

# Set to this to use case-sensitive completion
# CASE_SENSITIVE="true"

# Comment this out to disable weekly auto-update checks
# DISABLE_AUTO_UPDATE="true"

# Uncomment following line if you want to disable colors in ls
# DISABLE_LS_COLORS="true"

# Uncomment following line if you want to disable autosetting terminal title.
# DISABLE_AUTO_TITLE="true"

# Uncomment following line if you want red dots to be displayed while waiting for completion
# COMPLETION_WAITING_DOTS="true"

# Which plugins would you like to load? (plugins can be found in ~/.oh-my-zsh/plugins/*)
# Example format: plugins=(rails git textmate ruby lighthouse)
plugins=(compleat git zsh-syntax-highlighting tmux)

source $ZSH/oh-my-zsh.sh

# Customize to your needs...
export PATH=$PATH:/usr/local/sbin:/usr/local/bin:/usr/sbin:/usr/bin:/sbin:/bin:/usr/games

export HISTSIZE=5000

#utf for gnoem-terminal
export LANG=en_US.UTF-8

# location-labs

export EDITOR=vi
export PAGER=less
export PRINTER=hp_LaserJet_2420

export CATALINA_HOME=/opt/wm/apache-tomcat-5.5.28/
export JDK_HOME=/usr/lib/jvm/java-6-sun
export JAVA_HOME=/usr/lib/jvm/java-6-sun

alias ant='$ANT_HOME/bin/ant'
export ANT_OPTS=-XX:MaxPermSize=512m

function wm-project() {
    local name=$1

    local script=/usr/local/code/build_ctrl/build_ctrl/bin/wm-project.py

    if [ -n "$name" ]; then
        if [ -e /usr/local/code/${name}/${name}_project/project.xml ]; then
            spath=/usr/local/code/${name}/${name}_project/project.xml
        else
            spath=/usr/local/code/${name}/project.xml
        fi
        eval `$script -r choosy -c /usr/local/code -s -p ubuntu-10.04-64 -t 'export ${var}=${path}' $spath`
    fi
}

#USER SUBMITTED
alias ack=ack-grep $1

function ackv (){
    vim $(ack-grep -l -a -i $1)
}

alias meldhead='svn diff -r HEAD --diff-cmd meld'
alias meldrev='svn diff -r $1 --diff-cmd meld'

function mysvnfiles (){
    svn log -v | awk '/^r[0-9]+ / {user=$3} {if (user=="eric") {print $2}}' | sort | uniq
}

#local code
alias localcode='cd /usr/local/code'

#ignore . files/folders for complete
set match-hidden-files off

#sparkle status
alias sparklestatus='elinks http://tinderbox.engr.wavemarket.com/sparkle/status'

#Allow java AWT to display with xmonad(otherwise blank)
export _JAVA_AWT_WM_NONREPARENTING=1

#runs make, searches for source files, gets just the paths to the files and 
#appends space to end of line for error format for vim, and sorts opens in vim as error file
alias vimerrorfiles='vim -c "$(make release | egrep "/usr/local/code.*:[0-9]+:" | tr ":" " " | awk "{print \":e\", \$2\"|\"\":\"\$3}" | uniq | sort | paste -sd "|")"'

#same as above except lists files, and changes filename:34: -> filename 34
alias errorfiles='make release | egrep "/usr/local/code.*:[0-9]+:" | awk "{print \$2 \"  \"}" | sed -r "s/\:([0-9]+)\:/ \+\1/" | uniq | sort'

#add scala to path
#export PATH=$PATH:/opt/scala-2.9.1/bin
export PATH=$PATH:/opt/scala-2.10.0/bin
export PYTHONPATH=$PYTHONPATH:/usr/lib/python2.6/site-packages
export PYTHONPATH=$PYTHONPATH:/usr/local/code/sparkle/notifier

#alias question txt
alias questions='vim ~/questions.txt'

#android home
export ANDROID_SDK_HOME=/opt/adt-bundle-linux/sdk
export ANDROID_HOME=/opt/adt-bundle-linux/sdk
export PATH=$PATH:$ANDROID_SDK_HOME/platform-tools:$ANDROID_SDK_HOME/tools

#add gradle support
export GRADLE_HOME=/opt/gradle1.7
export GRADLE_COMMON=/usr/local/code/build_tools/gradle_common
export GRADLE_USER_HOME=/usr/local/code/.gradle
export PATH=$PATH:$GRADLE_HOME/bin

#add scala home for scala console in gradle files
#export SCALA_HOME=/opt/scala-2.9.1
export SCALA_HOME=/opt/scala-2.10.0/

#add groovy
export PATH=$PATH:/opt/groovy/bin

PATH=$PATH:$HOME/.rvm/bin # Add RVM to PATH for scripting

#PYTHON/PIP SETUP
if [ `id -u` != '0' ]; then

   # Always use pip/distribute
   export VIRTUALENV_USE_DISTRIBUTE=1

   # All virtualenvs will be stored here
   export WORKON_HOME=/usr/local/home/.virtualenvs

   source /usr/local/bin/virtualenvwrapper.sh
   export PIP_VIRTUALENV_BASE=$WORKON_HOME
   export PIP_RESPECT_VIRTUALENV=true

fi

#don't correct
unsetopt correct_all
unsetopt correct

# Less needs to pass through escaped color characters.
export LESS=-R

#export VAGRANT_HOME=/usr/local/code/vagrant

# Allows pressing enter to select a menu and execute.
bindkey -M menuselect '^M' .accept-line
zmodload zsh/complist

# Allow ctrl-q and s to pass through to vim
# http://stackoverflow.com/questions/7883803/why-doesnt-map-c-q-q-cr-work-in-vim
# http://unix.stackexchange.com/questions/12107/how-to-unfreeze-after-accidentally-pressing-ctrl-s-in-a-terminal
stty -ixon

# Add cabal haskell install location.
PATH=$PATH:$HOME/.cabal/bin 

# nvm path
[ -s $HOME/.nvm/nvm.sh ] && . $HOME/.nvm/nvm.sh # This loads NVM

# tmux color fix
alias tmux='TERM=xterm-256color tmux'
alias sparkle_scala='scala -J-Xmx2g -cp $(~davec/bin/topath /opt/wm/apache-tomcat-5.5.28/webapps/sparkle/WEB-INF/lib/*)'

# alias xdg-open to open file with default app.
alias ']'='xdg-open'
