###########################################################################
#
#   @autor Long Hoang <long@mindworker.de>
# 
#   @brief general useful aliases
#
###########################################################################

alias steveinstall='sudo apt-get install '

alias magic='emacs -nw ~/.bashrc'
alias emagic='emacs -nw ~/.emacs.d/init.el'
alias hostmagic='emacs -nw /etc/hosts'

# get rid of command not found
alias cd..='cd ..'
 
# a quick way to get out of current directory
alias ..='cd ..'
alias ...='cd ../../../'

# handy shortcuts
alias j='jobs -l'
alias em='emacs -nw'
alias sem='sudo emacs -nw'
alias chmox='chmod +x'
alias schmox='sudo chmod +x'

# this one saved by butt so many times
alias wget='wget -c' # resumes downloads by default
alias cp='cp -i'     # interactive cp by default (prevents unconcious overriding)

# ls shortcuts
alias lsvid='ls /dev/video*'

# utilities
alias sweep='find . -name "*~" | xargs rm -v; find . -name "*#" | xargs rm -v;'
alias sudosweep='find . -name "*~" | xargs sudo rm -v; find . -name "*#" | xargs sudo rm -v;'
alias dark='xset dpms force off' # black the screen

## trashput 
##   @note: need to install trashput in base folder
alias rmt='trash-put'
# alias rm='echo "rm is not save, please youse trash-put or rmt"'

function bck() { cp "$1" "$1.bck" ;}

function extract {
 if [ -z "$1" ]; then
    # display usage if no parameters given
    echo "Usage: extract <path/file_name>.<zip|rar|bz2|gz|tar|tbz2|tgz|Z|7z|xz|ex|tar.bz2|tar.gz|tar.xz>"
 else
    if [ -f $1 ] ; then
	# NAME=${1%.*}
	# echo $NAME
        # mkdir $NAME && cd $NAME
        case $1 in
          *.tar.bz2)   tar xvjf ./$1    ;;
          *.tar.gz)    tar xvzf ./$1    ;;
          *.tar.xz)    tar xvJf ./$1    ;;
          *.lzma)      unlzma ./$1      ;;
          *.bz2)       bunzip2 ./$1     ;;
          *.rar)       unrar x -ad ./$1 ;;
          *.gz)        gunzip ./$1      ;;
          *.tar)       tar xvf ./$1     ;;
          *.tbz2)      tar xvjf ./$1    ;;
          *.tgz)       tar xvzf ./$1    ;;
          *.zip)       unzip ./$1       ;;
          *.Z)         uncompress ./$1  ;;
          *.7z)        7z x ./$1        ;;
          *.xz)        unxz ./$1        ;;
          *.exe)       cabextract ./$1  ;;
          *)           echo "extract: '$1' - unknown archive method" ;;
        esac
    else
        echo "$1 - file does not exist"
    fi
fi
}

function histo() {
    history | awk '{CMD[$2]++;count++;}END { for (a in CMD)print CMD[a] " " CMD[a]/count*100 "% " a;}' | grep -v "./" | column -c3 -s " " -t | sort -nr | nl |  head -n15
}

function batmon() {
    while true; do
	acpitool -B | grep "Present rate";
	sleep 1;
    done
}
