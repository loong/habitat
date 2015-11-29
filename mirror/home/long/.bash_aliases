###########################################################################
#
#   @autor Long Hoang <long@mindworker.de>
# 
#   @brief general useful aliases
#
###########################################################################

# Workspaces

alias stevep='cd ~/Workspaces/go/src/github.com/mindworker/p2pfx && export PORT=8080 && export DATABASE_URL=postgres://nmptwgjmlcgxkb:flkBig3SUZvyPG1WNKsmixl64m@ec2-54-197-224-173.compute-1.amazonaws.com:5432/ded392jntj3rjj && export BASE_URL=http://localhost:8080/'

alias mush='~/habitat/scripts/mush.sh'
alias mushd='cd ~/Workspaces/go/src/github.com/mindworker/go-plainexchange'
alias curlcmd='stevep && emacs curl_cmd.txt &'

alias stevepp='cd ~/Workspaces/go/src/github.com/mindworker/p2pfx-private && export PORT=8080 && export DATABASE_URL=postgres://mtaiptbbiahgvn:ce45scYwflMx6ppiQeN918CKAN@ec2-54-217-202-109.eu-west-1.compute.amazonaws.com:5432/d5s6k2i0r9ptbb && export BASE_URL=http://localhost:8080/'

alias stevepm='cd ~/Workspaces/go/src/github.com/mindworker/p2pfx-msgs && export PORT=8081 && export DATABASE_URL=postgres://oftottqrvmxulh:y5XR5WUfVAU7Jr-ugtEtRlzRMd@ec2-23-23-81-221.compute-1.amazonaws.com:5432/dv6592468nt7 && export BASE_URL=http://localhost:8080/'

alias stevepu='cd ~/Workspaces/p2pfx/www'

alias stevea='cd ~/Workspaces/avr/atm32m1'

alias steveat='cd ~/Workspaces/go/src/github.com/mindworker/arduino-tutorial'

alias steveq='cd ~/Workspaces/comp3111h'
alias qcover='chromium-browser coverage/Chrome\ 45.0.2454\ \(Linux\ 0.0.0\)/index.html'
alias karmas='karma start karma.conf.js'

alias steveb='cd ~/Workspaces/comp3111h/questions-backend'
alias stever='cd ~/Workspaces/react/testProj'

alias steves='cd ~/Workspaces/hackSH/fimatech/node'
###

alias steveinstall='sudo apt-get install '

alias magic='emacs -nw ~/.bashrc'
alias amagic='emacs -nw ~/.bash_aliases'
alias tmagic='emacs -nw ~/.tmux.conf'
alias emagic='emacs -nw ~/.emacs.d/init.el'
alias gmagic='emacs -nw ~/.emacs.d/plugins/yasnippet/snippets/go-mode'
alias hostmagic='emacs -nw /etc/hosts'

# get rid of command not found
alias cd..='cd ..'
 
# a quick way to get out of current directory
alias ..='cd ..'
alias ...='cd ../../../'

alias nauti='nautilus .'

# handy shortcuts
alias j='jobs -l'
alias em='emacs -nw'
alias sem='sudo emacs -nw'
alias chmox='chmod +x'
alias schmox='sudo chmod +x'

# this one saved by butt so many times
alias wget='wget -c' # resumes downloads by default
alias cp='cp -i'     # interactive cp by default (prevents unconcious overriding)
alias df='df -h'

# ls shortcuts
alias lsvid='ls /dev/video*'
alias lsttu='ls /dev/ttyUSB*'

# git shortcuts
alias gs='git status'
alias gc='git commit -m'
alias gp='git push' 
alias g.='git add .'

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

# mobile + webdev
function curlj() {
    curl "$@" | python -m json.tool
}

alias emu='emulator -avd Nexus_5_API_Lollipop_x86_64'