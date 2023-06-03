###########################################################################
#
#   @autor Long Hoang
# 
###########################################################################

###########################################################################
# Habitat
###

# Source Prezto.
if [[ -s "${ZDOTDIR:-$HOME}/.zprezto/init.zsh" ]]; then
  source "${ZDOTDIR:-$HOME}/.zprezto/init.zsh"
fi

# Habitat bare repo commands
alias h='git --git-dir=$HOME/.habitat/ --work-tree=$HOME'
alias hs='git --git-dir=$HOME/.habitat/ --work-tree=$HOME status'
alias ha='git --git-dir=$HOME/.habitat/ --work-tree=$HOME add'
alias hc='git --git-dir=$HOME/.habitat/ --work-tree=$HOME commit -m'

###########################################################################
# Frequently used config files
###

alias magic='emacs -nw ~/.zshrc'
alias imagic='emacs -nw ~/.config/i3/config'
alias smagic='emacs -nw ~/.i3status.conf'
alias tmagic='emacs -nw ~/.tmux.conf'
alias emagic='emacs -nw ~/.emacs.d/init.el'
alias hmagic='emacs -nw /etc/hosts'

###########################################################################
# Very handy tweaks
###

alias cd..='cd ..'
alias ..='cd ..'
alias ...='cd ../../../'

alias em='emacs -nw'
alias sem='sudo emacs -nw'
alias v='vim -O'
alias chmox='chmod +x'
alias schmox='sudo chmod +x'
alias df='df -h'

alias cat='bat'
alias ls='exa'
alias ll='exa -l'
alias f='fzf --preview "bat --color \"always\" {}"'

# this one saved by butt so many times
alias wget='wget -c' # resumes downloads by default
alias cp='cp -i'     # interactive cp by default (prevents unconcious overriding)

# git shortcuts
alias gs='git status'
alias gc='git commit -m'
alias gp='git push'
alias gpu='git push -u origin'
alias gpom='git push origin master'
alias gco='git checkout'
alias gb='git branch'
alias g.='git add .'
alias gd='git diff'

alias gstore='git config credential.helper store'
alias gmod="git ls-files --modified | xargs git add"
alias gfix='git commit --fixup HEAD && git rebase -i --autosquash --autostash'
alias gwip='git commit -m WIP'
alias glint='git commit -m "Fix linting errors"'

function pr() {
    id=$1
    if [ -z $id ]; then
	echo "Retrieve pull request"
	echo "Usage: pr [issue number]"
	return 1
    fi
    git fetch origin pull/${id}/head:pr_${id}
    git checkout pr_${id}
}

# check if git name and email is set, if not set it to default
if [[ ! $(git config --global user.email) ]]; then
    echo "Git name and email conf not set, set them automatically"
    git config --global user.email "1732217+loong@users.noreply.github.com"
    git config --global user.name "Long Hoang"
    git config --global core.editor "emacs -nw"
    git config --global init.defaultBranch main
fi

# docker shortcuts
alias dp='docker ps'
alias dpa='docker ps -a'
alias dcup='docker-compose up'
alias dcdown='docker-compose down'
alias dstatus='docker-compose exec dev supervisorctl status'
alias dlog='docker-compose logs dev'
alias dstopall='docker stop $(docker ps -aq)'
alias drmall='docker ps -aq | xargs docker stop | xargs docker rm'

# go shortcuts
alias gotest="reflex -r '\.go$' -d fancy -- sh -c 'echo \"CHANGE DETECTED. RUN TEST:\"; go test ./...; echo \"DONE\n\"'"

# python shortcuts
alias pipr='pip install -r requirements.txt'
alias pipi='pip install'
alias 'pip i'='pip install'

alias acti='python3 -m venv venv && source venv/bin/activate'
alias act='source venv/bin/activate'
alias dec='deactivate'

###########################################################################
# WebDev Utils
###

alias nmod_sizes='find . -name "node_modules" -type d -prune -print | xargs du -chs'

function curlj() {
    curl "$@" | python -m json.tool
}

# Heroku
alias hlogs='heroku logs'
alias hpg='heroku pg:psql'
alias hdump='heroku pg:backups capture;curl -o latest.dump `heroku pg:backups public-url`'

###########################################################################
# Platform specific
###

set unamestr=`uname`

# Linux
if [[ "$unamestr" == 'Linux' ]]; then
    alias nauti='nautilus --no-desktop > /dev/null 2>&1 &'

    alias lightterm='~/habitat/base/themes/one-light'
    alias matterm='~/habitat/base/themes/material'

    alias rewifi='nmcli nm wifi off && nmcli nm wifi on'
    alias dark='xset dpms force off' # black the screen

# Mac OS X
elif [[ "$(uname)" == 'Darwin' ]]; then
    # autojump
    [ -f /opt/homebrew/etc/profile.d/autojump.sh ] && . /opt/homebrew/etc/profile.d/autojump.sh

    # aliases
    alias timestamp='date +%s | pbcopy; echo "Timestamp copied to clipboard"; date +%s;'
    alias subl='open -a "Sublime Text"'
    alias md='open -a MacDown'
    alias sweepds='find . -name "*.DS_Store" | xargs rm -v'
    alias cdev='open -n -a /Applications/Google\ Chrome.app/Contents/MacOS/Google\ Chrome --args --user-data-dir="/tmp/chrome_dev_test" --disable-web-security'
fi

###########################################################################
# Utilities
###

source ~/.config/private/supa-aliases 

alias sweep='find . -name "*~" | xargs rm -v; find . -name "*#" | xargs rm -v; find . -name ".#*" | xargs rm -v;'
alias sudosweep='find . -name "*~" | xargs sudo rm -v; find . -name "*#" | xargs sudo rm -v;'

alias speedtest='curl -s https://raw.githubusercontent.com/sivel/speedtest-cli/master/speedtest.py | python -'

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
