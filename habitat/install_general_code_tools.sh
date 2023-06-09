#!/bin/bash

unamestr=`uname`

# Linux, assuming it will be apt-get based system
if [[ "$unamestr" == 'Linux' ]]; then
    # todo add autojump
    sudo apt-get install tmux glances htop trash-cli git bat exa httpie
    
# Mac OS X
elif [[ "$unamestr" == 'FreeBSD' || "$unamestr" == 'Darwin' ]]; then
    if ! command -v brew &> /dev/null
    then
	echo "Homebrew not found, installing..."
	/bin/bash -c "$(curl -fsSL https://raw.githubusercontent.com/Homebrew/install/HEAD/install.sh)"
    fi
    brew install tmux the_silver_searcher trash glances git autojump bat exa httpie tmuxinator
fi
