#!/bin/bash

unamestr=`uname`

# Linux, assuming it will be apt-get based system
if [[ "$unamestr" == 'Linux' ]]; then
    # todo add autojump
    sudo apt-get install \
        git glances htop trash-cli \
        tmux tmuxinator \
        bat exa httpie \
        ripgrep silversearcher-ag \
        autojump

# Mac OS X
elif [[ "$unamestr" == 'FreeBSD' || "$unamestr" == 'Darwin' ]]; then
    if ! command -v brew &> /dev/null
    then
	echo "Homebrew not found, installing..."
	/bin/bash -c "$(curl -fsSL https://raw.githubusercontent.com/Homebrew/install/HEAD/install.sh)"
    fi
    brew install \
        git tmux glances htop trash \
        tmux tmuxinator \
        bat exa httpie \
        ripgrep the_silver_searcher \
        autojump
fi
