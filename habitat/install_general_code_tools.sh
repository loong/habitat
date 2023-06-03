#!/bin/bash

unamestr=`uname`

# Linux, assuming it will be apt-get based system
if [[ "$unamestr" == 'Linux' ]]; then
    # todo add autojump
    sudo apt-get install tmux glances htop trash-cli git bat exa httpie
    
# Mac OS X
elif [[ "$unamestr" == 'FreeBSD' || "$unamestr" == 'Darwin' ]]; then
    brew install tmux the_silver_searcher trash glances git autojump bat exa httpie
fi
