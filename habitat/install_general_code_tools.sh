#!/bin/bash

unamestr=`uname`

# Linux, assuming it will be apt-get based system
if [[ "$unamestr" == 'Linux' ]]; then
    # todo add autojump
    sudo apt-get install tmux htop trash-cli git
    
# Mac OS X
elif [[ "$unamestr" == 'FreeBSD' || "$unamestr" == 'Darwin' ]]; then
    brew install tmux the_silver_searcher trash htop git autojump
fi
