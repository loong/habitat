#!/bin/bash

# Linux, assuming it will be apt-get based system
if [[ $(uname -s) == "Linux" ]]; then
    sudo apt-get update
    sudo apt-get install -y \
        zsh \
        git glances htop trash-cli \
        emacs-nox \
        tmux tmuxinator \
        bat exa httpie \
        ripgrep autojump

# Mac OS X
elif [[ "$unamestr" == 'FreeBSD' || "$unamestr" == 'Darwin' ]]; then
    if ! command -v brew &> /dev/null; then
        echo "Homebrew not found, installing..."
        /bin/bash -c "$(curl -fsSL https://raw.githubusercontent.com/Homebrew/install/HEAD/install.sh)"
    fi

    brew install \
        git tmux glances htop trash \
        emacs \
        tmux tmuxinator \
        bat exa httpie \
        ripgrep autojump
fi
