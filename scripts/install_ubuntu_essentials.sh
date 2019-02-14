#!/bin/bash
HABITAT_PATH=`pwd`/..

function print_header {
    echo "------------------------------------------------------------------------"
    echo "   $@"
    echo "------------------------------------------------------------------------"
}

function install_pretzo {
    if [ -d ~/.zprezto ];
    then
	echo "Pretzo already installed"
	return
    fi
    
    git clone --recursive https://github.com/sorin-ionescu/prezto.git ~/.zprezto

    # Backup zsh config if it exists
    if [ -f ~/.zshrc ];
    then
	mv ~/.zshrc ~/.zshrc.backup
    fi

    # Create links to zsh config files
    ln -s ~/.zprezto/runcoms/zlogin ~/.zlogin
    ln -s ~/.zprezto/runcoms/zlogout ~/.zlogout
    ln -s ~/.zprezto/runcoms/zpreztorc ~/.zpreztorc
    ln -s ~/.zprezto/runcoms/zprofile ~/.zprofile
    ln -s ~/.zprezto/runcoms/zshenv ~/.zshenv
    ln -s ~/.zprezto/runcoms/zshrc ~/.zshrc

    chsh `which zsh`
}

function install_apt_gets {
    if [ -f ~/.longh/.aptget.locks ];
    then
	echo "Apt-gets already installed"
	return
    fi

    sudo apt-get update
    sudo apt-get install -y git zsh emacs24-nox tmux htop silversearcher-ag

    mkdir -p ~/.longh
    touch ~/.longh/.aptget.locks
}

function install_configs {
    if [ -f ~/.longh/.configs.locks ];
    then
	echo "Configs already installed"
	return
    fi

    echo "Source aliases"
    echo "source $HABITAT_PATH/base/aliases" >> ~/.zprezto/runcoms/zshrc

    touch ~/.longh/.aptget.locks
}


print_header "Apt-get essentials"
install_apt_gets

print_header "Install Pretzo"
install_pretzo

print_header "Install aliases, files"
install_configs
