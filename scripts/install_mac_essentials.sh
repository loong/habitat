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

    chsh -s `which zsh`
}

function install_brews {
    if [ -f ~/.longh/.brews.locks ];
    then
	echo "Apt-gets already installed"
	return
    fi

    # Install homebrew
    /bin/bash -c "$(curl -fsSL https://raw.githubusercontent.com/Homebrew/install/HEAD/install.sh)"
    echo '# Set PATH, MANPATH, etc., for Homebrew.' >> /Users/longh/.zprofile
    echo 'eval "$(/opt/homebrew/bin/brew shellenv)"' >> /Users/longh/.zprofile
    eval "$(/opt/homebrew/bin/brew shellenv)"

    brew update
    brew install tmux htop trash-cli unzip tree emacs the_silver_searcher

    mkdir -p ~/.longh
    touch ~/.longh/.brews.locks
}

function install_configs {
    if [ -f ~/.longh/.configs.locks ];
    then
	echo "Configs already installed"
	return
    fi

    echo "Source aliases"
    echo "source $HABITAT_PATH/base/aliases" >> ~/.zprezto/runcoms/zshrc

    echo "Install init.el"
    mkdir -p ~/.emacs.d/
    ln -s $HABITAT_PATH/bck/init.el ~/.emacs.d/init.el

    echo "Instal .tmux.conf"
    ln -s $HABITAT_PATH/bck/tmux.conf ~/.tmux.conf

    touch ~/.longh/.configs.locks
}


print_header "Brew essentials"
install_brews

print_header "Install Pretzo"
install_pretzo

print_header "Install aliases, files"
install_configs
