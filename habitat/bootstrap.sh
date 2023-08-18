#!/usr/bin/env sh

current_user=$(whoami)

# Check if the user is not "longh"
if [ "$current_user" != "longh" ]; then
    echo "Logged in user is not longh. Creating user longh."

    sudo useradd -m -G sudo longh
    sudo passwd longh
fi

sudo su longh -c " \
    alias h='/usr/bin/git --git-dir=$HOME/.habitat/ --work-tree=$HOME'; \
    git clone --bare https://github.com/loong/habitat $HOME/.habitat; \
    h checkout; \
    h config --local status.showUntrackedFiles no;"
