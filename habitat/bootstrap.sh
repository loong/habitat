#!/usr/bin/env sh

current_user=$(whoami)

# Check if the user is not "longh"
if [ "$current_user" != "longh" ]; then
    echo "Logged in user is not longh. Creating user longh."

    sudo useradd -m -G sudo longh
    sudo passwd longh
fi

DIR=/home/longh/

sudo su longh -c " \
    cd $DIR; \
    /usr/bin/git --git-dir=$DIR/.habitat/ --work-tree=$DIR clone --bare https://github.com/loong/habitat $DIR/.habitat; \
    /usr/bin/git --git-dir=$DIR/.habitat/ --work-tree=$DIR checkout; \
    /usr/bin/git --git-dir=$DIR/.habitat/ --work-tree=$DIR config --local status.showUntrackedFiles no;"

sudo su longh
