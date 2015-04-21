#!/bin/bash

USER=long

function mir {
    echo "Backing up $2..."
    cp -r --parents $1 mirror
}

echo
echo "--------------------------------------------------"
echo "Create Local Mirror Backup"
echo "--------------------------------------------------"
mir ~/.bashrc                    "bashrc"
mir "/etc/udev/rules.d/*"        "udev rules"
mir /etc/hosts                   "hosts"
mir ~/.emacs.d/"*"               ".emacs.d"

echo 
echo "--------------------------------------------------"
echo "Clean up"
echo "--------------------------------------------------"
find . -name "*~" | xargs rm -v; find . -name "*#" | xargs rm -v;
rm mirror/home/$USER/.emacs.d/session.*
