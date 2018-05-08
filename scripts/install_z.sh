#!/bin/sh

wget https://raw.githubusercontent.com/rupa/z/master/z.sh
mv z.sh ~/.z.sh

if ! grep -Fxq "source ~/.z.sh" ~/.zshrc
then
    echo "source ~/.z.sh" >> ~/.zshrc
fi

wget https://raw.githubusercontent.com/rupa/z/master/z.1
sudo mv z.1 /usr/share/man/man1/
