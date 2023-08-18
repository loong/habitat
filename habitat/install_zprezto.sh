#!/usr/bin/env sh

git clone --recursive https://github.com/sorin-ionescu/prezto.git ~/.zprezto

cd

ln -s ./.zprezto/runcoms/zlogin ./.zlogin
ln -s ./.zprezto/runcoms/zlogout ./.zlogout
ln -s ./.zprezto/runcoms/zpreztorc ./.zpreztorc
ln -s ./.zprezto/runcoms/zprofile ./.zprofile
ln -s ./.zprezto/runcoms/zshenv ./.zshenv

chsh -s `which zsh`
