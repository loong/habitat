#!/bin/bash

if [ -f ~/.longh/.java8.locks ];
then
    echo "Java8 already installed"
    exit
fi

sudo add-apt-repository ppa:webupd8team/java
sudo apt update
sudo apt install -y oracle-java8-installer
sudo apt install -y oracle-java8-set-default

sudo bash -c "echo \"export JAVA_HOME=/usr/lib/jvm/java-8-oracle/\" >> /etc/bash.bashrc"
sudo bash -c "echo \"export PATH=$PATH:$JAVA_HOME/bin\" >> /etc/bash.bashrc"

sudo bash -c "echo \"export JAVA_HOME=/usr/lib/jvm/java-8-oracle/\" >> /etc/zsh/zshrc"
sudo bash -c "echo \"export PATH=$PATH:$JAVA_HOME/bin\" >> /etc/zsh/zshrc"

touch ~/.longh/.java8.locks
