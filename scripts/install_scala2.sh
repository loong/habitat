#!/bin/bash

if [ `javac -version` != "javac 1.8*"]
then
    echo "Please install java"
    exit
fi

sudo apt-get remove scala-library scala
sudo wget www.scala-lang.org/files/archive/scala-2.11.8.deb -O /tmp/install_scala.deb
sudo dpkg -i /tmp/install_scala.deb

echo "deb https://dl.bintray.com/sbt/debian /" | sudo tee -a /etc/apt/sources.list.d/sbt.list
sudo apt-key adv --keyserver hkp://keyserver.ubuntu.com:80 --recv 2EE0EA64E40A89B84B2DF73499E82A75642AC823
sudo apt-get update
sudo apt-get install -y sbt
