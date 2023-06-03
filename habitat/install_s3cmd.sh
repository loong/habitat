#!/bin/sh

wget https://sourceforge.net/projects/s3tools/files/s3cmd/2.0.1/s3cmd-2.0.1.tar.gz
tar xvfz s3cmd-2.0.1.tar.gz
cd s3cmd-2.0.1
sudo python setup.py install
cd ..
rm -rf s3cmd-2.0.1 s3cmd-2.0.1.tar.gz
