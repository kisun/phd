#!/bin/sh

# If you would like to do some extra provisioning you may
# add any commands you wish to this file and they will
# be run after the Homestead machine is provisioned.

## Install GTFSDB
apt-get -y update && apt-get -y install python-psycopg2
git clone https://github.com/OpenTransitTools/gtfsdb.git
cd gtfsdb && easy_install zc.buildout && easy_install . && buildout install prod postgresql

## Install GTFSRDB
cd
pip install --upgrade gtfs-realtime-bindings
git clone https://github.com/tmelliott/gtfsrdb.git
ln -fs ~/gtfsrdb/gtfsrdb.py /usr/local/bin/gtfsrdb.py
