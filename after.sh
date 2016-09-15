#!/bin/sh

# If you would like to do some extra provisioning you may
# add any commands you wish to this file and they will
# be run after the Homestead machine is provisioned.

apt-get -y update && apt-get -y install libpq-dev

## Install GTFSDB - no longer needed.
# if [ ! -d "/home/vagrant/gtfsdb" ]; then
  # apt-get -y update && apt-get -y install python-psycopg2
#   git clone https://github.com/OpenTransitTools/gtfsdb.git /home/vagrant/gtfsdb
#   cd /home/vagrant/gtfsdb && easy_install zc.buildout && easy_install . && buildout install prod postgresql
# fi

## Install GTFSRDB
if [ ! -d "/home/vagrant/gtfsrdb" ]; then
  apt-get -y update && apt-get -y install python-psycopg2
  pip install --upgrade gtfs-realtime-bindings sqlalchemy
  git clone https://github.com/tmelliott/gtfsrdb.git /home/vagrant/gtfsrdb
  ln -fs /home/vagrant/gtfsrdb/gtfsrdb.py /usr/local/bin/gtfsrdb.py
fi
