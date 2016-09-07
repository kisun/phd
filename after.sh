#!/bin/sh

# If you would like to do some extra provisioning you may
# add any commands you wish to this file and they will
# be run after the Homestead machine is provisioned.
#
apt-get install python-psycopg2
git clone https://github.com/OpenTransitTools/gtfsdb.git
cd gtfsdb && easy_install zc.buildout && easy_install . && buildout install prod postgresql
