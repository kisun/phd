#!/bin/bash

echo "Backing up the GTFS Realtime database ..."

DIR=/home/tell029/Documents/uni/phd/code/MBTA
BDIR=$DIR/backups
DATE=$(date +%Y-%m-%d_%H%M)
BACKUP=gtfs-historical.backup-$DATE.db
cp $DIR/gtfs-historical.db $BDIR/$BACKUP

LATEST=gtfs-historical.backup-latest.db
rm -f $BDIR/$LATEST
ln -s $BACKUP $BDIR/$LATEST

echo "Deleting duplicate rows ..."

sqlite3 $BDIR/$BACKUP "DELETE FROM vehicle_positions WHERE oid NOT IN (SELECT MAX(oid) FROM vehicle_positions GROUP BY vehicle_id, timestamp);"

echo "Done."
