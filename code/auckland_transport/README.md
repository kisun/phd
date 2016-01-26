# Auckland Transport GTFS Analysis

This directory will be focused on analysing the Auckland Transport GTFS feed, available from https://api.at.govt.nz/docs/v1.


### Setup

The first step is to add the API key:
```{bash}
$ echo API_KEY > apikey.txt
```

Necessary python tools:
```{bash}
sudo apt-get install python-pip python-sqlalchemy
pip install -U pip setuptools
```
Install Google Protobuf from https://github.com/google/protobuf (both C++ and python).

Then run `make` to download the necessary components of the GTFS feed, create a database, and fill it out.

***

### To-do list

- ~~~extract GTFS data from files and add to database (preferably using `gtfsdb-load` or similar)~~~
- ~~~save latest GTFS real-time data to a database (preferably using `gtfsrdb` or similar)~~~
  - `trip_start_date` needs calculating manually by the looks of things ...
- ~~~set up a block (either by using the `block_id` column, or manually creating one using historical data), and~~~ write a function to automate it for a given `vehicle_id`/`trip_id` and `date`/`timestamp`
- implement a simple __Particle Filter__ to replicate the standard Kalman Filter used in previous studies
- compare the PF results to those from the KF, as well as basic schedule data
- compare different __summary statistics__ by using the posterior predictive distribution of _time until arrival_
- investigate __credible intervals__ for providing more informative predictions
- use alternative models to model the state space
- other methods of predicting time until arrival
