# Title page

Good morning everyone!
Pleasure to be here this morning ...
I'm a PhD student at the University of Auckland across the road ...
Main goal - develop model - improved predictions of bus arrival time ...
Used to provide ETA countdown at stops - journey planning applications.
This morning - talking about approach using a vehicle and road state model simultaneously ...



# Overview

First a quick motivation
Then I'll give a brief overview of the two models ...
Then discuss how they can be used to make predictions ...


# What's wrong with the current system?

Main problem is inaccuracy
Typically ETA's are underestimates - leads to frustration
Current system not robust - as in the following example
- we see app reporting "7 stops away" - the loop is anti-clockwise
- but then it appears bus gone around loop, departed from stop!
- but actually second observation was an error - carried forward -> still departed
Another shortcoming - all this data available - only use a fraction of it for predictions!
- some models (not used by AT) that use all busses on a common route
- none that seem to use generic road state that's not directly paired with particular routes

-> intention to improve accuracy
-> our model solves "departed" issue and several others for free!
-> we'll see later how non-route-specific speed information is used


# Vehicle State Model

First we need to convert GPS data to something we can use

## slide 1

Goal - take GPS observations provided by AT's API - thats Y-k - latitude and longitude
and use this to estimate the state of the vehicle, in terms of it's distance into trip
(so, how far into the route the bus has traveled) as well as speed and several other parameters
- all has to happen in real-time!

## slide 2

Here we see the observation space - point in 2D - route shape is drawn to see how it compares to
first component of X, which is simply a point on a line, 1D
Easy to see how distance corresponds to point on route
However - observations of Y are noisy - GPS error, interference etc

Important point: many models first "guess" what X is - use as data
We instead can transform X into observation space to compute likelihood - shown later
-> solves the "bus has departed" error

## slide 3

We are using PF - which represents X by a sample of points referred to as particles
Provides us with very flexibile framework - no assumptions such as symmetry, unimodality
Provided model sufficient, good coverage of state space -
- at this stage, want good coverage, NOT accuracy - ensure we capture "true" state in sample
see later - likelihood is intuitive

Involves two steps
Prediction step - predict all states where bus COULD be before looking at observation
Update step - compare predictions to true observation - effectively remove particles no longer plausible


## slide 4 - Step 1: predict

So, prediction step - start at previous state
Then transition each particle to obtain predictions for the next observation
Transition function f - starts at previous state,
- adds system noise - allow speed to vary - a simple truncated normal distribution
 so vehicles can't go backwards
- determine location after travelling at speed for amount of time
need to account for possiblity of stopping at stops along the way
- for each particle, Bernoulli probability of stopping
- and how long it waits at the stop
- overall dwell time - time lost due to stopping - has minimum value "gamma"

## slide 5 -pictures!

see - all particles have same speed - add system noise to speed
then let particles travel - some stop at the stop, others don't
left with sample of state predictions
-> next step involves determining which of these capture the real bus

## slide 6- update

update step - involves determining liklehood
Intead of guessing "x" from the data, - transform particles so we can compare directly with observation
- first - transform from point on line to lat/lon (distance along 2D line)
- then center particles on obs and project on flat plane - equal scale in all directions
- simply bivariate normal likelihood - because of projection, distibution symmetrical
- So we get simple formula for likleihood of each particle
Proportional sufficient - used to compute weights
Peform weighted resample - retain particles plausible given data

## slide 7 - pictures

start where we left off - predictions of state
show on line - transform into lat/lon space
Now zoom in - add observation
Center particles on observation - project onto flat plane
Compute likelihood and weights - resample
Left with posterior state estimate!

Note: in practice, use many more particles - better coverage.



# Road state model

Now move on to road state - that is, speed along stretches of the road

## slide 1

First requirement - get speed estimates from PF
Then identify stretches - or segments - of road common to multiple routes
And use data from ALL busses -> estimate road segment speed

Already have speed estimates

## slide 2- step 2

Have to identify segments of road
Decided to use intersections - whenever routes MEET, it will be at an intersection!
-> easy to scale up slowly (add more routes later)

## slide 3- step 3

So for step 3, we apply a KF
Road state represented by MEAN VECTOR and COVARAIANCE MATRIX
Standard KF algorithm sufficient - normal adequate
PF speed estimates -> mean = data; variance used to estimate R = measurment error
-> several busses traveling similar speed -> low variance -> ACCURATE measurement
-> several busses different speeds -> high variance -> INACCURATE measurement
Filter weights observations appropriately


# Predicting arrival time

Finally we can use these two models together to make arrival time predictions

## slide 1

compare four methods
just quickly - a bit of extra notation -
If Ta/Td missing, can use particle filter estimates of arrival time
-> distribution from particles!

## 1. schedule
just a baseline for comparison

## 2. schedule deviation
this is likely what the current system uses
- but unsure as it's a black box and nobody from AT will explain it to us
effectively - deviation from schedule at last stop carried forward to upcoming stops

## 3. vehicle state
simply uses current speed to determine how long to travel to stop
- for each particle to get a distribution
- include dwell time uncertainty
- thus can be multimodal!

## 4. road state
instead of uses PF speed, use KF speed
- travel time to end of current segment
- travel time through intermediate segments
- travel time to stop
- again for each particle, sampling speed from KF distibution
- and as with vehicle state, incorporate dwell time uncertainty


## pictures!

so here's a visual of the four methods
- grey boxes are actual arrival times reported by API - width represents dwell time
- black - scheduled arrival times
- orange - schedule deviation - beginning of route, behind schedule, so carried forward
- green - vehicle state - ignores any intermediate changes in traffic
- blue - road state - uses speeds as estimated by previous trips (this example - only one route being used)

PLAY:
- see that vehicle state is far too variable,
while road state remains fairly constant
- deviation + vehicle state ~OK for short term, unreliable for long

-> this is without any tuning of dwell times etc as not yet any historical data!
-> KF often underestimates ETA - potentially because of dwell times

## Conclusions

Nothing to say about schedule - it's fixed!
Deviation - mostly relies on schedule accuracy, not good for long range
Vehicle state - too much variability to be useful at long-range
Road state - seems to perform well even at long range, low variability

## Intervals

Finally, we need to relay predictions + their uncertainty to commuters.
Some variabilty we cannot escape
- e.g., no stopping/lights vs lots of stopping + many red lights
Prediction intervals seem appropriate
- easy to compute from sample quantiles
- arguably intuitive
  - give passengers information about what time they need to be at stop by
  - but informed that it might take a little longer
- necessary to bias - lower bound ~earliest you can expect the bus


## What's next?

So what are our future plans?
First up - add more routes - see if doing so has any value, compared to "single route" methods.
Build up a historical database
- allow estimate dwell time parameters
- and potentially better model for road states - relationships between segments
In order to scale up to all routes, need to increase performance of model
-> every second = faster prediction = more up-to-date!
-> don't want predictions to be 30 seconds old by the time they are released!

