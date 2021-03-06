Improved Prediction of Bus Time Arrival
=======================================

PhD Statistics, University of Auckland
--------------------------------------

Supervisor: Professor Thomas Lumley


------------------------------------------------------------------------------------------------------
Public transport prediction has received a lot of attention in the past, with several modelling
techniques having been developed.  However, many of these only account for information regarding a
single bus or route.  Further, anomalies caused by inadequate predictions (e.g., buses going
"backwards in time" due to sudden increases in ETA) are encountered which understandably frustrate
commuters.

A potential cause of these problems is that only data corresponding directly to the bus or route
being predicted is used. We would like to implement Bayesian models that use all of the data - both
live and historical - simultaneously to predict all bus arrival times, with the goal to improve
prediction accuracy (e.g., if a bus is late due to heavy traffic, the next one along the same road
probably will be too - even if it's a different route altogether). Inevitably, this introduces
computational problems that will need to be overcome - an ideal system will be capable of making new
predictions every minute or so.

The choice of summary statistic is also something we will investigate, especially the use of
interval rather than point estimates. This could help to reduce anomalies such as "time-travelling
buses", and give commuters a better idea of expected waiting times. From these new ideas we
can investigate applications (e.g., journey planning) and accessibility options so commuters can
readily access the information. We will be working with Auckland Transport, who have already
agreed to give us access to the necessary data.

------------------------------------------------------------------------------------------------------


