# ProcessMiningProlog
## EPITA - LKR Class

Group members:

- amadou_a
- douill_a
- billio_r
- legoua_g
- chowdu_f

The purpose of this project was to take a first approach to Process Mining.

Given input logs in the form "[<a, b, c>, <a, b, d>, <b, d>^2]", the program builds a model
and return an event script describing the model.

# How to launch the program


# Implementation details
The process discovery is done using the Inductive Mining Algorithm, created by Leemans et al.,2016 and described in
their paper *Scalable process discovery and conformance checking. Software & Systems Modeling*. 

The Inductive Mining Algorithm turns event logs into a model represented with a Directly Follows Graph.
This graph is then split up in smaller graphs using different cut operators:
- The sequential cut, which occurs when an activity A follows B but B never follows A.
- The exclusive cut, where A and B never reach each other.
- The parallel cut, where A and B can appear in any intertwined order.
- Finally, the loop cut, which is composed of a body and redo activities.

For example files, please take a look at the examples folder.
