breed [ states state ]  ;states of our MDP
breed [ paths path ]    ;paths between states of our MDP
breed [ legends legend ];shows action colors

;reward for being in the state
;current utility of the state
;flag indicating if this is inital state
;list of all the paths leading out from a state
;the action the policy will choose
states-own [ reward c-util init-state outbound-paths policy-action ]

;indicates which action this path is associated with
;indicates the probability associated with this path
paths-own [ action-id path-strength ]

globals [ max-acts num-paths delta finished ]

to setup
  ca
  set-default-shape states "circle"
  set-default-shape paths "line"
  set-default-shape legends "face happy"
  set finished false
  create-states num-states
  setup-states
  build-legend
  build-mdp
end

to setup-states
  ask states [
    set size size-state  ;make states easier to see
    set color red
    set reward random 100  ;set a random reward for each state
    set init-state 0 ;set to NOT inital state
    set c-util 0  ;start as 0 for value iteration
    set outbound-paths []  ;empty list for new states
    set label reward  ;display the reward
    
    rt ( num-states / 360 ) ]  ;circular heading to maintain an even spacing between the states
  ask min-one-of states [ reward ] [  ;find the smallest reward state and set as the inital state
    set color grey 
    set init-state 1 ] ;set initial state
  ask states [ 
    repeat 15 [ 
      fd 1
      display ] ]
end

to build-legend
  locals [ index ]
  create-legends num-actions
  ask legends [ set size 2 ]
  set index 0
  foreach values-from legends [who] [
    ask legend ? [
      set color (140 - (35 + 10 * index))
      set label index
      setxy ( 17 - (num-actions * 2) + (index * 2) ) 16.5
    set index index + 1 ] ]
end

to go
  if (finished) [stop]
  value-iteration
end

to build-mdp  ;a bunch of trees really, since NetLogo doesn't like self-links or multiple links between two nodes
  ;the list of connected states
  ;destination node
  ;starting node
  ;how many times we run the tree setup
  ;current tree we are building
  ;list of treetops
  locals [ mdp from-state to-state cycles treenum treetops ]
  ifelse ( num-actions > num-states - 1 ) [ set cycles num-states - 1 ] [ set cycles num-actions ]
  set treenum 0  ;which tree we are working on
  set treetops []  ;starting node
  repeat cycles [
    ifelse ( treenum = 0 ) [ ;set top of tree
      set mdp (list state value-from one-of states with [init-state = 1] [who]) ] [  ;first time use the intial state
      set mdp (list state value-from one-of states with [ not member? self treetops ] [who] ) ]  ;otherwise choose a state that hasn't gotten to be a treetop yet
    set treetops lput item 0 mdp treetops  ;add to the list of treetops after a member is chosen
    while [ length mdp < num-states ] [  ;fill out the tree
      set from-state item random length mdp mdp  ;pick a state that has already been connected
      set to-state one-of states with [ not member? self mdp ]  ;pick a state that has not been connected
      if( to-state != nobody ) [ ;iff successful choice, create a link to that state
        ask from-state [
          if(not(__out-path-neighbor? to-state)) [  ;unlucky case that there is already a link, back out and don't create
            set outbound-paths lput to-state outbound-paths  ;yay, add a link
            set num-paths num-paths + 1
            __create-path-to to-state [] ] ]
        set mdp lput to-state mdp ] ]
    set treenum treenum + 1 ]
    assign-actions
end

to-report randomized-list [ limit ]  ;build a list with limit elements with the unique numbers 0-limit shuffled randomly
  locals [ end-list temp ]
  set end-list []
  while [length end-list < limit] [
    set temp random limit
    if( not member? temp end-list ) [ set end-list lput temp end-list ] ]
  report end-list
end

;has to use current links to assign actions in such a way that each action leaving a state has a probabilty sum of 1
;unsolveable situations usually only arise when the action:state ratio is very small (+20 states so I have limited the amount of states)
to assign-actions
;action to be assigned
;list of choices
;a counter to move through the list of choices
;path strength
;the total strength of all of a particular action leaving a state
locals [ act act-choices counter p-str tot-act-weight]
  set counter 0
  ask states [
    while [count __my-out-paths with [ path-strength = 0 ] > 0] [
      set act-choices randomized-list num-actions  ;new for each iteration
      set act item counter act-choices
      set tot-act-weight sum values-from __my-out-paths with [action-id = act] [path-strength]
      ifelse (path-probability = 1) [ set p-str path-probability * 100 ] [  ;force determinism
        set p-str (100 * path-probability) - 0.2 * random( 100 * path-probability ) ]
      set p-str precision ( p-str / 100 ) 2
      ifelse( tot-act-weight + path-probability > 1 ) [ set p-str (1 - tot-act-weight) ] [  ;force a summation to 100%
        if( tot-act-weight = 1 ) [  ;go to a new action
          set counter counter + 1
          set act item (counter) act-choices ] ] 
      ask one-of __my-out-paths with [(path-strength = 0)] [
        set path-strength precision p-str 2
        set action-id act
        set color (140 - (35 + 10 * action-id))
        set label path-strength ] ] ]
end

;Value Iteration Algorithm
to value-iteration
locals [ prev-util ]
  without-interruption [
    if( not finished) [
    ask states [
      set delta 0
      set prev-util c-util
      set c-util precision ( reward + (discount-factor * max-act)) 2
      set label c-util
      if ( abs ( prev-util - c-util ) > delta ) [ set delta (abs ( prev-util - c-util ))]
      set color (140 - (35 + 10 * policy-action))
      ifelse( init-state = 1 ) [ set color color + 2 ] [ set color color - 1 ] ] ] ]
  if ( delta <= (error-tolerance * (1 - discount-factor) / discount-factor) ) [ set finished true ]
  graph-policy-action
end

;return the expected utility
to-report max-act
;expected utility of a connected state
locals [ expect-util max-action ]
  set expect-util 0  ;reset
  ask __my-out-paths [
    if((path-strength * value-from __other-end [ reward ]) > expect-util) [ 
    set expect-util (path-strength * value-from __other-end [ c-util ])
    set max-action action-id ]
  ]
  set policy-action max-action
  report expect-util
end

to graph-policy-action
  locals [ counter ]
  set-current-plot "Policy Action"
  set-plot-y-range -1 1 + max values-from states [policy-action]
  set-plot-x-range 0 1 + num-states
  plot-pen-reset
  repeat num-states [
    plot value-from state counter [policy-action]
    set counter counter + 1 ]
end
@#$#@#$#@
GRAPHICS-WINDOW
303
10
733
461
17
17
12.0
1
10
1
1
1
0
0
0
1
-17
17
-17
17

CC-WINDOW
5
475
742
570
Command Center
0

BUTTON
65
125
128
158
setup
setup
NIL
1
T
OBSERVER
T
NIL

SLIDER
10
46
182
79
num-states
num-states
2
10
7
1
1
NIL

SLIDER
10
82
182
115
num-actions
num-actions
1
8
3
1
1
NIL

SLIDER
10
10
182
43
path-probability
path-probability
0.01
1
0.57
0.01
1
NIL

MONITOR
12
328
87
377
Total Paths
num-paths
0
1

SLIDER
184
10
300
43
discount-factor
discount-factor
0
1
1.0
0.01
1
NIL

SLIDER
184
46
300
79
error-tolerance
error-tolerance
0.01
0.45
0.28
0.01
1
NIL

BUTTON
145
155
287
188
Value Iteration Step
go
NIL
1
T
OBSERVER
T
NIL

BUTTON
157
119
270
152
Value Iteration
go
T
1
T
OBSERVER
T
NIL

MONITOR
12
380
87
429
Delta
delta
2
1

MONITOR
90
379
163
428
e * (1-y)/y
(error-tolerance * ((1 - discount-factor) / discount-factor))
2
1

SLIDER
185
82
299
115
size-state
size-state
1
6
4
1
1
NIL

PLOT
89
227
289
377
Policy Action
state
action id
0.0
10.0
0.0
10.0
true
false
PENS
"default" 1.0 1 -16777216 true

@#$#@#$#@
Title: Value Iteration
Author: William Wiles
Description:

Small demo of the Value Iteration Algorithm

------


HOW IT WORKS
------------
The MDP is made with a series of trees.  First a tree is made from the initial state to the rest of the states and then based on the number of actions chosen trees are made from other states chosen at random.  As a special note, it is noticable that the preliminary tree that connects the initial state to the rest of the states is a very good candidate for containing the solution path for the iteration algorithm.  However, since the links are setup independent of when the probabilities are assigned, this is not absolute.  

I ran into much difficulty in picking a way to build the MDP since the link turtles are fairly constrained.  This is the third revision and I admit it is not perfect, an action from a state can only implicitly leave the agent in the same state; i.e. when the links are built the calling state will not have itself as a choice, and therefore it is not as random as it ought be.  The way an action can cause the agent to remain in the same state occurs when the summation of all probabilities corrisponding to that action at that state not equaling 1.  The remainder is the probabilty of that action causing the agent to remain in the same state.  Another issue that I've lived with by virtue of complexity is that the build-MDP function likes complete graphs.  If there is a high number of action choices relative to the number of states (somewhere around 1:1) then you will very likely get a complete or near complete graph.  Had I addeded support for links to not be associated to just one action, i.e. a link holds an "action list" then this might not be the case.  Experimenting with each link holding a list of actions proved extremely complex when summing individual probilities for actions from a state, and proved to possibly jeopordize the manner for remaining in the same state.

The value iteration algorithm is implemented almost exactly as mentioned in Chapter 1 pg. 13, instead of looping through all states, each state is called to perform the utility update on its own.  Since the MDPs are not particularly "deep" with the maximum set at 10 states, a solution is found relatively quickly.  


HOW TO USE IT
-------------
Setup will build the MDP to solve and assign rewards to each state.  The initial state
will be indicated in blue and it will have the smallest reward.  After setting up, you
can either step through the value-iteration algorithm one step at a time or run it till
completion.  

Notice that the reward which is initially displayed in the state will change to the current utility of the state.  The error function as well as the current delta is displayed in the lower left, and the current policy will be updated in each iteration of the algorithm.  This graph is NOT a histogram.  The states run accross the x-axis and the value associated with each state is the number of the action that the policy has chosen for that state.

In the upper right of the world there are smiley faces, these display the colors corrisponding to the action numbers.

While value-iteration is running, the color of the state will corrispond (similarly in hue) to the action chosen by the policy.  The intial state will be slightly brighter, while other states will be slightly darker.  This serves as only as a visual aid to the Value Action graph which is more accurate in showing the policy for each state.


THINGS TO NOTICE
----------------
Sometimes during setup, links will flash back and forth between colors, this is due to
a problem with the number of states vs the number of links assigned, with a dependancy
on the maximum probability allowed to a link.  Sometimes setup will become confused.
Halting and restarting usually resolves the problem as it is rare that it occurs.


NETLOGO FEATURES
----------------
The turtle parallelism made writing the iteration portions quite easy, but required much revision on my part.  Much different than the standard function-based programming scheme of Java and C.


CREDITS AND REFERENCES
----------------------
I referenced the distributed breakout algorithm (DBgc.nlogo), http://jmvidal.cse.sc.edu/netlogomas/DBgc.html for strategy on designing graphs with links.

And for API info, http://ccl.northwestern.edu/netlogo/
@#$#@#$#@
default
true
0
Polygon -7500403 true true 150 5 40 250 150 205 260 250

link
true
0
Line -7500403 true 150 0 150 300

link direction
true
0
Line -7500403 true 150 150 30 225
Line -7500403 true 150 150 270 225

airplane
true
0
Polygon -7500403 true true 150 0 135 15 120 60 120 105 15 165 15 195 120 180 135 240 105 270 120 285 150 270 180 285 210 270 165 240 180 180 285 195 285 165 180 105 180 60 165 15

arrow
true
0
Polygon -7500403 true true 150 0 0 150 105 150 105 293 195 293 195 150 300 150

box
false
0
Polygon -7500403 true true 150 285 285 225 285 75 150 135
Polygon -7500403 true true 150 135 15 75 150 15 285 75
Polygon -7500403 true true 15 75 15 225 150 285 150 135
Line -16777216 false 150 285 150 135
Line -16777216 false 150 135 15 75
Line -16777216 false 150 135 285 75

bug
true
0
Circle -7500403 true true 96 182 108
Circle -7500403 true true 110 127 80
Circle -7500403 true true 110 75 80
Line -7500403 true 150 100 80 30
Line -7500403 true 150 100 220 30

butterfly
true
0
Polygon -7500403 true true 150 165 209 199 225 225 225 255 195 270 165 255 150 240
Polygon -7500403 true true 150 165 89 198 75 225 75 255 105 270 135 255 150 240
Polygon -7500403 true true 139 148 100 105 55 90 25 90 10 105 10 135 25 180 40 195 85 194 139 163
Polygon -7500403 true true 162 150 200 105 245 90 275 90 290 105 290 135 275 180 260 195 215 195 162 165
Polygon -16777216 true false 150 255 135 225 120 150 135 120 150 105 165 120 180 150 165 225
Circle -16777216 true false 135 90 30
Line -16777216 false 150 105 195 60
Line -16777216 false 150 105 105 60

car
false
0
Polygon -7500403 true true 300 180 279 164 261 144 240 135 226 132 213 106 203 84 185 63 159 50 135 50 75 60 0 150 0 165 0 225 300 225 300 180
Circle -16777216 true false 180 180 90
Circle -16777216 true false 30 180 90
Polygon -16777216 true false 162 80 132 78 134 135 209 135 194 105 189 96 180 89
Circle -7500403 true true 47 195 58
Circle -7500403 true true 195 195 58

circle
false
0
Circle -7500403 true true 0 0 300

circle 2
false
0
Circle -7500403 true true 0 0 300
Circle -16777216 true false 30 30 240

cow
false
0
Polygon -7500403 true true 200 193 197 249 179 249 177 196 166 187 140 189 93 191 78 179 72 211 49 209 48 181 37 149 25 120 25 89 45 72 103 84 179 75 198 76 252 64 272 81 293 103 285 121 255 121 242 118 224 167
Polygon -7500403 true true 73 210 86 251 62 249 48 208
Polygon -7500403 true true 25 114 16 195 9 204 23 213 25 200 39 123

cylinder
false
0
Circle -7500403 true true 0 0 300

dot
false
0
Circle -7500403 true true 90 90 120

face happy
false
0
Circle -7500403 true true 8 8 285
Circle -16777216 true false 60 75 60
Circle -16777216 true false 180 75 60
Polygon -16777216 true false 150 255 90 239 62 213 47 191 67 179 90 203 109 218 150 225 192 218 210 203 227 181 251 194 236 217 212 240

face neutral
false
0
Circle -7500403 true true 8 7 285
Circle -16777216 true false 60 75 60
Circle -16777216 true false 180 75 60
Rectangle -16777216 true false 60 195 240 225

face sad
false
0
Circle -7500403 true true 8 8 285
Circle -16777216 true false 60 75 60
Circle -16777216 true false 180 75 60
Polygon -16777216 true false 150 168 90 184 62 210 47 232 67 244 90 220 109 205 150 198 192 205 210 220 227 242 251 229 236 206 212 183

fish
false
0
Polygon -1 true false 44 131 21 87 15 86 0 120 15 150 0 180 13 214 20 212 45 166
Polygon -1 true false 135 195 119 235 95 218 76 210 46 204 60 165
Polygon -1 true false 75 45 83 77 71 103 86 114 166 78 135 60
Polygon -7500403 true true 30 136 151 77 226 81 280 119 292 146 292 160 287 170 270 195 195 210 151 212 30 166
Circle -16777216 true false 215 106 30

flag
false
0
Rectangle -7500403 true true 60 15 75 300
Polygon -7500403 true true 90 150 270 90 90 30
Line -7500403 true 75 135 90 135
Line -7500403 true 75 45 90 45

flower
false
0
Polygon -10899396 true false 135 120 165 165 180 210 180 240 150 300 165 300 195 240 195 195 165 135
Circle -7500403 true true 85 132 38
Circle -7500403 true true 130 147 38
Circle -7500403 true true 192 85 38
Circle -7500403 true true 85 40 38
Circle -7500403 true true 177 40 38
Circle -7500403 true true 177 132 38
Circle -7500403 true true 70 85 38
Circle -7500403 true true 130 25 38
Circle -7500403 true true 96 51 108
Circle -16777216 true false 113 68 74
Polygon -10899396 true false 189 233 219 188 249 173 279 188 234 218
Polygon -10899396 true false 180 255 150 210 105 210 75 240 135 240

house
false
0
Rectangle -7500403 true true 45 120 255 285
Rectangle -16777216 true false 120 210 180 285
Polygon -7500403 true true 15 120 150 15 285 120
Line -16777216 false 30 120 270 120

leaf
false
0
Polygon -7500403 true true 150 210 135 195 120 210 60 210 30 195 60 180 60 165 15 135 30 120 15 105 40 104 45 90 60 90 90 105 105 120 120 120 105 60 120 60 135 30 150 15 165 30 180 60 195 60 180 120 195 120 210 105 240 90 255 90 263 104 285 105 270 120 285 135 240 165 240 180 270 195 240 210 180 210 165 195
Polygon -7500403 true true 135 195 135 240 120 255 105 255 105 285 135 285 165 240 165 195

line
true
0
Line -7500403 true 150 0 150 300

line half
true
0
Line -7500403 true 150 0 150 150

pentagon
false
0
Polygon -7500403 true true 150 15 15 120 60 285 240 285 285 120

person
false
0
Circle -7500403 true true 110 5 80
Polygon -7500403 true true 105 90 120 195 90 285 105 300 135 300 150 225 165 300 195 300 210 285 180 195 195 90
Rectangle -7500403 true true 127 79 172 94
Polygon -7500403 true true 195 90 240 150 225 180 165 105
Polygon -7500403 true true 105 90 60 150 75 180 135 105

plant
false
0
Rectangle -7500403 true true 135 90 165 300
Polygon -7500403 true true 135 255 90 210 45 195 75 255 135 285
Polygon -7500403 true true 165 255 210 210 255 195 225 255 165 285
Polygon -7500403 true true 135 180 90 135 45 120 75 180 135 210
Polygon -7500403 true true 165 180 165 210 225 180 255 120 210 135
Polygon -7500403 true true 135 105 90 60 45 45 75 105 135 135
Polygon -7500403 true true 165 105 165 135 225 105 255 45 210 60
Polygon -7500403 true true 135 90 120 45 150 15 180 45 165 90

square
false
0
Rectangle -7500403 true true 30 30 270 270

square 2
false
0
Rectangle -7500403 true true 30 30 270 270
Rectangle -16777216 true false 60 60 240 240

star
false
0
Polygon -7500403 true true 151 1 185 108 298 108 207 175 242 282 151 216 59 282 94 175 3 108 116 108

target
false
0
Circle -7500403 true true 0 0 300
Circle -16777216 true false 30 30 240
Circle -7500403 true true 60 60 180
Circle -16777216 true false 90 90 120
Circle -7500403 true true 120 120 60

tree
false
0
Circle -7500403 true true 118 3 94
Rectangle -6459832 true false 120 195 180 300
Circle -7500403 true true 65 21 108
Circle -7500403 true true 116 41 127
Circle -7500403 true true 45 90 120
Circle -7500403 true true 104 74 152

triangle
false
0
Polygon -7500403 true true 150 30 15 255 285 255

triangle 2
false
0
Polygon -7500403 true true 150 30 15 255 285 255
Polygon -16777216 true false 151 99 225 223 75 224

truck
false
0
Rectangle -7500403 true true 4 45 195 187
Polygon -7500403 true true 296 193 296 150 259 134 244 104 208 104 207 194
Rectangle -1 true false 195 60 195 105
Polygon -16777216 true false 238 112 252 141 219 141 218 112
Circle -16777216 true false 234 174 42
Rectangle -7500403 true true 181 185 214 194
Circle -16777216 true false 144 174 42
Circle -16777216 true false 24 174 42
Circle -7500403 false true 24 174 42
Circle -7500403 false true 144 174 42
Circle -7500403 false true 234 174 42

turtle
true
0
Polygon -10899396 true false 215 204 240 233 246 254 228 266 215 252 193 210
Polygon -10899396 true false 195 90 225 75 245 75 260 89 269 108 261 124 240 105 225 105 210 105
Polygon -10899396 true false 105 90 75 75 55 75 40 89 31 108 39 124 60 105 75 105 90 105
Polygon -10899396 true false 132 85 134 64 107 51 108 17 150 2 192 18 192 52 169 65 172 87
Polygon -10899396 true false 85 204 60 233 54 254 72 266 85 252 107 210
Polygon -7500403 true true 119 75 179 75 209 101 224 135 220 225 175 261 128 261 81 224 74 135 88 99

wheel
false
0
Circle -7500403 true true 3 3 294
Circle -16777216 true false 30 30 240
Line -7500403 true 150 285 150 15
Line -7500403 true 15 150 285 150
Circle -7500403 true true 120 120 60
Line -7500403 true 216 40 79 269
Line -7500403 true 40 84 269 221
Line -7500403 true 40 216 269 79
Line -7500403 true 84 40 221 269

x
false
0
Polygon -7500403 true true 270 75 225 30 30 225 75 270
Polygon -7500403 true true 30 75 75 30 270 225 225 270

@#$#@#$#@
NetLogo 3.1.2
@#$#@#$#@
@#$#@#$#@
@#$#@#$#@
@#$#@#$#@
