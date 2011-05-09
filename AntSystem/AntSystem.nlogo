globals [ best-tour 
          best-tour-cost  
          node-diameter ]

breed [ nodes node ]
breed [ ants ant ]

links-own [ node-a
            node-b
            cost 
            pheromone ]
            
ants-own [ tour
           tour-cost ]
        
;;;;;;;;;;;;;;;::::::;;;;;;;;;
;;; Setup/Reset Procedures ;;;
;;;;;;;;;;;;;;;;;;;;;::::::;;;

to setup
  ;; (for this model to work with NetLogo's new plotting features,
  ;; __clear-all-and-reset-ticks should be replaced with clear-all at
  ;; the beginning of your setup procedure and reset-ticks at the end
  ;; of the procedure.)
  __clear-all-and-reset-ticks
  set node-diameter 1.5
  set-default-shape nodes "circle"
  
  setup-nodes
  setup-links
  setup-ants

  set best-tour get-random-path
  set best-tour-cost get-tour-length best-tour
  update-best-tour
end

to setup-nodes
  ;; Create x and y ranges that will not allow a node to be created
  ;; that goes outside of the link of the world.
  let x-range n-values (max-pxcor - node-diameter / 2) [? + 1]
  let y-range n-values (max-pycor - node-diameter / 2) [? + 1]
  
  create-nodes num-of-nodes [
    setxy one-of x-range one-of y-range
    set color yellow
    set size node-diameter
  ]
end

to setup-links
  let remaining-nodes [self] of nodes 
  while [not empty? remaining-nodes] [
    let a first remaining-nodes
    set remaining-nodes but-first remaining-nodes      
    ask a [
      without-interruption [
        foreach remaining-nodes [
            create-link-with ? [
            hide-link
            set color red
            set thickness 0.3
            set node-a a
            set node-b ?
            set cost ceiling calculate-distance a ?
            set pheromone random-float 0.1
          ]  
        ]
      ]
    ]
  ]
end

to setup-ants
  create-ants num-of-ants [
    hide-turtle
    set tour []
    set tour-cost 0
  ]
end

to reset
  ;; Reset the ant colony and the pheromone in the graph
  ask ants [die]
  ask links [die]
  setup-links
  setup-ants
  
  ;; Update the best tour
  set best-tour get-random-path
  set best-tour-cost get-tour-length best-tour
  update-best-tour  
  
  ;; Clear all of the plots in the model and reset the number of ticks
  clear-all-plots
end

;;;;;;;;;;;;;;;;;;;;;;
;;; Main Procedure ;;;
;;;;;;;;;;;;;;;;;;;;;;

to go
;  no-display
  ask ants [
    set tour get-as-path
    set tour-cost get-tour-length tour 

    if tour-cost < best-tour-cost [
      set best-tour tour
      set best-tour-cost tour-cost
      update-best-tour
    ]
  ]

  update-pheromone
      
  do-plots
  display
end

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Path Finding Procedures            ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

to-report get-random-path
  let origin one-of nodes
  report fput origin lput origin [self] of nodes with [self != origin] 
end

to-report get-as-path
  let origin one-of nodes
  let new-tour (list origin)
  let remaining-nodes [self] of nodes with [self != origin]
  let current-node origin
  
  ;; Create the new path for the ant
  while [not empty? remaining-nodes] [
    let next-node choose-next-node current-node remaining-nodes
    set new-tour lput next-node new-tour
    set remaining-nodes remove next-node remaining-nodes
    set current-node next-node
  ]
  
  ;; Move the ant back to the origin
  set new-tour lput origin new-tour
  
  report new-tour
end

to-report choose-next-node [current-node remaining-nodes]
  let probabilities calculate-probabilities current-node remaining-nodes
  let rand-num random-float 1
  report last first filter [first ? >= rand-num] probabilities
end

to-report calculate-probabilities [current-node remaining-nodes]
  let transition-probabilities []
  let denominator 0
  foreach remaining-nodes [
    ask current-node [ 
      let next-link link-with ? 
      let transition-probability ([pheromone] of next-link ^ alpha) * ((1 / [cost] of next-link) ^ beta)
      set transition-probabilities lput (list transition-probability ?) transition-probabilities
      set denominator (denominator + transition-probability)
    ]
  ]

  let probabilities []
  foreach transition-probabilities [
    let transition-probability first ?
    let destination-node last ?
    set probabilities lput (list (transition-probability / denominator) destination-node) probabilities
  ]

  ;; Sort the probabilities
  set probabilities sort-by [first ?1 < first ?2] probabilities
  
  ;; Normalize the probabilities
  let normalized-probabilities []
  let total 0
  foreach probabilities [
    set total (total + first ?)
    set normalized-probabilities lput (list total last ?) normalized-probabilities
  ]
  
  report normalized-probabilities  
end

to update-pheromone
  ;; Evaporate the pheromone in the graph
  ask links [
    set pheromone (pheromone * (1 - rho))
  ]
  
  ;; Add pheromone to the paths found by the ants 
  ask ants [
    without-interruption [
      let pheromone-increment (100 / tour-cost)
      foreach get-tour-links tour [
        ask ? [ set pheromone (pheromone + pheromone-increment) ]    
      ]
    ]
  ]
end

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Plotting/GUI Procedures ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

to update-best-tour
  ask links [ hide-link ]
  foreach get-tour-links best-tour [
    ask ? [ show-link ]
  ]
end

to do-plots
  set-current-plot "Best Tour Cost"
  plot best-tour-cost
  
  set-current-plot "Tour Cost Distribution"
  set-plot-pen-interval 10
  histogram  [tour-cost] of ants
end

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Miscellaneous Procedures ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

to-report get-tour-links [tour-nodes]
  let xs but-last tour-nodes
  let ys but-first tour-nodes
  let tour-links []
  (foreach xs ys [ 
    ask ?1 [ set tour-links lput link-with ?2 tour-links] 
  ])
  report tour-links
end

to-report get-tour-length [tour-nodes]
  report reduce [?1 + ?2] map [[cost] of ?] get-tour-links tour-nodes
end

to-report calculate-distance [a b]
  let diff-x [xcor] of a - [xcor] of b
  let diff-y [ycor] of a - [ycor] of b
  report sqrt (diff-x ^ 2 + diff-y ^ 2)
end
@#$#@#$#@
GRAPHICS-WINDOW
187
10
623
467
-1
-1
6.0
1
10
1
1
1
0
0
0
1
0
70
0
70
0
0
1
ticks
30.0

BUTTON
10
10
65
43
Setup
setup
NIL
1
T
OBSERVER
NIL
NIL
NIL
NIL
1

BUTTON
125
10
180
43
Go
go
T
1
T
OBSERVER
NIL
NIL
NIL
NIL
1

MONITOR
706
418
771
463
Best Tour
best-tour-cost
3
1
11

MONITOR
631
417
698
462
Ticks
ticks
3
1
11

PLOT
630
10
945
229
Best Tour Cost
Time
Cost
0.0
10.0
0.0
10.0
true
false
"" ""
PENS
"default" 1.0 0 -16777216 true "" ""

PLOT
631
238
945
410
Tour Cost Distribution
Tour Cost
Number of Ants
0.0
1000.0
0.0
10.0
true
false
"" ""
PENS
"default" 1.0 1 -16777216 true "" ""

SLIDER
10
127
180
160
alpha
alpha
0
20
1
1
1
NIL
HORIZONTAL

SLIDER
10
166
180
199
beta
beta
0
20
5
1
1
NIL
HORIZONTAL

SLIDER
10
205
180
238
rho
rho
0
0.99
0.5
0.01
1
NIL
HORIZONTAL

SLIDER
10
50
180
83
num-of-nodes
num-of-nodes
0
50
46
1
1
NIL
HORIZONTAL

SLIDER
10
89
180
122
num-of-ants
num-of-ants
0
100
59
1
1
NIL
HORIZONTAL

BUTTON
68
10
123
43
Reset
reset
NIL
1
T
OBSERVER
NIL
NIL
NIL
NIL
1

@#$#@#$#@
# Ant System  
## CREDITS

Chistopher Roach and Benito Mendoza  


## WHAT IS IT?

This model is an implementation of the Ant System algorithm, as described in [here](http://citeseer.ist.psu.edu/dorigo96ant.html), that is being used to solve the Traveling Salesman Problem.

## HOW IT WORKS

The Ant System algorithm can be used to find the shortest path in a graph by employing the same decentralized mechanism exploited by ant colonies foraging for food.  In the model, each agent (i.e., ant) constructs a tour in the graph by making a choice at each node as to which node will be visited next according to a probability associated with each node.  The probability of an ant choosing a specific node at any time is determined by the amount of pheromone and the cost (i.e., the distance from the current node i to the next node j, where node j has not yet been visited) associated with each edge.  

The attributes in this model that can be adjusted to change the behavior of the algorithm are alpha, beta, and rho.  The alpha and beta values are used to determine the transition probability discussed above, where the values are used to adjust the relative influence of each edge's pheromone trail and path cost on the ant's decision.  A rho value is also associated with the algorithm and is used as an evaporation rate which allows the algorithm to "forget" tours which have proven to be less valuable.

## HOW TO USE IT

Choose the number of nodes and ants that you wish to have in the simulation (for best results set the number of ants equal to the number of nodes in the graph).  Click the SETUP button to create a random graph, a new colony of ants, and draw an initial tour on the graph. Click the GO button to start the simulation.  The RESET button keeps the same graph that was generated by the SETUP operation, but it resets everything else in the algorithm (i.e., it destroys all ants and edges in the graph and clears all of the plots).  The RESET button allows the user to run several tests with the same graph for data gathering.

The alpha slider controls the propensity of the ants to exploit paths with high amounts of pheromone on them.  The beta slider controls how greedy the ants are, i.e., the ant's to edges with the lowest cost.  The delta slider controls the evaporation rate of the pheromone in the graph where the higher the delta, the faster the pheromone evaporates.

## THINGS TO NOTICE

In the model, two plots are given to show how the algorithm is performing.  The "Best Tour Cost" plot shows the cost of the best tour found so far over the life of the current run.  The "Tour Cost Distribution" plot is a histrogram that shows how the ants are distributed throughout the solution space.  In this plot, the vertical axis is the number of ants and the horizontal axis is tour cost, where each bar has a range of 10 units.

## THINGS TO TRY

According to [1], emperical evidence shows that the optimal settings for the algorithm are: alpha = 1, beta = 5, rho = 0.5.  Try adjusting each of these settings from the optimal and take notice of how they affect the performance of the algorithm.  Watch the "Best Tour Cost" plot to see if adjustments lead to a steadier march towards the best tour or perhaps they add up to a good initial search that settles quickly into a local optimum.  Study the "Tour Cost Distribution" plot to see if changes to the evaporation rate lead to stagnation?  Can you find more optimal settings than those that have been found through previous experimentation?

## CREDITS

This model is an implementation of the Ant System algorithm from [1]. 

When refering to this model in academic publications, please use: Roach, Christopher (2007).  NetLogo Ant System model. Computer Vision and Bio-inspired Computing Laboratory, Florida Institute of Technology, Melbourne, FL.

## REFERENCES

 * Dorigo, M., Maniezzo, V., and Colorni, A., The Ant System: Optimization by a colony of cooperating agents.  IEEE Transactions on Systems, Man, and Cybernetics Part B: Cybernetics, Vol. 26, No. 1. (1996), pp. 29-41. http://citeseer.ist.psu.edu/dorigo96ant.html

 * http://www.tsp.gatech.edu/

## CHANGES 

20100428

@#$#@#$#@
default
true
0
Polygon -7500403 true true 150 5 40 250 150 205 260 250

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

link
true
0
Line -7500403 true 150 0 150 300

link direction
true
0
Line -7500403 true 150 150 30 225
Line -7500403 true 150 150 270 225

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
NetLogo 5.0beta2
@#$#@#$#@
@#$#@#$#@
@#$#@#$#@
<experiments>
  <experiment name="experiment1" repetitions="1" runMetricsEveryStep="true">
    <setup>setup</setup>
    <go>go</go>
    <timeLimit steps="30"/>
    <metric>best-tour-cost</metric>
  </experiment>
</experiments>
@#$#@#$#@
@#$#@#$#@
default
0.0
-0.2 0 1.0 0.0
0.0 1 1.0 0.0
0.2 0 1.0 0.0
link direction
true
0
Line -7500403 true 150 150 90 180
Line -7500403 true 150 150 210 180

@#$#@#$#@
0
@#$#@#$#@
