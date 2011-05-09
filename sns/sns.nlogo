;Nodes represent the retailers, distributers, and factory.
breed [nodes node]
;breed [links link]

;nodes have color of either: green, red, blue
;distances: [ [agent distance][...]] used to calculate min distance to every other agent
nodes-own [distances cluster-coefficient]

;components is a list of agents, each a member of a different component
globals [changed-distances? characteristic-path-length clustering-coefficient components max-distance size-max-component]

to ultralog
  while [count nodes < num-nodes] [
    layout-spring nodes links 0.2 2 1
   
    let partner find-preferential-partner ;apply Pi function: choose stochastically based on degree
    let new-node-who 0
    create-nodes 1 [
      set new-node-who who
      set shape "circle"
      set color generate-node-color
      setup-distances
      if (color = blue) [ ;battalion
        create-link-with partner [ ;first edge attaches with 
          set color gray
        ]
        if (random-float 1.0 < p) [ ;create second link
          let nodes-at-distance-two nodes with [get-distance-to myself = 2]
          if (any? nodes-at-distance-two) [
            create-link-with (one-of nodes-at-distance-two) [
              set color gray
            ]
          ]
        ]
      ]
      
      if (color = red) [; FSB- Forward Support Battalion
        create-link-with partner [ ;first edge attaches with 
          set color gray
        ]
        let nodes-at-distance-lte-3 nodes with [self != myself and get-distance-to myself <= 3 and (not link-neighbor? myself)]
        ifelse (count nodes-at-distance-lte-3 >= 2) [
          create-links-with (n-of 2 nodes-at-distance-lte-3) [
            set color gray
          ]
        ][
          if (any? nodes-at-distance-lte-3) [ ;there is only 1
            create-links-with nodes-at-distance-lte-3 [
              set color gray
            ]
          ]
        ] 
      ]
      
      if (color = green)[
        let list-of-neighbors fput self []
        repeat 5 [
          let n find-preferential-partner-not-in list-of-neighbors
          if (n != nobody) [
            create-link-with n [
              set color gray
            ]
          ]
          set list-of-neighbors fput n list-of-neighbors
        ]
      ]
    ]

    ;update distances. 
    update-distances-new-node (turtle new-node-who)
    calculate-distances 
  ]
end

to setup-attachment [find-fun]
  while [count nodes < num-nodes] [
    layout-spring nodes links 0.2 2 1
  
    create-nodes 1 [
      set shape "circle"
      set color generate-node-color
      let num-attachments 1
      setup-distances
      if (color = blue) [
        set num-attachments num-attachments + ifelse-value (random-float 1.0 < p) [1][0]
      ]
      if (color = red) [
        set num-attachments 3
      ]
      if (color = green) [
        set num-attachments 5
      ]

      let list-of-neighbors fput self []
      repeat num-attachments [
        let n run-result (word find-fun " list-of-neighbors")
        if (n != nobody) [
          create-link-with n [
            set color gray
          ]
        ]
        set list-of-neighbors fput n list-of-neighbors
      ]
    ]      
  ]
end

to-report find-preferential-partner
  let total random-float sum  [count my-links] of nodes
  let partner nobody
  ;; reporter procedures always run without interruption,
  ;; so we don't need "without-interruption" in order to
  ;; make the turtles go one at a time
  ask nodes
  [
    let nc (count my-links)
    if partner = nobody
    [
      ifelse nc > total
        [ set partner self ]
        [ set total total - nc ]
    ]
  ]
  if (partner = nobody) [ ;this only happens if there are no edges in the graph (initial condition) so just pick one at random
    report one-of nodes
  ]
  report partner
end

to-report find-preferential-partner-not-in [list-of-agents]
  let candidates nodes with [not member? self list-of-agents]
  let total random-float sum [count my-links] of candidates 
  let partner nobody
  ;; reporter procedures always run without interruption,
  ;; so we don't need "without-interruption" in order to
  ;; make the turtles go one at a time
  ask candidates
  [
    let nc (count my-links)
    if partner = nobody
    [
      ifelse nc > total
        [ set partner self ]
        [ set total total - nc ]
    ]
  ]
  report partner
end

to-report find-random-partner-not-in [list-of-agents]
  let candidates nodes with [not member? self list-of-agents]
  report one-of candidates
end


to setup-initial-nodes
  let who-of-first 0
   create-nodes 1 [
     set who-of-first who
     set color generate-node-color
     setup-distances
     set shape "circle"
   ]
   create-nodes 1 [
     set color generate-node-color
     setup-distances
     set shape "circle"
     create-link-with (turtle who-of-first) [
       set color gray
     ]
   ] 
end

;Reports node color based on ratios specified by user
to-report generate-node-color
  let s ratio-of-red + ratio-of-blue + ratio-of-green
  let r random s
  if (r < ratio-of-red) [
    report red
  ]
  if (r < ratio-of-red + ratio-of-blue) [
    report blue
  ]
  report green
end

to preferential-attachment
  setup-attachment "find-preferential-partner-not-in"
end

to random-attachment
  setup-attachment "find-random-partner-not-in"
end

to setup
  ;; (for this model to work with NetLogo's new plotting features,
  ;; __clear-all-and-reset-ticks should be replaced with clear-all at
  ;; the beginning of your setup procedure and reset-ticks at the end
  ;; of the procedure.)
  __clear-all-and-reset-ticks
  ask patches [set pcolor white]
  set components []
  setup-initial-nodes
  run model
  layout-spring nodes links 0.2 5 1
  
  set-current-plot "Edge Distribution"
;  histogram-from nodes [count my-links]
  calculate-distances
  set-components
  let max-component get-largest-component
  set characteristic-path-length get-characteristic-path-length max-component
  set clustering-coefficient get-clustering-coefficient max-component
  set max-distance get-max-distance max-component
  set size-max-component get-size-of-largest-component
end

to run-test [choose]
  setup
  repeat (.8 * num-nodes) [
    set-current-plot "Characteristic Path Length"
    plot characteristic-path-length
    set-current-plot "Largest Component"
    plot size-max-component
    set-current-plot "Max Distance"
    plot max-distance
    
    if (choose = "random") [
      ask one-of nodes [die]
    ]
    if (choose = "targeted") [
      ask (max-one-of nodes [count my-links]) [die]
    ]
    
    ask nodes [
      setup-distances
    ]
    set num-nodes num-nodes - 1
    calculate-distances
    set-components
    let max-component get-largest-component
    set characteristic-path-length get-characteristic-path-length max-component
    set clustering-coefficient get-clustering-coefficient max-component
    set max-distance get-max-distance max-component
    set size-max-component get-size-of-largest-component
    repeat 5 [layout-spring nodes links 0.2 5 1]
  ]
end

to layout
  layout-spring nodes links 0.2 2 1
end

;----------
;global utility/graph functions

;get all nodes to calculate their distances matrix
;implement dijikstra's algorithm
to calculate-distances
  set changed-distances? true;
;  ask nodes [setup-distances] ;probably don't need to do this as we are only adding nodes....
  while [changed-distances?] [
    set changed-distances? false
    ask nodes [update-distances]
  ]
end

;Updates everyone's distances by adding a distance to new-node which is 1 + their distance to
; a randomly chosen neighbor of new-node. This might be too big so we still need to calculate-distances after this.
;This function speeds up the later call to calculate-distances (a bit).
;Assumes connected graph.
to update-distances-new-node [new-node]
    let new-node-neighbor [one-of link-neighbors] of new-node 
    ask nodes with [self != new-node][
      set-distance-to new-node (get-distance-to new-node-neighbor + 1)
    ]
end

;Report a list of all pairs (n choose 2) from the-list. Does not contain duplicates (permutations)
to-report get-all-pairs [the-list]
  let result []
  foreach the-list [
    let a ?
    foreach the-list [
      if (? != a) [
        set result fput list a ? result
      ]
    ]
  ]
  report result
end

to-report get-all-pairs-agents [agent-set]
  report get-all-pairs [self] of agent-set 
end

;Calculates the characteristic path length, which is the smallest number of links it takes to connect one node to another, averaged 
;over all pairs of nodes in the network.
;Do this by first having each node calculate its minimun distance to every other node and then we average over all pairs
to-report get-characteristic-path-length [component]
  let pairs get-all-pairs-agents (nodes with [get-component = component])
  if (length pairs = 0) [report 0]
  let sum-distances 0
  foreach pairs [
    ask (first ?) [ ;since Im assuming bidirections it does not matter which one I ask.
;      show get-distance-to (item 1 ?)
      set sum-distances sum-distances + get-distance-to (item 1 ?)
    ]
  ]
  report sum-distances / length pairs
end

to-report get-max-distance [component]
  report max [max map [item 1 ?] distances] of (nodes with [get-component = component]) 
end

to-report get-clustering-coefficient [component]
  ask nodes with [get-component = component] [
    set cluster-coefficient get-node-clustering-coefficient
  ]
  report mean [cluster-coefficient] of (nodes with [get-component = component]) 
end

to-report closest-xy [x y agent-set]  ; Return closest agent to x, y
  report min-one-of agent-set [distancexy-nowrap x y]
end

to set-components
  ask nodes [
    without-interruption [
      if (get-component = nobody) [
        set components fput self components
      ]
    ]
  ]
end

;reports the agent that is the representative for the largest component
to-report get-largest-component
  let whole-component reduce [ifelse-value (count ?1 > count ?2)[?1][?2]] (map [nodes with [get-component = ?]] components)
  report [get-component] of (one-of whole-component) 
end

to-report get-size-of-largest-component
  report max map [count ?] (map [nodes with [get-component = ?]] components)
end

;-------------------------------------------------------------
;node functions

;-------------
;graph calculations
;
; The second measure is the clustering coefficient. This measures the amount of cliquishness 
; of the network, that is, the fraction of neighbouring nodes that are also connected to one another. For example, in an all-to-all 
; connected network, the clustering coefficient is one.
;
;Calculate distances to every other node using Dijistra's max-flow algorithm
;

;set distance to myself to be 0
to setup-distances
  set distances fput (list self 0) []
end

to-report assoc [element lst]
  foreach lst [
    if (first ? = element) [
      report ?]
  ]
  report []
end

to-report get-component
  foreach (components) [
    if (get-distance-to ? > -1) [
      report ?
    ]
  ]
  report nobody
end

to-report get-distance-to [the-other]
  foreach distances [
    if (first ? = the-other) [
      report (item 1 ?)
      ]
  ]
  report -1 
end

to set-distance-to [the-other num]
  let already-there false
  set changed-distances? true
 
  foreach distances [
    if (first ? = the-other) [
      without-interruption [ ;don't want someone else reading after delete but before addition
        set distances remove ? distances
        set distances fput (list the-other num) distances
        set already-there true
      ]
    ]
  ]
  if (not already-there) [
    set distances fput (list the-other num) distances
  ]
end

;node updates its "distances" variable by adding 1 to neighbors' distances, if that is smaller.
to update-distances
  let n-distances [distances] of link-neighbors 
  set n-distances ifelse-value (empty? n-distances) [ [] ] [reduce [sentence ?1 ?2] n-distances]
  
;  show n-distances
  foreach n-distances [
    let mine get-distance-to (first ?)
    if (mine = -1 or item 1 ? + 1 < mine) [
      set-distance-to (first ?) (item 1 ? + 1)
    ]
  ]
end

;report my clustering coefficient: the fraction of my neighbors who are also connected to each other
;requires: 'distances' matrix must already be calculated
;assume bidirection, but this should probably change
to-report get-node-clustering-coefficient
  let neighbors-list remove-duplicates [self] of link-neighbors 
  let pairs get-all-pairs neighbors-list
  let num-pairs-connected 0
  foreach pairs [
    ask (first ?) [
      if (get-distance-to (item 1 ?) = 1) [
        set num-pairs-connected num-pairs-connected + 1
      ]
    ]
  ]
  if (length pairs = 0) [report 1.0]
  report num-pairs-connected / length pairs
end
@#$#@#$#@
GRAPHICS-WINDOW
243
10
673
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
1
1
1
-17
17
-17
17
0
0
1
ticks
30.0

BUTTON
7
16
79
49
NIL
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

CHOOSER
7
53
230
98
model
model
"ultralog" "preferential-attachment" "random-attachment"
0

SLIDER
10
153
182
186
ratio-of-blue
ratio-of-blue
1
50
25
1
1
NIL
HORIZONTAL

SLIDER
10
188
182
221
ratio-of-red
ratio-of-red
1
50
4
1
1
NIL
HORIZONTAL

SLIDER
10
224
182
257
ratio-of-green
ratio-of-green
1
50
1
1
1
NIL
HORIZONTAL

SLIDER
11
110
183
143
num-nodes
num-nodes
10
100
23
1
1
NIL
HORIZONTAL

SLIDER
10
269
182
302
p
p
0
1
0.5
0.01
1
NIL
HORIZONTAL

MONITOR
186
99
243
152
nodes
count nodes
0
1
13

BUTTON
82
16
159
49
NIL
layout
T
1
T
OBSERVER
NIL
NIL
NIL
NIL
1

PLOT
11
308
211
458
Edge Distribution
# edges
NIL
1.0
10.0
0.0
10.0
true
false
"" ""
PENS
"default" 1.0 1 -16777216 true "" ""

MONITOR
678
116
881
169
Characteristic Path Length
characteristic-path-length
2
1
13

MONITOR
676
176
848
229
Clustering Coefficient
clustering-coefficient
2
1
13

MONITOR
679
239
834
292
Largest Component
get-size-of-largest-component
0
1
13

PLOT
289
462
576
632
Characteristic Path Length
Nodes Removed
CPL
0.0
10.0
0.0
1.0
true
false
"" ""
PENS
"default" 1.0 0 -16777216 true "" ""

BUTTON
677
25
827
58
Random Attacks
run-test \"random\"
NIL
1
T
OBSERVER
NIL
NIL
NIL
NIL
1

PLOT
8
462
280
632
Largest Component
Nodes Removed
Size
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
582
462
838
632
Max Distance
Nodes Removed
Distance
0.0
10.0
0.0
10.0
true
false
"" ""
PENS
"default" 1.0 0 -16777216 true "" ""

MONITOR
681
300
825
353
Max Distance in LC
max-distance
0
1
13

BUTTON
677
74
832
107
Targeted Attacks
run-test \"targeted\"
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
# Supply Network Survivability  
## CREDITS

Jose M Vidal  

## WHAT IS IT?

This model shows how various supply-chain network topologies fare under attack. The original model was developed to study the military's supply chain vulnerability to terrorist or military attacks, part of the Ultralog project.

 * Hari Prasad Thadakamalla, Usha Nandini Raghavan, Soundar Kumara, and Reka Albert. [Survivability of Multiagent-Based Supply Networks: A Topological Perspectiv](http://jmvidal.cse.sc.edu/lib/thadakamalla04a.html). _IEEE Intelligent Systems,_ 19(5):24--31, 2004.

## CHANGES

20100623
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
@#$#@#$#@
@#$#@#$#@
default
0.0
-0.2 0 0.0 1.0
0.0 1 1.0 0.0
0.2 0 0.0 1.0
link direction
true
0
Line -7500403 true 150 150 90 180
Line -7500403 true 150 150 210 180

@#$#@#$#@
0
@#$#@#$#@
