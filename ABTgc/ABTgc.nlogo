extensions [table]

globals [all-colors]

turtles-own [
  message-queue  ;; list of [message-type message-value]
  value          ;; number from 0...possible-colors
  naybors        ;; list of turtles
  local-view     ;; map from int (who-number) to value
  no-goods       ;; list of lists of pairs
]


to setup
  clear-all
  ;; YOU find a better command to do this
  set all-colors filter [? < possible-colors] [0 1 2 3 4 5 6 7 8 9 10 11 12 13 14 15]
  set-default-shape turtles "circle"
  create-turtles node-count
  ask turtles [setxy random-xcor random-ycor]
  
  ;; We give the turtles random colors and then try to create the desired number of links without
  ;;   violating constraints. This will ensure a solvable graph. If we get stuck before we reach
  ;;   the desired number of links, a message is printed

  ask turtles [get-random]
  let lonelies sort turtles
  repeat (edges-to-nodes * node-count) [
    ifelse empty? lonelies
    [
      let needing-friends turtles with [((count my-links) < (node-count - 1)) and (not empty? link-options)]
      ifelse (count needing-friends > 0)
      [
        let nf one-of needing-friends
        ask nf [link-up]
      ]
      [
        show "CANNOT FORM ANYMORE LINKS"
      ]
    ]
    [
      let lonely one-of lonelies
      ask lonely [link-up]
      set lonelies filter [not (? = lonely or [link-neighbor? lonely] of ?)] lonelies 
    ]
  ]
end

to layout
  layout-spring turtles links 1 12 3
end

to setup-abt
  ;; Gives the turtles random values, sets up relevant data structures and sends out initial "ok" messages
  ask turtles [
    get-random
    set message-queue []
    set local-view table:make
    set naybors sort link-neighbors
    set no-goods []
    foreach naybors [
      let w ([who] of ?)
      set no-goods sentence no-goods map [normalize-nogood list (list who ?) (list w ?)] all-colors
    ]
  ]
  ask turtles [send-out-new-value]
end

to go
  let important-turtles turtles with [not empty? message-queue]
  ifelse (count important-turtles > 0) [
    ask important-turtles [handle-message]
  ][
    ifelse (bad-links = 0)[
      show "SOLUTION FOUND"
    ][
      show "NO MORE MESSAGES"
    ]
    stop
  ]
end


;; ==Turtle Procedures==


;; For setup

to-report link-options
  ;; All of the turtles this turtle could link with (i.e., not the same color and not already linked)
  report sort turtles with [(not link-neighbor? myself) and (not (([value] of myself) = ([value] of self)))]
end

to link-up
  ;; Creates a link between this turtle and another that is not the same color
  let li link-options
  create-link-with one-of li
end

to get-random
  ;; sets value and color to random
  set value random possible-colors
  be-honest
end

;; For running

to send-out-new-value
  ;; Sends "ok"s to neighbors
  let my-message list "ok" (list who value)
  foreach naybors [
    ask ? [set message-queue (lput my-message message-queue)]
  ]
end

to handle-message
  if not empty? message-queue [
    ;; THIS IS A RIDICULOUS BUNCH OF BOILERPLATE
    let message first message-queue
    set message-queue but-first message-queue
    let message-type first message
    let message-value last message
    ifelse message-type = "ok" [
      let someone first message-value
      let val last message-value
      handle-ok someone val
    ][
      ifelse message-type = "nogood" [
        handle-nogood message-value
      ][
        handle-add-neighbor message-value
      ]
    ]
  ]
end

to handle-ok [someone val]
  table:put local-view someone val
  check-local-view
end

to handle-nogood [nogood]
  if(not member? nogood no-goods) [
    set no-goods fput nogood no-goods
    ;; for each new neighbor
    foreach (filter [not member? (turtle first ?) naybors] nogood) [
      let new-naybor turtle (first ?)
      set naybors fput new-naybor naybors
      table:put local-view (first ?) (last ?)
      let message (list "new-neighbor" who)
      ask new-naybor [
        set message-queue lput message message-queue
      ]
    ]
    check-local-view
  ]
end

to-report can-i-be? [val]
  ;; Turtle checks if a particular value is consistent with his local-view
  table:put local-view who val
  foreach no-goods [
    if (violates? local-view ?) [
      table:remove local-view who
      report false
    ]
  ]
  table:remove local-view who
  report true
end

to-report violates? [assignments constraint]
  foreach constraint [
    if not (table:has-key? assignments (first ?) and (table:get assignments first ?) = (last ?)) [report false]
  ]
  report true
end

to handle-add-neighbor [someone]
  if (not (member? (turtle someone) naybors)) [
    set naybors fput turtle someone naybors
      
    ;; Book doesn't say anything about sending an "ok?" in the pseudocode, but seems to imply that in the text.
    ;; Anyways, it makes sense I think
  
    let message (list "ok" (list who value))
    ask turtle someone [
      set message-queue lput message message-queue
    ]
  ]
end

to check-local-view
  if not can-i-be? value [
    let try-these filter [not (? = value)] all-colors
    let can-be-something-else false
    while [not empty? try-these] [
      let try-this first try-these
      set try-these but-first try-these
      
      if can-i-be? try-this [
        set try-these [] ;; break loop
        set value try-this
        be-honest
        set can-be-something-else true
        send-out-new-value
      ]
    ]
    if not can-be-something-else [backtrack]
  ]
end

to backtrack
  let no-good normalize-nogood find-new-nogood
  ifelse(not member? no-good no-goods) [
    if ([] = no-good) [
      show "EMPTY NO-GOOD FOUND - NO SOLUTION"
      stop
    ]
    set no-goods fput no-good no-goods
    ask (turtle first first no-good) [
      set message-queue lput (list "nogood" no-good) message-queue
    ]
  ] [ show "SHIT OLD NOGOOD"]
  ;;check-local-view ;; doing this would get us into an infinite loop, but the pseudo-code in the book has it
end

;; For finding nogoods, I used the simple local-view option, but also experimented with hyperresolution, which didn't go so well.

to-report find-new-nogood
  report find-new-nogood-hyper
end

to-report find-new-nogood-hyper
  ;;hyperresolution
  let my-who who
  report remove-duplicates filter [not ((first ?) = my-who)] (reduce [sentence ?1 ?2] no-goods)
end

to-report find-new-nogood-simple
  report table:to-list local-view
end

to be-honest 
  ;; sets visible color to match value
  set color item value base-colors
end

to-report normalize-nogood [nogood]
  ;; Sorts the terms so that we can easily identify duplicates
  report sort-by [(first ?1) < (first ?2)] nogood
end


;; Global Reports, mostly for monitors


to-report bad-links
  report count links with [is-bad-link]
end

to-report is-bad-link
  let two-vals [value] of both-ends
  report first two-vals = last two-vals
end

to-report unhandled-messages
  report sum [length message-queue] of turtles
end

to-report average-nogoods
  report (sum [length no-goods] of turtles) / (count turtles)
end

to-report average-local-view
  report (sum [(length naybors) / ((count turtles) - 1)] of turtles) / (count turtles)
end

to-report local-view-knowledge
  report (sum [(table:length local-view) / (length naybors)] of turtles) / (count turtles)
end
@#$#@#$#@
GRAPHICS-WINDOW
308
10
747
470
16
16
13.0
1
10
1
1
1
0
0
0
1
-16
16
-16
16
0
0
1
ticks

BUTTON
1
116
75
149
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

SLIDER
1
10
193
43
node-count
node-count
2
50
14
1
1
nodes
HORIZONTAL

BUTTON
1
150
80
183
Layout
layout
T
1
T
OBSERVER
NIL
NIL
NIL
NIL

SLIDER
1
44
186
77
edges-to-nodes
edges-to-nodes
1
7
3.19
.01
1
NIL
HORIZONTAL

SLIDER
1
78
214
111
possible-colors
possible-colors
2
10
6
1
1
colors
HORIZONTAL

BUTTON
1
184
106
217
Setup ABT
setup-abt
NIL
1
T
OBSERVER
NIL
NIL
NIL
NIL

BUTTON
1
218
64
251
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

MONITOR
160
125
305
170
Violated Constraints
bad-links
0
1
11

MONITOR
150
225
305
270
Unhandled Messages
unhandled-messages
0
1
11

MONITOR
175
275
306
320
Average Nogoods
average-nogoods
3
1
11

MONITOR
110
175
306
220
Average Size of Local View
average-local-view
3
1
11

MONITOR
150
325
306
370
Local View Knowledge
local-view-knowledge
3
1
11

MONITOR
180
375
307
420
Busy Turtles
count turtles with [not (empty? message-queue)]
0
1
11

TEXTBOX
10
262
118
537
Size of local view is size of neighbors compared to the whole graph.\n\nLocal view knowledge is the percentage of the neighbors for which the turtles have an associated value.\n\nBusy turtles are turtles with messages in their queue.
12
0.0
1

@#$#@#$#@
NODE VALUES
-----------
I used simple integers to represent node values, corresponding to the indexes in the built-in base-colors array. So in a 3-color graph, the values will be the numbers [0,1,2], and they will refer to the corresponding colors in the base-colors array.

PRIORITIES
----------
Priorities are implemented by who number, with lower numbers having lower priorities.

SOLVABLE GRAPHS
---------------
The program as written always generates solvable graphs. It does this by
1) Generating the specified number of turtles
2) Assigning random colors to each turtle (from the specified number of colors)
3) Generating legal links until either the links-to-nodes ratio has been reached, or no legal links can be generated. In the latter case, a warning is written to the console.

GENERATING NO-GOODS
-------------------
I implemented two no-good generators. The first, which is used by default, is the naive generator which uses the local view. I also tried to create a hyperresolution generator, but my simple implementation would just concatenate all of the previously generated nogoods (filtering out the elements referring to self), which certainly isn't sufficient to solve the problem, and may also be worse than the local-view version (it also generates tautological nogoods most of the time, i.e. NOT(TURTLE1(A) ^ TURTLE1(B) ^ ...)). If I had time, I think I would enjoy trying to do hyperresolution properly.

IMPLEMENTATION NOTES
--------------------
1) I used the table extension for the local-view datastructure, with the who-numbers for keys.
2) A single nogood is represented as a list of pairs, where [[1 3] [2 4]] means that "turtle 1 having value 3 and turtle 2 having value 4" is no-good. 
3) Despite using who-numbers for local-view and no-goods, I implemented "naybors" as a list of turtles. Could've gone either way I guess.


CREDITS
-------

Title: Asynchronous Backtracking
Author: Gary Fredericks
Description: 

Generates and solves graph coloring problems using ABT.

---------
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

sheep
false
0
Rectangle -7500403 true true 151 225 180 285
Rectangle -7500403 true true 47 225 75 285
Rectangle -7500403 true true 15 75 210 225
Circle -7500403 true true 135 75 150
Circle -16777216 true false 165 76 116

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
NetLogo 4.1
@#$#@#$#@
@#$#@#$#@
@#$#@#$#@
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
1
@#$#@#$#@
