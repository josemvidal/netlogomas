turtles-own [lost inside my-xcor my-ycor historyx historyy bad-move]  ; variable to store if a particular turtle is lost
globals [rand-list rand-list-nozero inside-move found]

; Called when user clicks on the setup button
to setup
  ca
  set rand-list [-1 -0.5 0 0.5 1]  ; initializes the list of random numbers (used for creating error)
  set rand-list-nozero [-0.5 -0.25 0.25 0.5]
  set inside-move [1 0 0 0 0 0 0 0 0 0]
  ; set-default-shape turtles "circle"
  create-n-turtles num-turtles
    
  ;***************************************************************************************
  ; Uncomment the following lines to see the shape they are conforming to
  ;***************************************************************************************
  ask patches
  [
    ; this series of if statements set up the vertical rectangles
    ;if ((pxcor <= -12) and (pxcor >= -15)) and ((pycor <= 15) and (pycor >= -15))
    ;[ set pcolor gray ]
    ;if ((pxcor >= 12) and (pxcor <= 15)) and ((pycor <= 15) and (pycor >= -15))
    ;[ set pcolor gray ]
    ;if ((pxcor <= -3) and (pxcor >= -6)) and ((pycor <= 15) and (pycor >= -15))
    ;[ set pcolor gray ]
    ;if ((pxcor >= 3) and (pxcor <= 6)) and ((pycor <= 15) and (pycor >= -15))
    ;[ set pcolor gray ]
    
    ; this series of if statements set up the horizontal rectangles
    ;if ((pycor <= -12) and (pycor >= -15)) and ((pxcor <= 15) and (pxcor >= -15))
    ;[ set pcolor gray ]
    ;if ((pycor >= 12) and (pycor <= 15)) and ((pxcor <= 15) and (pxcor >= -15))
    ;[ set pcolor gray ]
    ;if ((pycor <= -3) and (pycor >= -6)) and ((pxcor <= 15) and (pxcor >= -15))
    ;[ set pcolor gray ]
    ;if ((pycor >= 3) and (pycor <= 6)) and ((pxcor <= 15) and (pxcor >= -15))
    ;[ set pcolor gray ]
  ]
  
end



; Actually sets up the initial locations of the turtles
to create-n-turtles [n]
  let t1 0
  let t2 0
  let t3 0
  let the-neighbors 0
  

  crt n
  [
    setxy ((random world-width) + min-pxcor) ((random world-height) + min-pycor)
    set found (list 0)
    set heading random 360
    set lost true
    set inside false
    set color red
    set bad-move false
    set shape "square"
   
      set my-xcor (xcor + one-of rand-list-nozero)
      set my-ycor (ycor + one-of rand-list-nozero)
    
    set historyx (list my-xcor)
    set historyy (list my-ycor)
    
    ;************************************************************************************
    ; When turtle is first created, try to look for neighbors and triangulate position
    ;************************************************************************************
    set the-neighbors find-neighbors  ; get an agentset of 3 nearby turtles
    if(count the-neighbors = 3)  ; if we actually have 3 nearby neighbors...
    [
      let id-neighbors [who] of the-neighbors ; get the id's of the turtle neighbors
      set t1 (distance-nowrap (turtle (first id-neighbors)) + one-of rand-list)  ; get the distance (approx) to each neighbor
      set t1 (distance-nowrap (turtle (item 1 id-neighbors)) + one-of rand-list)
      set t1 (distance-nowrap (turtle (last id-neighbors)) + one-of rand-list)
      
      ;*********************************************************************************
      ; Do the following for all 3 neighbors:
      ; - Find the distance between the neighbors
      ; - Find the angle between 2 of the neighbors and the current agent
      ; - Find the x offset using sin(angle) to one neighbor
      ; - Find the y offset using sin(angle) to other neighbor
      
      set lost false
    ]
  ]
end


; Updates the locations of the turtles
to update
  if(count turtles < num-turtles)
  [
    create-n-turtles num-turtles - count turtles
  ]
  while [count turtles > num-turtles]
  [
    ask one-of turtles [die]
  ]
  ask turtles [without-interruption [move]]
end


; Moves the turtles (does all the actual work for turtle movement)
to move
  let g 0
  let cx 0
  let cy 0
  let the-neighbors 0
  let newx 0
  let newy 0
  
    
    ; now, to move one step in the gradient you just have to
    set the-neighbors find-neighbors
    set g get-gradient the-neighbors
    set my-xcor (xcor + first g + one-of rand-list-nozero)
    set my-ycor (ycor + last g + one-of rand-list-nozero)
    
    set historyx fput my-xcor historyx  ; add latest x location to front of the list
    if (length historyx > 8)
    [ set historyx remove (last historyx) historyx ]  ; remove oldest x location from the list
    set historyy fput my-ycor historyy  ; add latest y location to front of the list
    if (length historyy > 8)
    [ set historyy remove (last historyy) historyy ]  ; remove oldest y location from the list
    
    set newx (xcor + first g)
    set newy (ycor + last g)
    
    set bad-move true  ; bad-move is when an agent inside the shape tries to move outside the shape
    
    ; this series of if statements set up when the agent is in a vertical rectangle
    if ((my-xcor <= -12) and (my-xcor >= -15)) and ((my-ycor <= 15) and (my-ycor >= -15))
    [ 
      set inside true
      set bad-move false
    ]
    if ((my-xcor >= 12) and (my-xcor <= 15)) and ((my-ycor <= 15) and (my-ycor >= -15))
    [
      set inside true
      set bad-move false 
    ]
    if ((my-xcor <= -3) and (my-xcor >= -6)) and ((my-ycor <= 15) and (my-ycor >= -15))
    [
      set inside true
      set bad-move false
    ]
    if ((my-xcor >= 3) and (my-xcor <= 6)) and ((my-ycor <= 15) and (my-ycor >= -15))
    [
      set inside true
      set bad-move false
    ]
    
    ; this series of if statements set up when the agent is in a horizontal rectangle
    if ((my-ycor <= -12) and (my-ycor >= -15)) and ((my-xcor <= 15) and (my-xcor >= -15))
    [
      set inside true
      set bad-move false
    ]
    if ((my-ycor >= 12) and (my-ycor <= 15)) and ((my-xcor <= 15) and (my-xcor >= -15))
    [
      set inside true
      set bad-move false
    ]
    if ((my-ycor <= -3) and (my-ycor >= -6)) and ((my-xcor <= 15) and (my-xcor >= -15))
    [
      set inside true
      set bad-move false
    ]
    if ((my-ycor >= 3) and (my-ycor <= 6)) and ((my-xcor <= 15) and (my-xcor >= -15))
    [
      set inside true
      set bad-move false
    ]
    
    ifelse (inside = true)
    [ 
      ifelse (bad-move = false)
      [
        set color blue
        setxy newx newy
      ]
      [
        let rand-move one-of inside-move
        if(rand-move = 1)
        [ 
          let offset one-of [-.1 -0.5 0.5 .1]
          setxy (xcor + offset) (ycor + offset)
        ]
      ]
    ]
    [ setxy newx newy ]   
      
    ; setxy (xcor + first g) (ycor + last g)
    display
end


; Returns a list of 3 neighbors
to-report find-neighbors
  let near 0
  let tnear 0
  let rand 0
  let sensor 0
  let neighbor 0
  let t-here 0
  let th-id 0
  let isin 0
    ; declare some local variables
  set sensor 1  ; sets sensor radius initially to 1
  set rand 0;
  ; set rand random-one-of rand-list  ; get a random offset from the list
  set tnear turtles in-radius-nowrap (sensor + rand)  ; find all turtles within a certain radius
  while[(count tnear) < 5]  ; while the turtle can't find 3 neighbors
  [
    set sensor (sensor + 1)  ; increment sensor radius by 1
    ; set rand random-one-of rand-list  ; get a random offset from the list
    if(sensor >= 25) [report tnear]  ; if the sensor tries to go too far, just exit b/c it's not going to find more agents
    set tnear turtles in-radius-nowrap (sensor + rand)  ; find all turtles within a certain radius
  ]
  
  ;set t-here turtles-here
  ;set th-id values-from t-here [who]
  ;set isin false
  
  if ((count tnear) >= 5)  ; if turtle has 5 or more neighbors, prune to 3 neighbors (not including self)
  [ 
    set neighbor (n-of 3 tnear)
    set near [who] of neighbor
    
    ; foreach th-id [ if(member? ?1 near) [ set isin true ] ]
    
    ; while[isin = true]
    while[member? who near]
    [
      set neighbor (n-of 3 tnear)
      set near [who] of neighbor
      ;foreach th-id[ if(member? ? near) [ set isin true ] ]
    ]
  ]
  
  ; show near
  ; set lost false
  ; show neighbor
  
  report neighbor
end


;***********************************************************************************************
; If an agent is lost, uses data from nearby neighbors to triangulate it's own position
to find-turtle
  let near 0
  let tnear 0
  let rand 0
    ; declare some local variables
  set rand one-of rand-list  ; get a random offset from the list
  set tnear turtles in-radius-nowrap (6 + rand)  ; find all turtles within a certain radius
  set near [who] of tnear  ; get id's of these turtles
  set near remove who near  ; remove self from the list
  
  if (length near) < 3  ; if turtle doesn't find 3 neighbors, exit procedure and continue
  [ stop ]
  
  while [(length near) > 3]  ; if turtle has more than 3 neighbors, prune to 3 neighbors
  [ set near remove (one-of near) near ]
 ; set lost false
 ; show near
end
;**************************************************************************************************

; Displaces a large number of the turtles a random distance away from the original shape
to shake
  let x1 0
  let x2 0
  let y1 0
  let y2 0
  
  set x1 random max-pxcor
  set x2 (x1 - abs((random 5) + 10))
  set y1 random max-pycor
  set y2 (y1 - abs((random 5) + 10))
  
  ask turtles
  [
    if (xcor <= x1) and (xcor >= x2) and (ycor <= y1) and (ycor >= y2)
    [
      set lost true
      set inside false
      set color green
      setxy (xcor + (random 5) - 10) (ycor + (random 5) - 10)
    ]
  ]
end




;turtle function
; if other-node is a distance of more than d away from my perceived coordinates
;     (where I think I am: my-xcor my-ycor)
;    then report the unit vector which points towards other-node
; if other-node is a distance of less than d 
;    then report the unit vector which points away from other-node
; else report (0 0)
to-report get-unit-vector-towards [other-node]
   no-display ;turn display off to hide the jump back and forth.
   let d distance-nowrap other-node ;the real distance to other-node
   let tx xcor ;save my coordinates
   let ty ycor
   setxy my-xcor my-ycor ;temporarily place myself where I think I am.
   
   set heading ifelse-value (d < 1) [heading][towards-nowrap other-node]

   ifelse (distance other-node > d) [
      report (list dx dy)
   ][
      ifelse (distance other-node < d)[
         set heading heading + 180
         report (list dx dy)
      ][
         report (list 0 0)
      ]
  ]
  ; the following 2 lines are useless because they will never get executed
  setxy tx ty ;put me back where I really am
  display
end

;turtle function
;reports a unit vector that is the gradient (step 2 in the algorithm) for this 
; turtle given the-neighbors.
;the-neighbors is the list of neighbors
to-report get-gradient [the-neighbors]
   let vector reduce [(list ((first ?1) + (first ?2)) ((last ?1) + (last ?2)))]
                      [get-unit-vector-towards myself] of the-neighbors 
   let magnitude sqrt ((first vector) * (first vector) 
                  + (item 1 vector) * (item 1 vector))
   if (magnitude = 0) [ report (list 0 0) ]
   report (list (first vector / magnitude) (item 1 vector / magnitude))
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

SLIDER
8
60
180
93
num-turtles
num-turtles
0
2000
1006
1
1
NIL
HORIZONTAL

BUTTON
8
11
71
44
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

BUTTON
78
11
148
44
NIL
update
NIL
1
T
OBSERVER
NIL
NIL
NIL
NIL

BUTTON
155
11
225
44
NIL
update
T
1
T
OBSERVER
NIL
NIL
NIL
NIL

BUTTON
8
106
73
139
NIL
shake
NIL
1
T
OBSERVER
NIL
NIL
NIL
NIL

@#$#@#$#@
Title: Shapebugs
Author: Matt Baker
Description:
This is an implementation of the Shapebugs algorithm from
<ul>
<li>Jimming Cheng, Winston Cheng, and Radhika Nagpal. <a href="http://jmvidal.cse.sc.edu/lib/cheng05a.html">&ldquo;Robust and Self-Repairing Formation Control for Swarms of Mobile Agents&rdquo;</a>. In <i>Proceedings of the Twentieth National Conference on Artificial Intelligence,</i> p. 59--64, AAAI Press. 2005</li>
</ul>

My program begins (via the setup button) by randomly dispersing the agents throughout the display.  All agents start in the lost state and assume that they are outside of the desired shape.  An agent inside the shape is assumed to have been found.

Each agent's movement is as follows:
<ul>
<li>If a neighbor reaches a minimum distance the agent repels that neighbor</li>
<li>If an agent finds itself inside of the desired shape, it will follow the above rules as long as such movement does not take it outside of the shape</li>
<li>If an agent is inside of the shape, and it determines that its next move will take it outside of the shape, it will instead do one of 2 things:
	(a) Stay still: 90% probability
	(b) Move either slightly to the right, left, up, or down: 10% probability</li>
</ul>

Agent colors:
<ul>
<li>A red agent is an agent outside of the desired shape</li>
<li>A blue agent is an agent inside of the desired shape</li>
<li>A green agent is an agent that has been displaced</li>
</ul>

Agents can be added or removed dynamically via the slider.  The shake button chooses a random square from the screen and moves all agents within that square in random directions away from their starting positions.

Note: I found that using 1,000+ agents seems to work best for modeling the shape.
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
Circle -7500403 true true 30 30 240

circle 2
false
0
Circle -7500403 true true 16 16 270
Circle -16777216 true false 46 46 210

cow
false
0
Polygon -7500403 true true 200 193 197 249 179 249 177 196 166 187 140 189 93 191 78 179 72 211 49 209 48 181 37 149 25 120 25 89 45 72 103 84 179 75 198 76 252 64 272 81 293 103 285 121 255 121 242 118 224 167
Polygon -7500403 true true 73 210 86 251 62 249 48 208
Polygon -7500403 true true 25 114 16 195 9 204 23 213 25 200 39 123

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
Polygon -7500403 true true 60 270 150 0 240 270 15 105 285 105
Polygon -7500403 true true 75 120 105 210 195 210 225 120 150 75

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
0
@#$#@#$#@
