;exploration rate is decreased for each trial

breed [ blocks ]
breed [ passengers ]
breed [ taxis ]



taxis-own [qtable exploration-rate exploration-count reduction-rate previous-xcor previous-ycor reward trial-count action-count init_xcor init_ycor num-of-actions]

passengers-own [destination]

globals [d-state-number]

to-report get-Qtable-structure
    report n-values world-width [ n-values world-height [n-values num-of-actions [0]]] ; more straight forward encoding but might give flexibility
end

to setup
  ca
  setup-plotting
  ask patches [
    set pcolor white]
  ask patches with [(pxcor + pycor) mod 2 = 0][
    set pcolor black]
  set-default-shape passengers "person"
  set-default-shape taxis "car"
  set-default-shape blocks "tile brick"
  set d-state-number (world-width * world-height * 50)
  
  create-taxis num-taxis [
    setxy random-pxcor random-pycor
    set num-of-actions 4
    ;set xcor (max-pxcor * -1)
    ;set ycor max-pycor
    set init_xcor xcor
    set init_ycor ycor
    
    set previous-xcor (xcor + max-pxcor)
    set previous-ycor (ycor + max-pycor)
    set qtable get-Qtable-structure
    set exploration-rate 1
    set exploration-count 0
    set reduction-rate .9
    set action-count 0
    set trial-count 0
    ]
  
  create-passengers num-taxis [
    setxy random-pxcor random-pycor
    ;setxy 0 0
    ;setxy 2 -3
    ]
    
  create-blocks num-of-blocks [
    setxy random-pxcor random-pycor
  ]
  
  ;show screen-size-x
  ;show screen-size-y
end

to go
  ask taxis [action-execution (choose-action (xcor + max-pxcor) (ycor + max-pxcor)) ]
  ask taxis [learn]
  ;ask taxis [show qtable]
;  ask taxis [show exploration-rate]
end

;to-report get-State [x y]
;    report (list x y)
;end

to-report get-Qvalue [x y action]
    report item action (item y (item x qtable))
end

;to-report get-next-states [action]
;    
;end

to-report get-reward
    ifelse (member? (floor xcor) (map [floor ?] [xcor] of passengers)) and (member? (floor ycor) (map [floor ?] [ycor] of passengers)) [
      ;setxy get-random-xcor get-random-ycor
      ;set previous-xcor (xcor + screen-edge-x)
      ;set previous-ycor (ycor + screen-edge-y)     
      report reward-value]
      [ 
        ifelse ((count taxis-here) = 2) [                                                                   
        report -1 * 100 ]
        [
          ifelse (member? (floor xcor) (map [floor ?] [xcor] of blocks)) and (member? (floor ycor) (map [floor ?] [ycor] of blocks)) [                                                                   
          report -1 * 100 ]
          [ report 0 ]
        ] 
      ]
end

to update-Qtable [action]
    let tmp1 item previous-xcor qtable
    let tmp2 item previous-ycor tmp1
    let current-qvalue item action tmp2 
    ;let new-qvalue (reward + reduction-rate * get-max-qvalue (xcor + screen-edge-x) (ycor + screen-edge-y))
    let new-qvalue (reward + learning-rate * get-max-qvalue (xcor + max-pxcor) (ycor + max-pycor))
    set tmp2 replace-item action tmp2 new-qvalue
    set tmp1 replace-item previous-ycor tmp1 tmp2
    set qtable replace-item previous-xcor qtable tmp1 
end

to learn
  let action 0
  if (heading = 90)[
    set action 1]
  if (heading = 180)[
    set action 2]
  if (heading = 270)[
    set action 3]
  set reward get-reward
  update-Qtable action
  if (reward = reward-value) [
      ;setxy get-random-xcor get-random-ycor
      set xcor init_xcor
      set ycor init_ycor
      set previous-xcor (xcor + max-pxcor)
      set previous-ycor (ycor + max-pycor)
      
      ; change exploration rate when agent gets the reward.
      let exploration-rate-discount (1 - (exploration-count / (d-state-number * 2)))
      ;set exploration-count (exploration-count + 1)
   
      ifelse exploration-rate-discount > 0  [
        set exploration-rate exploration-rate-discount
      ][
        set exploration-rate 0
      ]
      
      set trial-count (trial-count + 1)
      do-plot
      set action-count 0
  ]
    
end

; valance exploration rate and exploitation rate
;to-report choose-action1 [x y]
;    let dice random 10
;    ifelse dice < 9 [
;      let utils map [get-Qvalue x y ?] (list 0 1 2 3)
;      report item (position (max utils) utils) (list 0 90 180 270)
;    ]
;    [
;      let dice1 random 4
;      report item dice1 (list 0 90 180 270)
;    ]
;end

to-report choose-action [x y]
    ;show xcor
    ;show ycor
    
    let dice random-float 1
    let action-list (list 0 1 2 3)
    let action-direction (list 0 90 180 270)
    ifelse (xcor = max-pxcor) [
        set action-list (list 0 2 3)
        set action-direction (list 0 180 270)
        if (ycor = max-pycor) [
          set action-list (list 2 3)
          set action-direction (list 180 270)
        ]
        if (ycor = (-1 * max-pycor)) [
          set action-list (list 0 3)
          set action-direction (list 0 270)
        ]
    ]
    [
      ifelse (xcor = (-1 * max-pxcor)) [
          set action-list (list 0 1 2)
          set action-direction (list 0 90 180)
          if (ycor = max-pycor) [
            set action-list (list 1 2)
            set action-direction (list 90 180)
          ]
          if (ycor = (-1 * max-pycor)) [
            set action-list (list 0 1)
            set action-direction (list 0 90 ) 
          ]
      ][
        if (ycor = max-pycor) [
            set action-list (list 1 2 3)
            set action-direction (list 90 180 270)
        ]
    
        if (ycor = (-1 * max-pycor)) [
            set action-list (list 0 1 3)
            set action-direction (list 0 90 270)
        ]
      ] 
    ]
      
    ifelse dice > exploration-rate [
      let utils map [get-Qvalue x y ?] action-list
      report item (position (max utils) utils) action-direction
    ]
    [
      let dice1 random (length action-list)
      ; reduce the exploratio rate when you chose random action
      ;set exploration-count (exploration-count + 1)
      
      ;show dice1
      ;show item dice1 action-direction
      report item dice1 action-direction
    ]
       
end


to-report get-max-qvalue [x y]
    let utils map [get-Qvalue x y ?] (list 0 1 2 3)
    report (max utils)
end

to action-execution [action-num]
   set heading action-num
   set previous-xcor (xcor + max-pxcor)
   set previous-ycor (ycor + max-pycor)
   
   ; use linear exploration discount over time
   ; let exploration-rate-discount (1 - (exploration-count / d-state-number))
   set exploration-count (exploration-count + 1)
   
   ; ifelse exploration-rate-discount > 0  [
   ;   set exploration-rate exploration-rate-discount
   ; ][
   ;   set exploration-rate 0
   ; ]
   jump 1
   set action-count (action-count + 1)
   ;set heading 0
end

to setup-plotting
  set-current-plot "Performance"
  set-plot-y-range  min-pycor max-pycor          ; set y range to view size
end

to do-plot
  ;set-current-plot-pen "trial-count"
  ;plot trial-count            ; plot xcor -- there's only one turtle, so its id must be 0
  set-current-plot-pen "action-count"
  plot action-count            ; plot ycor -- ditto
end
  
@#$#@#$#@
GRAPHICS-WINDOW
303
10
579
307
3
3
38.0
1
10
1
1
1
0
1
1
1
-3
3
-3
3
0
0
1
ticks

BUTTON
11
18
80
51
Set Up
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
147
19
210
52
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

PLOT
9
312
578
539
Performance
trial
number of moves
0.0
10.0
0.0
10.0
true
false
PENS
"action-count" 1.0 0 -16777216 true

SLIDER
17
103
189
136
learning-rate
learning-rate
0
1
0.9
0.1
1
NIL
HORIZONTAL

SLIDER
16
142
188
175
num-of-blocks
num-of-blocks
0
10
8
1
1
NIL
HORIZONTAL

SLIDER
17
183
189
216
reward-value
reward-value
100
500
300
20
1
NIL
HORIZONTAL

SLIDER
15
64
187
97
num-taxis
num-taxis
1
10
1
1
1
NIL
HORIZONTAL

BUTTON
82
18
145
51
Go
go
NIL
1
T
OBSERVER
NIL
NIL
NIL
NIL

@#$#@#$#@
title: Q-Learning
author: Seang Chan Ryu
description: An implementation of the Q-learning algorithm for a simple path-finding domain.
<ul><li>Christopher J. C. H. Watkins and Peter Dayan. <a href="http://jmvidal.cse.sc.edu/lib/watkins92a.html ">Q-Learning</a>. <i>Machine Learning,</i> 8(3-4):279--292, 1992.</li></ul>
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

tile brick
false
0
Rectangle -1 true false 0 0 300 300
Rectangle -7500403 true true 15 225 150 285
Rectangle -7500403 true true 165 225 300 285
Rectangle -7500403 true true 75 150 210 210
Rectangle -7500403 true true 0 150 60 210
Rectangle -7500403 true true 225 150 300 210
Rectangle -7500403 true true 165 75 300 135
Rectangle -7500403 true true 15 75 150 135
Rectangle -7500403 true true 0 0 60 60
Rectangle -7500403 true true 225 0 300 60
Rectangle -7500403 true true 75 0 210 60

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
