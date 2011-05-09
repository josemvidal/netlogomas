breed [agents agent]

agents-own [utility-vector last-action]
;utility-vector has one item for each action from 'actions', the value of the item is the utility the agent has gotten from that action. Utilities are updated with a discount-rate.
;last-action is the last action I took

globals [actions attendance]
;num-nights is 'l' from the paper. It is the number of nights per week that each agent goes out to the bar.
;actions is a list of 7 elements, each one is a list of 7 elements (0,1) where 1 indicates attendance that night: [[1 0 0 0 0 0 0][0 1 0 0 0 0 0]....]
;discount-rate is a number between 0 and 1, util = util*discount-rate at every time step
;attendance is the attendance vector this week

to setup
  ;; (for this model to work with NetLogo's new plotting features,
  ;; __clear-all-and-reset-ticks should be replaced with clear-all at
  ;; the beginning of your setup procedure and reset-ticks at the end
  ;; of the procedure.)
  __clear-all-and-reset-ticks
  create-agents num-agents [
    hide-turtle
    setxy 0 who
    set utility-vector n-values 7 [100]
  ]
  set actions []
  foreach n-values 7 [?] [
    let start-day ?
    let action n-values 7 [0]
    repeat num-nights [
      set action replace-item start-day action 1
      set start-day (start-day + 1) mod 7
    ]
    set actions lput action actions
  ]
  repeat 100 [
    ask agents [take-random-action]
    set attendance reduce [add ?1 ?2] [last-action] of agents
    ask agents [learn]
  ]
end

to-report add [v1 v2]
  report (map [?1 + ?2] v1 v2)
end

to-report subtract [v1 v2]
  report (map [?1 - ?2] v1 v2)
end


;Exponential utility function, the maximum is given by the specific list item (for num-nights=1 its is 3).
;The best global strategy given this utility function is to have 6 nights where attendance maximizes utility for that night (3 agents)
; and everyone else goes to the 7th night.
to-report global-utility [att]
  report sum map [? * exp ((0 - ?) / (item (num-nights - 1)  [3 6 8 10 12 15]))] att
end

to go
  ask agents [take-action]
  set attendance reduce [add ?1 ?2] [last-action] of agents
  ask agents [show-action]
  ask agents [learn]
  set-current-plot "Attendance"
  clear-plot 
  set-plot-pen-mode 1
  foreach attendance [plot ?]
  set-current-plot "Global Utility"
  plot global-utility attendance
end

;Returns an index in the range [0..length v] chosen stochastically---in proportion to the values of the items in v.
to-report stochastic-choice [v]
  let r random-float sum v
  let index 0
  let base-prob 0
  repeat length v [
    set base-prob base-prob + (item index v)
    if (r <= base-prob) [ report index]
    set index index + 1
    ]
 show "ERROR: stochastic-choice"
 show v
 show sum v
 show base-prob
end

to-report boltzmann-choice [v]
  report stochastic-choice map [exp (? / 3)] v
end

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;agents' functions

;report action index by choosing stochastically using utility-vector. Note that the paper uses Boltzmann distribution
to take-action
    set last-action item (stochastic-choice utility-vector) actions
end

to take-random-action
  set last-action item (random length actions) actions
end

to-report get-reward
  if (reward-function = "wonderful-life-0") [
    report 1 + (global-utility attendance) - (global-utility (subtract attendance last-action))]
  if (reward-function = "wonderful-life-1") [
    report 1 + (global-utility attendance) - global-utility (add (subtract attendance last-action) [1 1 1 1 1 1 1])]
  if (reward-function = "aristocratic") [
    report 1 +  (global-utility attendance) - global-utility (add (subtract attendance last-action) n-values 7 [1 / 7])
  ]
  show (word "ERROR: get-reward:" reward-function)
end

;update my utility-matrix using the appropiate reward function and age all utility values.
to learn
  set utility-vector map [? * discount-rate] utility-vector ;age all utilities
  let index position last-action actions
  let reward get-reward
;  set utility-vector replace-item index utility-vector ((item index utility-vector) + learning-rate * reward)
  set utility-vector replace-item index utility-vector ((item index utility-vector) + reward)
end

to show-action
  ask patches with [pycor = [who] of myself] [set pcolor black]
  ask patches with [pycor = [who] of myself and (item pxcor [last-action] of myself = 1)] [set pcolor blue]
end
@#$#@#$#@
GRAPHICS-WINDOW
214
10
459
1672
-1
-1
25.9
1
4
1
1
1
0
0
0
1
0
6
0
62
0
0
1
ticks
30.0

SLIDER
7
39
179
72
num-agents
num-agents
1
100
63
1
1
NIL
HORIZONTAL

BUTTON
5
278
77
311
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

SLIDER
7
73
179
106
num-nights
num-nights
1
6
1
1
1
NIL
HORIZONTAL

SLIDER
8
107
180
140
discount-rate
discount-rate
0
1
0.98
0.01
1
NIL
HORIZONTAL

BUTTON
79
278
142
311
NIL
go
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
144
278
207
311
NIL
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

PLOT
409
10
685
159
Attendance
NIL
NIL
0.0
7.0
0.0
10.0
true
false
"" ""
PENS
"default" 1.0 0 -16777216 true "" ""

PLOT
408
160
684
310
Global Utility
NIL
NIL
0.0
7.0
2.0
3.0
true
false
"" ""
PENS
"default" 1.0 0 -16777216 true "" ""

CHOOSER
8
142
180
187
reward-function
reward-function
"wonderful-life-0" "wonderful-life-1" "aristocratic"
2

@#$#@#$#@
# COIN  

## WHAT IS IT?

This model implements the COllective INtelligence framework and attempts to reproduce the results from

 * David Wolpert and Kagan Tumer. [Optimal Payoff Functions for Members of Collectives](http://jmvidal.cse.sc.edu/lib/wolpert01a.html). _Advances in Complex Systems,_  4(2--3):265--279, 2001.

It implements the wonderful life utility (WLU) clamped down to 0 (no attendance) and 1 (attend every day) and the aristrocatic utility function for the El Farol Bar problem. Since the paper does not mention the exact parameter values used I have been unable to exactly reproduce their results. Namely, this simulation seems to take longer to converge than theirs.

Each row in the main output window represents one agent and each of the 7 columns is a day of the week. The patch is set to blue when the agent attends that particular night. Note that the agent often converge if you let the model run over 1000 steps.

Optimal global utility is achieved, for num-nights=1, if 3 agents attend every night except one night when all the other agents attend.

## CREDITS

Jose M Vidal

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
