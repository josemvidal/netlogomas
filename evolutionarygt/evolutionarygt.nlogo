breed [arrows arrow]
breed [travelers traveler]

patches-own [population my-next-population next-xcor next-ycor]


globals [ c3 mouse-a mouse-b mouse-c]

to setup
  ifelse (length payoffs = 3) [
    setup3
    ][
    setup2
  ]
end

;the 3x3 payoffs matrix case
to setup3
  ;; (for this model to work with NetLogo's new plotting features,
  ;; __clear-all-and-reset-ticks should be replaced with clear-all at
  ;; the beginning of your setup procedure and reset-ticks at the end
  ;; of the procedure.)
  __clear-all-and-reset-ticks
  set c3 (sqrt 3) / 3
  print-payoffs
  ask patches [
    set pcolor white
    set population to-population pxcor pycor
  ]
; ask patches with [item 0 population < 0 or item 1 population < 0 or item 2 population < 0][
;  set pcolor 4]
 crt 1 [
   set shape "link"
   set color black
   set heading 90
   setxy (max-pxcor) / 2 0
   set size max-pxcor
 ]
 crt 1 [
   set color black
   set shape "link"
   set heading 30
   setxy (max-pxcor / 4) max-pycor * sqrt(3) / 4
   set size max-pxcor
 ]
 crt 1 [
   set color black
   set shape "link"
   set heading -30
   setxy 3 * (max-pxcor / 4) max-pycor * sqrt(3) / 4
   set size max-pxcor
 ]
 crt 1 [
   set size 0
   setxy -.5 .8
   set label-color black
   set label "a"
 ]
 crt 1 [
   set size 0
   setxy  (max-pxcor - .4) .8
   set label-color black
   set label "b"
 ]
 crt 1 [
   set size 0
   setxy (max-pxcor / 2) (max-pxcor * sqrt(3) / 2)
   set label-color black
   set label "c"
 ]
 ask patches with [legal-population? population][
   sprout 1 [
     set breed arrows
     set shape "thinarrow"
     set color 18 ;light red
     set heading 0
     let my-pop to-population xcor ycor
     let endpoint to-coordinates next-population my-pop
     set next-xcor item 0 endpoint
     set next-ycor item 1 endpoint
     if (next-xcor != xcor or next-ycor != ycor) [
       set heading towardsxy next-xcor next-ycor
     ]
     set size 2 * distancexy next-xcor next-ycor ;end of arrow is my next point
     if (size < .01) [
       set shape "circle" 
       set size .3
     ]
;      set size 2
   ]
 ]
end

to print-payoffs
  clear-output
  output-print "   a  b  c"
  (foreach payoffs (list "a" "b" "c") [output-print (word ?2 " " ?1)])
end

to go
  ifelse (length payoffs = 3) [
    go3
   ][
    go2
   ]
end

to go3
  ask travelers [
    let my-pop to-population xcor ycor
    let endpoint to-coordinates next-population my-pop
    let xe item 0 endpoint
    let ye item 1 endpoint
    ifelse (abs (xe - xcor) > .01 or abs (ye - ycor) > .01 ) [
      set heading towardsxy xe ye
;      forward (distancexy xe ye) / 10
      setxy xe ye
    ][
      die
    ]
  ] 
  ifelse (mouse-inside?) [
    let mx mouse-xcor
    let my mouse-ycor
    let mouse-population to-population mx my
    ifelse (legal-population? mouse-population)[
      set mouse-a item 0 mouse-population
      set mouse-b item 1 mouse-population
      set mouse-c item 2 mouse-population
      if (mouse-down?) [
        create-travelers 1 [
          setxy mx my
          set color blue
          hide-turtle
          pen-down
        ]
      ]
    ][
    set mouse-a "NA"
    set mouse-b "NA"
    set mouse-c "NA" 
    ]
  ][
    set mouse-a "NA"
    set mouse-b "NA"
    set mouse-c "NA"
  ]
end

;Functions for dealing with populations and their mapping to coordinates
;In the 3x3 symmetric matrix case

to-report legal-location? [x y]
  let pop to-population x y
  report item 0 pop >= 0 and item 1 pop >= 0 and item 2 pop >= 0
end

to-report legal-population? [pop]
  report item 0 pop >= 0 and item 1 pop >= 0 and item 2 pop >= 0
end

to-report to-population [x y]
  set x x / max-pxcor
  set y y / max-pycor
  let a 1 - x - (y * c3)
  let b x - (y * c3)
  let c 2 * y * c3
  report (list a b c)
end

to-report to-coordinates [pop]
  let a item 0 pop
  let b item 1 pop
  let c item 2 pop
  let x b + .5 * c
  let y .5 * c * sqrt (3)
  report (list (x * max-pxcor) (y * max-pycor))
end

;Reports the total expected utility for performing an action
;whose payoffs are given by 'payoff-row' and assuming population 'pop'
to-report get-total-utility [pop payoff-row]
  let values (map [?1 * ?2]
    payoff-row
    pop)
  report sum values
end

;Assumes payoffs is a symetric matrix
to-report next-population [pop]
  let utilities (map [get-total-utility pop ?1]
    payoffs)
  let avg mean utilities
  let population-avg-payoff sum (map [?1 * (get-total-utility pop ?2)] pop payoffs)
  let s sum utilities
  let new-pop (map [?2 * (1 + ?1) ] utilities pop) ;1 because, if a population gets 0 utility it stays the same. Works.
;  let new-pop (map [?2 * (1 + ?1 - avg)] utilities pop) ;new population is proportional to distance from average utility
;  let new-pop (map [?1 * (1 + (?3 - population-avg-payoff))] pop payoffs utilities)
  let new-total sum new-pop
  report map [ifelse-value (new-total = 0) [0][? / new-total]] new-pop ;scale it back to 1
;  report map [ifelse-value (s = 0) [0][? / s]] utilities
end

;-----------------------------------------------------------------------------------------------------------------------
;Functions for dealing with populations and their mapping to coordinates
;In the 2x2 matrix case

to setup2
  ;; (for this model to work with NetLogo's new plotting features,
  ;; __clear-all-and-reset-ticks should be replaced with clear-all at
  ;; the beginning of your setup procedure and reset-ticks at the end
  ;; of the procedure.)
  __clear-all-and-reset-ticks
  print-payoffs2
  ask patches [
    set pcolor white
    set population to-population2 pxcor pycor
  ]
 ask patches with [legal-population2? population][
   sprout 1 [
     set breed arrows
     set shape "thinarrow"
     set color 18 ;light red
     set heading 0
     set population to-population2 xcor ycor
     set my-next-population next-population2 population
     let endpoint to-coordinates2 my-next-population
     set next-xcor item 0 endpoint
     set next-ycor item 1 endpoint
     if (next-xcor != xcor or next-ycor != ycor) [
       set heading towardsxy next-xcor next-ycor
     ]
     set size 2 * distancexy next-xcor next-ycor ;end of arrow is my next point
     if (size < .01) [
       set shape "circle" 
       set size .3
     ]
;      set size 2
   ]
 ]
end

to go2
  ask travelers [
    let my-pop to-population2 xcor ycor
    let endpoint to-coordinates2 next-population2 my-pop
    let xe item 0 endpoint
    let ye item 1 endpoint
    ifelse (abs (xe - xcor) > .01 or abs (ye - ycor) > .01 ) [
      set heading towardsxy xe ye
;      forward (distancexy xe ye) / 10
      setxy xe ye
    ][
      die
    ]
  ] 
  ifelse (mouse-inside?) [
    let mx mouse-xcor
    let my mouse-ycor
    let mouse-population to-population2 mx my
    ifelse (legal-population2? mouse-population)[
      set mouse-a item 0 mouse-population
      set mouse-b item 1 mouse-population
      if (mouse-down?) [
        create-travelers 1 [
          setxy mx my
          set color blue
          hide-turtle
          pen-down
        ]
      ]
    ][
    set mouse-a "NA"
    set mouse-b "NA"
    set mouse-c "NA" 
    ]
  ][
    set mouse-a "NA"
    set mouse-b "NA"
    set mouse-c "NA"
  ]
end


to print-payoffs2
  clear-output
  output-print "      a      b"
  (foreach payoffs (list "a" "b") [output-print (word ?2  " " ?1)])
end

to-report legal-location2? [x y]
  let pop to-population2 x y
  report item 0 pop >= 0 and item 1 pop >= 0
end

to-report legal-population2? [pop]
  report item 0 pop >= 0 and item 1 pop >= 0
end

to-report to-population2 [x y]
  set x x / max-pxcor
  set y y / max-pycor
  report (list x y)
end

to-report to-coordinates2 [pop]
  report (list (max-pxcor * item 0 pop) (max-pycor * item 1 pop))
end

to-report get-player-0-utility [pop]
  let a item 0 pop
  let b (1 - a)
  let his-a item 1 pop
  let his-b (1 - his-a)
  let row1 item 0 payoffs
  let row2 item 1 payoffs
  report a * (his-a * first item 0 row1 + his-b * first item 1 row1) + b * (his-a * first item 0 row2 + his-b * first item 1 row2)
end

to-report get-player-1-utility [pop]
  let a item 1 pop
  let b (1 - a)
  let his-a item 0 pop
  let his-b (1 - his-a)
  let col1 map [first ?] payoffs
  let col2 map [item 1 ?] payoffs
  report a * (his-a * item 1 item 0 col1 + his-b * item 1 item 1 col1) + b * (his-a * item 1 item 0 col2 + his-b * item 1 item 1 col2)
end

;Assumes payoffs is a symetric matrix
to-report next-population2 [pop]
  let p0-pop first pop
  let p1-pop item 1 pop
  let p0-utilities (list get-player-0-utility (list 1 p1-pop) get-player-0-utility (list 0 p1-pop))
  let p0-new-pop (map [?2 * (1 + ?1)] p0-utilities (list p0-pop (1 - p0-pop)))
  let p0-new-total sum p0-new-pop
  set p0-new-pop map [ifelse-value (p0-new-total = 0) [0] [? / p0-new-total]] p0-new-pop ;normalize population
  
  let p1-utilities (list get-player-1-utility (list 1 p0-pop) get-player-1-utility (list 0 p0-pop))
  let p1-new-pop (map [?2 * (1 + ?1)] p1-utilities (list p1-pop (1 - p1-pop)))
  let p1-new-total sum p1-new-pop
  set p1-new-pop map [ifelse-value (p1-new-total = 0) [0] [? / p1-new-total]] p1-new-pop ;normalize population
  
  report (list first p0-new-pop first p1-new-pop)
  
;  let population-avg-payoff sum (map [?1 * (get-player-0 pop ?2)] pop payoffs)
;  let s sum utilities
;  let new-pop (map [?2 * (1 + ?1) ] utilities pop) ;1 because, if a population gets 0 utility it stays the same. Works.
;  let new-total sum new-pop
;  report map [ifelse-value (new-total = 0) [0][? / new-total]] new-pop ;scale it back to 1
end


;-------------------------------------------------------------------------------
;functions to output drawing as pgf. You probably don't care about this.

to output-to-file [file]
  file-open file
  ask arrows [
    ifelse (shape = "thinarrow") [
      file-print (word "\\draw[->] (" xcor "," ycor") -- (" next-xcor "," next-ycor ");")
    ][
      file-print (word "\\draw[->] (" xcor "," ycor") circle (5pt);")
    ]
  ]
  file-close
end
@#$#@#$#@
GRAPHICS-WINDOW
210
10
708
529
-1
-1
44.4
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
10
0
10
0
0
1
ticks
30.0

BUTTON
5
23
77
56
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

BUTTON
79
23
142
56
go
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
23
207
56
go
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
17
391
74
444
a
mouse-a
3
1
13

MONITOR
77
391
134
444
b
mouse-b
3
1
13

MONITOR
136
391
193
444
c
mouse-c
3
1
13

OUTPUT
7
179
132
280
12

TEXTBOX
22
363
172
381
Mouse Position
13
0.0
0

CHOOSER
3
122
206
167
payoffs
payoffs
[[1 0 0] [0 0 0] [0 0 0]] [[5 4 4] [4 4 4] [4 4 4]] [[1 2 0] [0 1 2] [2 0 1]] [[1 4 0] [0 1 4] [4 0 1]] [[1 3 0] [0 1 3] [3 0 1]] [[1 0 0] [0 1 0] [0 0 1]] [[1 0 1] [1 0 0] [0 1 2]] [[1 1 0] [0 1 1] [1 0 1]] [[0 1 0] [0 1 0] [0 1 0]] [[1 1 1] [1 1 0] [0 0 0]] [[0 1 0] [0 0 2] [0 0 1]] [[0 2 0] [2 0 0] [1 1 0]] [[1 5 0] [0 1 5] [5 0 4]] [[2 2 2] [1 2 1] [1 2 1]] [[1 2 1] [2 1 2] [1 2 1]] [[0 2 0] [2 0 2] [1 1 1]] [[0 3 0] [3 0 3] [1 1 1]] [[2 0 1] [0 2 1] [1 1 0]] [[[1 1] [0 2]] [[2 0] [3 3]]] [[[1 0] [0 1]] [[0 1] [1 0]]] [[[1 0] [0 0]] [[0 0] [1 0]]]
2

@#$#@#$#@
# Evolutionary Game Theory  

## WHAT IS IT?
A visualization of a population of agents playing repeated games under the standard evolutionary game theory assumptions. We use a simplex to show how these populations evolve over time. Click the forever-go button and then click on the simplex to show how a population at that spot would evolve.

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

thinarrow
true
0
Line -7500403 true 150 0 150 150
Line -7500403 true 150 0 135 30
Line -7500403 true 150 0 165 30

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
