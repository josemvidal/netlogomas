; Congregation in market systems
; by
; Jose M Vidal

breed [ markets ]
breed [ buyers ]
breed [ sellers ]


;preferences- item x preferences = ordering of item number x in my prefs, 0 being my most preferred item
;valuations- item x valuations = value of item number x for me. This implements eq.1
;payment- valuation of the good received
buyers-own [preferences valuations reservation-price payment] 

;payment- amount of money(valuation) received for good
sellers-own [good reservation-price payment]

markets-own [computational-cost]

patches-own [market] ;the number of the market agent at this spot.

;profit- the sum of all seller's profit
globals [profit comp-cost]

to setup
  ca
  setup-buyers
  setup-sellers
  setup-markets
end

to update
  if (num-markets != count markets) [
    cp
    ask markets [die]
    setup-markets
    ask buyers [set payment 0]
    ask sellers [set payment 0]]
  ask buyers [go-to-market]
  ask sellers [go-to-market]
  ask markets [do-auction]
  set profit (sum [payment] of sellers ) / num-sellers
  set comp-cost (sum [computational-cost] of markets ) / num-markets
  set-current-plot "Profits"
  set-plot-pen-mode 0
  set-current-plot-pen "profit"
  plot profit
  set-current-plot-pen "comp"
  plot comp-cost * 10
  set-current-plot "Histogram"
  set-plot-pen-mode 1
  set-plot-x-range 0 10
  set-plot-y-range 0 10
  auto-plot-on
  histogram  [payment] of sellers
  
end

to-report get-random-xcor
    report (random world-width) + min-pxcor
end

to-report get-random-ycor
    report (random world-height) + min-pycor
end

to setup-markets
  set-default-shape markets "box"
  create-markets num-markets [
    set heading 0
    set color white
    set market who
    setxy get-random-xcor get-random-ycor]
  repeat 10 [
    ask markets [disperse]]
  ask markets [
    set heading 0
    set xcor pxcor ;center on patch
    set ycor pycor
    set pcolor white]
end

to setup-buyers
  create-buyers num-buyers [
    setxy get-random-xcor get-random-ycor
    set color green
    set reservation-price (random-float 5.0) + 5.0
    setup-preferences]
end

to setup-sellers
  create-sellers num-sellers [
    setxy get-random-xcor get-random-ycor
    set color red
    set reservation-price (random-float 5.0) + 5.0
    set good random num-goods]
end

;reports list which contains number 0..n-1
to-report make-list [n]
  let l 0
  
  set l lput (n - 1) []
  while [n > 0][
    set n n - 1
    set l lput n l]
  report l
end

to-report randomize-list [lst]
  let i 0
  let len 0
  let num-reps 0
  let a 0
  let a-val 0
  let b 0
  
  set len length lst
  set num-reps len * 2
  while [i < num-reps] [
    set a random len
    set a-val item a lst
    set b random len
    set lst replace-item a lst (item b lst)
    set lst replace-item b lst a-val
    set i i + 1
    ]
  report lst
end

;markets
to disperse
  if (count markets < 2) [stop]
  let closest min-one-of (markets with [who != [who] of myself]) [distance myself]
  if (distance closest > 1) [
    set heading 180 + towards closest
    fd 1
  ]
end

to do-auction
  let producers 0
  let consumers 0
  let next-producer 0
  let winning-buyer 0
  let second-buyer 0
  let num-agents-here 0
  
  set producers sellers-here
  set consumers buyers-here
  set num-agents-here count producers + count consumers
  ;they receive payment of 0 if the are unable to do a transaction
  ask producers [set payment 0]
  ask consumers [set payment 0]

  ;the paper is not clear on how this cost is calculated. We give it a shot.
  set computational-cost 0.1 * (count producers + count consumers ;messages
    + (count producers * count consumers)) ;comparisons
    
  while [any? producers and any? consumers][
    ;find the one whose product has the best (lowest rank) preference from the remaining buyers
;    set next-producer min-one-of producers [min values-from consumers [item (good-of myself) preferences]]
    ;find the one whose product has the highest valuation among the remaining buyers
    set next-producer max-one-of producers [max [item ([good] of myself) valuations] of consumers]
    
    
    set winning-buyer max-one-of consumers [item ([good] of next-producer) valuations]
    set second-buyer max-one-of (consumers with [who != [who] of winning-buyer]) [item ([good] of next-producer) valuations]
    if (second-buyer = nobody) [             ;is this OK?
      set second-buyer winning-buyer]
    ask next-producer [
      set payment  (item ([good] of next-producer) ([valuations] of second-buyer)) ]
;    set [payment] of next-producer (item ([good] of next-producer) ([valuations] of second-buyer))
    ask winning-buyer [
      set payment  item ([good] of next-producer) [valuations] of winning-buyer]
;    set [payment] of winning-buyer item ([good] of next-producer) [valuations] of winning-buyer
    set consumers consumers with [who != [who] of winning-buyer]
    set producers producers with [who != [who] of next-producer]
    ]
  ask sellers-here [set payment payment - ([computational-cost] of myself / num-agents-here)]
  ask buyers-here [set payment payment - ([computational-cost] of myself / num-agents-here)]
end

;buyers
to setup-preferences
  let op 0
  
  set op randomize-list make-list num-goods
  set preferences op
  set valuations []
  while [not empty? op][
    set valuations lput (reservation-price * (1 - (first op / num-goods ))) valuations;eq.1
    set op butfirst op]
end

to go-to-market
  let nm 0
  
  if (payment < tau * reservation-price)[
    set nm one-of markets with [who != market]
    if (nm = nobody) [stop]
    if (distance nm > 1) [
      set heading towards nm
      fd distance nm]]
end
@#$#@#$#@
GRAPHICS-WINDOW
247
10
572
356
17
17
9.0
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
1
76
173
109
num-markets
num-markets
1
500
100
1
1
NIL
HORIZONTAL

BUTTON
1
42
82
75
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

SLIDER
1
109
173
142
num-goods
num-goods
1
100
10
1
1
NIL
HORIZONTAL

SLIDER
1
142
173
175
num-buyers
num-buyers
1
500
50
1
1
NIL
HORIZONTAL

SLIDER
1
174
173
207
num-sellers
num-sellers
1
500
50
1
1
NIL
HORIZONTAL

SLIDER
1
207
173
240
tau
tau
0
1
0.5
0.01
1
NIL
HORIZONTAL

BUTTON
82
42
163
75
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

MONITOR
174
76
231
121
NIL
profit
2
1
11

BUTTON
163
42
244
75
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

PLOT
564
42
864
192
Profits
time
NIL
0.0
100.0
0.0
10.0
true
true
PENS
"profit" 1.0 0 -2674135 true
"comp" 1.0 0 -10899396 true

PLOT
565
194
765
344
Histogram
profit
#sellers
0.0
100.0
0.0
100.0
true
false
PENS
"default" 1.0 0 -16777216 true

MONITOR
174
126
231
171
comp
comp-cost
2
1
11

@#$#@#$#@
Title: Congregating in Market Systems
Author: Jose M Vidal
Description: We implement the congregating algorithm from
<ul>
<li>Chistopher H. Brooks and Edmund H. Durfee. 
<a href="hhttp://jmvidal.cse.sc.edu/library/index.html#brooks02a">
Congregating and market formation.</a>
In Proceedings of the 1st International Joint Conference on Autonomous Agents and MultiAgent Systems, pages 96-103, 2002.
</li>
</ul>
@#$#@#$#@
default
true
0
Polygon -7500403 true true 150 5 40 250 150 205 260 250

ant
true
0
Polygon -7500403 true true 136 61 129 46 144 30 119 45 124 60 114 82 97 37 132 10 93 36 111 84 127 105 172 105 189 84 208 35 171 11 202 35 204 37 186 82 177 60 180 44 159 32 170 44 165 60
Polygon -7500403 true true 150 95 135 103 139 117 125 149 137 180 135 196 150 204 166 195 161 180 174 150 158 116 164 102
Polygon -7500403 true true 149 186 128 197 114 232 134 270 149 282 166 270 185 232 171 195 149 186
Polygon -7500403 true true 225 66 230 107 159 122 161 127 234 111 236 106
Polygon -7500403 true true 78 58 99 116 139 123 137 128 95 119
Polygon -7500403 true true 48 103 90 147 129 147 130 151 86 151
Polygon -7500403 true true 65 224 92 171 134 160 135 164 95 175
Polygon -7500403 true true 235 222 210 170 163 162 161 166 208 174
Polygon -7500403 true true 249 107 211 147 168 147 168 150 213 150

arrow
true
0
Polygon -7500403 true true 150 0 0 150 105 150 105 293 195 293 195 150 300 150

bee
true
0
Polygon -1184463 true false 151 152 137 77 105 67 89 67 66 74 48 85 36 100 24 116 14 134 0 151 15 167 22 182 40 206 58 220 82 226 105 226 134 222
Polygon -16777216 true false 151 150 149 128 149 114 155 98 178 80 197 80 217 81 233 95 242 117 246 141 247 151 245 177 234 195 218 207 206 211 184 211 161 204 151 189 148 171
Polygon -7500403 true true 246 151 241 119 240 96 250 81 261 78 275 87 282 103 277 115 287 121 299 150 286 180 277 189 283 197 281 210 270 222 256 222 243 212 242 192
Polygon -16777216 true false 115 70 129 74 128 223 114 224
Polygon -16777216 true false 89 67 74 71 74 224 89 225 89 67
Polygon -16777216 true false 43 91 31 106 31 195 45 211
Line -1 false 200 144 213 70
Line -1 false 213 70 213 45
Line -1 false 214 45 203 26
Line -1 false 204 26 185 22
Line -1 false 185 22 170 25
Line -1 false 169 26 159 37
Line -1 false 159 37 156 55
Line -1 false 157 55 199 143
Line -1 false 200 141 162 227
Line -1 false 162 227 163 241
Line -1 false 163 241 171 249
Line -1 false 171 249 190 254
Line -1 false 192 253 203 248
Line -1 false 205 249 218 235
Line -1 false 218 235 200 144

bird1
false
0
Polygon -7500403 true true 2 6 2 39 270 298 297 298 299 271 187 160 279 75 276 22 100 67 31 0

bird2
false
0
Polygon -7500403 true true 2 4 33 4 298 270 298 298 272 298 155 184 117 289 61 295 61 105 0 43

boat1
false
0
Polygon -1 true false 63 162 90 207 223 207 290 162
Rectangle -6459832 true false 150 32 157 162
Polygon -13345367 true false 150 34 131 49 145 47 147 48 149 49
Polygon -7500403 true true 158 33 230 157 182 150 169 151 157 156
Polygon -7500403 true true 149 55 88 143 103 139 111 136 117 139 126 145 130 147 139 147 146 146 149 55

boat2
false
0
Polygon -1 true false 63 162 90 207 223 207 290 162
Rectangle -6459832 true false 150 32 157 162
Polygon -13345367 true false 150 34 131 49 145 47 147 48 149 49
Polygon -7500403 true true 157 54 175 79 174 96 185 102 178 112 194 124 196 131 190 139 192 146 211 151 216 154 157 154
Polygon -7500403 true true 150 74 146 91 139 99 143 114 141 123 137 126 131 129 132 139 142 136 126 142 119 147 148 147

boat3
false
0
Polygon -1 true false 63 162 90 207 223 207 290 162
Rectangle -6459832 true false 150 32 157 162
Polygon -13345367 true false 150 34 131 49 145 47 147 48 149 49
Polygon -7500403 true true 158 37 172 45 188 59 202 79 217 109 220 130 218 147 204 156 158 156 161 142 170 123 170 102 169 88 165 62
Polygon -7500403 true true 149 66 142 78 139 96 141 111 146 139 148 147 110 147 113 131 118 106 126 71

box
true
0
Polygon -7500403 true true 45 255 255 255 255 45 45 45

butterfly1
true
0
Polygon -16777216 true false 151 76 138 91 138 284 150 296 162 286 162 91
Polygon -7500403 true true 164 106 184 79 205 61 236 48 259 53 279 86 287 119 289 158 278 177 256 182 164 181
Polygon -7500403 true true 136 110 119 82 110 71 85 61 59 48 36 56 17 88 6 115 2 147 15 178 134 178
Polygon -7500403 true true 46 181 28 227 50 255 77 273 112 283 135 274 135 180
Polygon -7500403 true true 165 185 254 184 272 224 255 251 236 267 191 283 164 276
Line -7500403 true 167 47 159 82
Line -7500403 true 136 47 145 81
Circle -7500403 true true 165 45 8
Circle -7500403 true true 134 45 6
Circle -7500403 true true 133 44 7
Circle -7500403 true true 133 43 8

circle
false
0
Circle -7500403 true true 35 35 230

person
false
0
Circle -7500403 true true 155 20 63
Rectangle -7500403 true true 158 79 217 164
Polygon -7500403 true true 158 81 110 129 131 143 158 109 165 110
Polygon -7500403 true true 216 83 267 123 248 143 215 107
Polygon -7500403 true true 167 163 145 234 183 234 183 163
Polygon -7500403 true true 195 163 195 233 227 233 206 159

spacecraft
true
0
Polygon -7500403 true true 150 0 180 135 255 255 225 240 150 180 75 240 45 255 120 135

thin-arrow
true
0
Polygon -7500403 true true 150 0 0 150 120 150 120 293 180 293 180 150 300 150

truck-down
false
0
Polygon -7500403 true true 225 30 225 270 120 270 105 210 60 180 45 30 105 60 105 30
Polygon -8630108 true false 195 75 195 120 240 120 240 75
Polygon -8630108 true false 195 225 195 180 240 180 240 225

truck-left
false
0
Polygon -7500403 true true 120 135 225 135 225 210 75 210 75 165 105 165
Polygon -8630108 true false 90 210 105 225 120 210
Polygon -8630108 true false 180 210 195 225 210 210

truck-right
false
0
Polygon -7500403 true true 180 135 75 135 75 210 225 210 225 165 195 165
Polygon -8630108 true false 210 210 195 225 180 210
Polygon -8630108 true false 120 210 105 225 90 210

turtle
true
0
Polygon -7500403 true true 138 75 162 75 165 105 225 105 225 142 195 135 195 187 225 195 225 225 195 217 195 202 105 202 105 217 75 225 75 195 105 187 105 135 75 142 75 105 135 105

wolf-left
false
3
Polygon -6459832 true true 117 97 91 74 66 74 60 85 36 85 38 92 44 97 62 97 81 117 84 134 92 147 109 152 136 144 174 144 174 103 143 103 134 97
Polygon -6459832 true true 87 80 79 55 76 79
Polygon -6459832 true true 81 75 70 58 73 82
Polygon -6459832 true true 99 131 76 152 76 163 96 182 104 182 109 173 102 167 99 173 87 159 104 140
Polygon -6459832 true true 107 138 107 186 98 190 99 196 112 196 115 190
Polygon -6459832 true true 116 140 114 189 105 137
Rectangle -6459832 true true 109 150 114 192
Rectangle -6459832 true true 111 143 116 191
Polygon -6459832 true true 168 106 184 98 205 98 218 115 218 137 186 164 196 176 195 194 178 195 178 183 188 183 169 164 173 144
Polygon -6459832 true true 207 140 200 163 206 175 207 192 193 189 192 177 198 176 185 150
Polygon -6459832 true true 214 134 203 168 192 148
Polygon -6459832 true true 204 151 203 176 193 148
Polygon -6459832 true true 207 103 221 98 236 101 243 115 243 128 256 142 239 143 233 133 225 115 214 114

wolf-right
false
3
Polygon -6459832 true true 170 127 200 93 231 93 237 103 262 103 261 113 253 119 231 119 215 143 213 160 208 173 189 187 169 190 154 190 126 180 106 171 72 171 73 126 122 126 144 123 159 123
Polygon -6459832 true true 201 99 214 69 215 99
Polygon -6459832 true true 207 98 223 71 220 101
Polygon -6459832 true true 184 172 189 234 203 238 203 246 187 247 180 239 171 180
Polygon -6459832 true true 197 174 204 220 218 224 219 234 201 232 195 225 179 179
Polygon -6459832 true true 78 167 95 187 95 208 79 220 92 234 98 235 100 249 81 246 76 241 61 212 65 195 52 170 45 150 44 128 55 121 69 121 81 135
Polygon -6459832 true true 48 143 58 141
Polygon -6459832 true true 46 136 68 137
Polygon -6459832 true true 45 129 35 142 37 159 53 192 47 210 62 238 80 237
Line -16777216 false 74 237 59 213
Line -16777216 false 59 213 59 212
Line -16777216 false 58 211 67 192
Polygon -6459832 true true 38 138 66 149
Polygon -6459832 true true 46 128 33 120 21 118 11 123 3 138 5 160 13 178 9 192 0 199 20 196 25 179 24 161 25 148 45 140
Polygon -6459832 true true 67 122 96 126 63 144

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
