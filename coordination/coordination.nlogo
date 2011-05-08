; Coordination game
; by
; Jose M Vidal
; $Id: coordination.nlogo,v 1.3 2005/01/30 20:20:08 jmvidal Exp jmvidal $

turtles-own [past-rewards-blue past-rewards-red update-clock friends]

globals [time num-turtles old-k]

to setup
    ca
    set time 0
    set-default-shape turtles "box"
    set num-turtles world-width * world-height
    set old-k k
    ask patches [
        sprout 1 [
            set heading 0
            set update-clock random update-delay
            ]
     ]
     ask turtles [
        set friends get-friends
        set past-rewards-blue []
        set past-rewards-red []
        ifelse (random-float 1.0 < .5) [
          set color blue] 
        [
          set color red]]
end

to-report converged?
    let c 0
  
    set c count turtles with [color = red]
    if (c = 0 or c = count turtles) [
        report true]
    report false
end

to update
    let c 0
  
    if (converged?) [stop]
    if (old-k != k)[
        ask turtles [set friends get-friends]
        set old-k k]
    ask one-of turtles [doit]
    set-current-plot-pen "red"
    set c count turtles with [color = red]
    plot c
    set-current-plot-pen "blue"
    plot num-turtles - c
    set time time + 1
end

;;turtles
to-report get-payoff [c1 c2]
    if (c1 = c2) [report 1]
    report -1
end

to update-history [p]
    ifelse (color = blue)[
        set past-rewards-blue fput p past-rewards-blue
        set past-rewards-red fput 0 past-rewards-red]
        [
        set past-rewards-blue fput 0 past-rewards-blue
        set past-rewards-red fput p past-rewards-red]
    while [length past-rewards-blue > memory-window] [
        set past-rewards-blue butlast past-rewards-blue
        set past-rewards-red butlast past-rewards-red]
end

to set-next-color
    let b 0
  let w 0
  
    if (update-clock > 0)[
        set update-clock update-clock - 1
        stop]
    set update-clock update-delay
    set b sum past-rewards-blue
    set w sum past-rewards-red
    ifelse (b > w) [
        set color blue]
        [
        if (b < w)[
            set color red]]
end

to-report get-partner
    if (model = 1) [
   ;     report one-of (turtles with [who != [who] of myself]])
        report one-of (other turtles)]
    if (model = 2)[
        report one-of friends]
end

to-report get-friends
    report (other turtles) with [
;            (who != [who] of myself) and 
            (min list abs (who - [who] of myself)
                        abs (num-turtles - abs (who - [who] of myself))) <= k ]
end

to doit
    let the-other 0
    let payoff 0
  
    set the-other get-partner
    set payoff get-payoff color ([color] of the-other)
    update-history payoff
    ask the-other [update-history payoff]
    
    set-next-color
    ask the-other [set-next-color] 
        
end
@#$#@#$#@
GRAPHICS-WINDOW
253
13
630
411
5
5
33.4
1
10
1
1
1
0
1
1
1
-5
5
-5
5
0
0
1
ticks

BUTTON
2
44
84
78
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
84
45
165
78
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

SLIDER
4
81
176
114
update-delay
update-delay
0
200
69
1
1
NIL
HORIZONTAL

PLOT
634
21
888
171
Agents
NIL
NIL
0.0
100.0
0.0
100.0
true
false
PENS
"red" 1.0 0 -2674135 true
"blue" 1.0 0 -13345367 true

MONITOR
634
208
691
261
NIL
time
9
1
13

SLIDER
4
115
176
148
memory-window
memory-window
1
200
200
1
1
NIL
HORIZONTAL

BUTTON
166
45
247
78
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

SLIDER
3
200
175
233
model
model
1
2
2
1
1
NIL
HORIZONTAL

TEXTBOX
10
153
126
197
1=fully connected\n2=regular graph
13
0.0
0

SLIDER
4
234
176
267
k
k
1
100
10
1
1
NIL
HORIZONTAL

@#$#@#$#@
Title: The Coordination Game
Author: Jose M Vidal
Description:
This is the binary coordination game. Each agent can be either red
or blue. At each time step some of the agents change their color simultaneously.
The goal is to find out how long it takes, under different conditions, for the
population to converge to one color. This problem was presented in
<ul>
<li>Yoav Shoham and Moshe Tennenholtz. 
<a href="http://jmvidal.cse.sc.edu/lib/shoham97a.html">
On the emergence of social conventions: modeling, analysis, and simulations</a>. Artificial Intelligence, 94 (139--166) 1997.
</li>
<li>
Jordi Delgado. 
<a href="http://jmvidal.cse.sc.edu/lib/delgado02a.html">
Emergence of social conventions in complex networks</a>. Artificial Intelligence, 141. p. 171--185, 2002.
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
