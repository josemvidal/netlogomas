; path-finding with pheronomes problem
; by
; Jose M Vidal

breed [ ghosts ]
breed [ targets ]
breed [ hazards ]
breed [ walkers ]


;There are 4 pheronomes. Their values range from 0-1.
patches-own [target hazard to-target to-home]

;when wait-steps is 0 the walker can fd 1, these set it back to walker-speed.
walkers-own [wait-steps steps hazards-hit]

;mode = "to-target" or "to-home"
ghosts-own [mode steps drop-size]


to-report get-random-xcor
    report (random world-width) + min-pxcor
end

to-report get-random-ycor
    report (random world-height) + min-pycor
end

to setup
  ca
  set-default-shape walkers "box"
  set-default-shape targets "circle"
  set-default-shape hazards "box"
  create-hazards num-hazards [
    setxy get-random-xcor get-random-ycor
    set color orange
    set heading 0
    ask patch-here [
      set hazard 10]
  ]  
  create-targets num-targets [
    setxy get-random-xcor get-random-ycor
    set color green
    ask patch-here [
      set target 10
    ]
  ]
  create-walkers 1 [
    set wait-steps walker-speed
    setxy get-random-xcor get-random-ycor
    set color magenta]
end

to update
  ask targets [go-targets]
  ask hazards [go-hazards]
  ask ghosts [find-target-or-home]
  ask walkers [move-to-target]
  diffuse target diffusion-rate / 100
  diffuse hazard diffusion-rate / 100
  diffuse to-target diffusion-rate / 100
  diffuse to-home diffusion-rate / 100
  ask patches [go-patches]
  
  if (first [at-target?] of walkers) [stop]
end

;ghosts
to find-target-or-home
  set steps steps + 1
  if (steps > ghosts-lifetime) [die]
  ifelse (mode = "to-target")[ ;;looking for the target
    if (any? targets-here) [
      set mode "to-home"
      set color magenta
      set drop-size 20
      rt 180
      stop]
    set to-home (to-home + drop-size)
    set drop-size drop-size - 1
    if (drop-size < 1) [set drop-size 1]
    uphill target
;    set heading uphill target ; or to-target
;    fd 1
  ]
  [
    if (any? walkers-here) [ ;were home
      die]
    set to-target (to-target + drop-size)
    set drop-size drop-size - 1
    if (drop-size < 1) [set drop-size 1]
    uphill-to-home
    fd 1
  ]
end

to uphill-to-home
  let ahead 0
  let scent-left 0
  let scent-right 0
  
  ;; sniff left and right, and go where the strongest smell is
  set ahead next-to-home
  rt 45
  set scent-right next-to-home
  lt 90
  set scent-left next-to-home
  rt 45
  if ((scent-right > ahead) or (scent-left > ahead))
    [ ifelse (scent-right > scent-left) 
      [ rt 45 ]
      [ lt 45 ]
  ]
end

to-report next-to-home  ;; reports what the strength of chemical right in front of the ant is   
    report [to-home] of patch-at dx dy
end

;walkers
to move-to-target
  if (wait-steps = 0)[
    set wait-steps walker-speed
    set steps steps + 1
    uphill to-target
;    set heading uphill to-target
;    fd 1
    if (any? hazards-here) [
      set hazards-hit hazards-hit + 1] 
    ]
  set wait-steps wait-steps - 1
  if (count ghosts < max-num-ghosts) [
    ask patch-here [
      sprout 1 [
        set breed ghosts
        set drop-size 20
        set mode "to-target"
        set color green
        fd 1]]]
end

to-report at-target?
  report any? targets-here
end

;patches
to go-patches
  set target target * (100 - evaporation-rate) / 100
  set hazard hazard * (100 - evaporation-rate) / 100
  set to-target to-target * (100 - evaporation-rate) / 100
  set to-home to-home * (100 - evaporation-rate) / 100
  if (show-target) [
    set pcolor scale-color yellow target 0 1]
  if (show-hazard) [
    set pcolor scale-color yellow hazard 0 1]
  if (show-to-home) [
    set pcolor scale-color yellow to-home 0 1]
  if (show-to-target) [
    set pcolor scale-color yellow to-target 0 1]
end

;hazards
to go-hazards
  set hazard hazard + 1
end

;targets
to go-targets
  set target target + 1
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
3
76
175
109
num-targets
num-targets
1
100
1
1
1
NIL
HORIZONTAL

SLIDER
3
142
175
175
walker-speed
walker-speed
0
100
50
1
1
NIL
HORIZONTAL

SLIDER
3
175
176
208
max-num-ghosts
max-num-ghosts
0
100
5
1
1
NIL
HORIZONTAL

BUTTON
3
42
84
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

BUTTON
84
42
165
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

BUTTON
165
42
246
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

SLIDER
3
109
175
142
num-hazards
num-hazards
1
100
100
1
1
NIL
HORIZONTAL

SLIDER
4
241
176
274
diffusion-rate
diffusion-rate
0
100
10
1
1
NIL
HORIZONTAL

SLIDER
4
274
176
307
evaporation-rate
evaporation-rate
0
100
5
1
1
NIL
HORIZONTAL

SWITCH
564
42
702
75
show-target
show-target
1
1
-1000

SWITCH
564
75
710
108
show-hazard
show-hazard
1
1
-1000

SWITCH
564
108
720
141
show-to-home
show-to-home
1
1
-1000

SWITCH
564
141
722
174
show-to-target
show-to-target
1
1
-1000

SLIDER
4
208
176
241
ghosts-lifetime
ghosts-lifetime
0
100
50
1
1
NIL
HORIZONTAL

MONITOR
177
76
234
121
steps
[steps] of one-of walkers
0
1
11

MONITOR
177
126
244
171
hazards
[hazards-hit] of one-of walkers
0
1
11

@#$#@#$#@
Title: Path-Finding Using Pheromones
Author: Jose M Vidal
Description:
In this problem a series of drones tries to find a path from the source
to the target while staying away from the obstacles. They use pheromones.
I tried to implement something similar to
<ul>
<li>
H. Van Dyke Parunak, Sven Brueckner, and John Sauter. 
<a href="http://jmvidal.cse.sc.edu/library/index.html#parunak02a">
Synthetic pheronome mechanisms for coordination of unmanned vehicles</a>. 
In Proceedings of the First Intenational Joint Conference on Autonomous Agents and Multiagent Systems, pages 448-450, Bologna, Italy, 2002. ACM Press, New York, NY.
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
