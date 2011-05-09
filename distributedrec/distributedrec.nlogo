;Distributed recommender system 
; by
;Jose M Vidal

breed [ agents ]
breed [ documents ]


;preferences is a binary 2D vector, we map the first element to x and the second to y
;documents-read is the list of the who of the documents the agent has read
;documents-liked is list of lists
;   item who-x documents-liked contains the list of the who of the documents the agent knows that who-x likes
;      if who-x = who then this is the list of documents the agent knows it likes.
;recommendations is a history of the recommendations received from each agent, indexed by who. (i.e. a list of lists)
;   the list is a binary vector, 0 if I had already read that document, 1 otherwise. First one is most recent.
;rec holds the latest recommendation
;x the payoff I expect from getting a recommendation from each agent, indexed by who of agent.
;utility is the utility for this round
;accrued-utility is the utility the agent has accrued
;accrued-gain
;expected-utility is what this agent can expect to gain by choosing a random document and reading it.
agents-own [preferences documents-read documents-liked recommendations rec x utility accrued-utility accrued-gain expected-utility]

;description is the same structure as agents.preferences
documents-own [description read-by]

globals [time read-all]

to set-coordinates-based-on-prefs [prefs]
    set xcor ((item 0 prefs) * world-width) + min-pxcor
    set ycor ((item 1 prefs) * world-height) + min-pycor
end

to-report make-list [n v]
    let res 0
  
    set res []
    repeat n [
        set res fput v res]
    report res
end

to setup
    let tmpag 0
  
    ;; (for this model to work with NetLogo's new plotting features,
  ;; __clear-all-and-reset-ticks should be replaced with clear-all at
  ;; the beginning of your setup procedure and reset-ticks at the end
  ;; of the procedure.)
  __clear-all-and-reset-ticks
    set time 0
    ask patches [set pcolor white]
    set-default-shape documents "box"
    repeat num-agents [
        create-agent]
    make-documents num-documents
    set read-all 0 ;none has read them all
    ask agents [
        ;these two are just temporary, so I can print them out
        set utility (count (documents in-radius like-radius)) * (reward-reading - cost-reading)
        set accrued-utility utility + ((num-documents - count (documents in-radius like-radius)) * (0 - cost-reading))
        
        ;calculate expected utility. Notice that this value will be wrong
        ;if we start adding documents later on.
        set expected-utility (((num-documents - count (documents in-radius like-radius)) * (0 - cost-reading))
                            +((num-documents - count (documents in-radius like-radius)) * (reward-reading - cost-reading))) /
                                    num-documents
        ]
    type "Max utility="
    print sum  [utility] of agents
    type "Expected utility="
    print sum  [accrued-utility] of agents
    ask agents [
        set utility 0
        set accrued-utility 0]
end

to create-agent
    let other-prefs 0
  let prefs 0
  
    ifelse (random-float 1.0 < clustering-prob and any? agents)[
        set other-prefs [preferences] of (one-of agents)
        set prefs list (((random-float .1) - .05) + (item 0 other-prefs))
                       (((random-float .1) - .05) + (item 1 other-prefs))]
        [
        set prefs list (random-float 1.0) (random-float 1.0)]
    create-agents 1 [  
        set preferences prefs
        set-coordinates-based-on-prefs preferences
        set heading 0
        set size 10
        set color (who * 10) + 5
        set documents-read []
        set documents-liked make-list num-agents []
        set recommendations make-list num-agents []
        set x make-list num-agents 0
        set utility 0
        set accrued-utility 0
        set accrued-gain 0
        create-temporary-plot-pen (word "a" who)
        set-current-plot-pen (word "a" who)
        set-plot-pen-color color
        set rec -1]
        
end

to make-documents [n]
    create-documents n [
        set description list (random-float 1.0) (random-float 1.0)
        set-coordinates-based-on-prefs description
        set read-by []
        set color blue
        set size 1
        set heading 0]
end

to update
    let chosen-agent 0
  
    set time time + 1
    if (time > 30000)[stop]
;    if (random 1.0 <  .1) [create-documents 1]
    if (read-all = num-agents) [stop]
      
    set chosen-agent one-of agents
    ask chosen-agent [set utility 0]
    no-display
    set-current-plot "gain"
    ask chosen-agent [doit]
    display
    ask chosen-agent [set accrued-utility accrued-utility + utility]
    ask agents [
        create-temporary-plot-pen (word "a" who)
        plot accrued-gain]
    set-current-plot "utility"
    plot sum [accrued-utility] of agents 
end

;reports a list which is the disjoint of l1 and l2
to-report disjoint [l1 l2]
    let res 0
  
    set res []
    while [not empty? l1][
        if (member? (first l1) l2)[
            set res fput (first l1) res]
        set l1 butfirst l1]
    report res
end

;returns a list that contains all the elements of l1 except for those that are in l2
; l1 - l2
to-report list-substract [l1 l2]
    let res 0
  
    set res []
    while [not empty? l1][
        if (not member? (first l1) l2)[
            set res fput (first l1) res]
        set l1 butfirst l1]
    report res
end

;;;
;agents member functions

to-report i-like? [doc]
    ask doc [
      set read-by fput ([who] of myself) read-by
    ]
   ; set ([read-by] of doc) fput who ([read-by] of doc)
    report distance doc <= like-radius
end

;assumes I have not read doc, that is, not member? doc documents-read
to read-document [doc recommended?]
    if (doc != nobody) [
        ifelse (i-like? doc)[
            set documents-liked replace-item who documents-liked (fput ([who] of doc) (item who documents-liked))       
            ;draw a line to that document
            pen-down
            set heading towards doc
            fd distance doc
            pen-up
            set-coordinates-based-on-prefs preferences
            set heading 0
            set utility utility + (reward-reading - cost-reading)
            if (recommended?) [
                set accrued-gain accrued-gain + ((reward-reading - cost-reading - cost-message) - expected-utility)]]
            [
            if (recommended?)[
                set accrued-gain accrued-gain - cost-reading]
            set utility utility - cost-reading]
        set documents-read fput ([who] of doc) documents-read
        if (length documents-read = num-documents) [
            set read-all read-all + 1]]
end

to doit
    let chosen-who 0
  
    ifelse (random-float 1.0 < document-explore)[ ;pick a doc at random and read it
        read-document one-of (documents with [not member? who ([documents-read] of myself)]) false]
        [ ;ask someone about it
        ifelse (random-float 1.0 < agent-explore)[ ;pick agent at random
            exchange-recommendations-with one-of agents with [who != [who] of myself]]
            [;pick best agent, only if there is someone that can provide expected utility greater than the cost of me sending a message.
            ifelse (max x > cost-message)[;ok, found the best guy
                exchange-recommendations-with (turtle position (max x) x)]
                [;if there is no greedy strategy available then fall back to reading a random document
                read-document one-of (documents with [not member? who ([documents-read] of myself)]) false]]]
end

to-report i-know-that-i-like-it? [doc-who]
    report member? doc-who (item who documents-liked)
end

to-report i-have-read-it? [doc-who]
    report member? doc-who documents-read
end

;recommend a document that this agent has read and liked
;but that agnt has *not* read. Pick randomly from among this set.
;A distributed implementation of this would require agnt to post the list of documents it has read, or tell it to this agent.
to-report get-recommendation-for [agnt]
    let possible-recs 0
  
    set possible-recs list-substract (item who documents-liked) ([documents-read] of agnt)
    if (0 = length possible-recs)[
        report -1]
    report item (random length possible-recs) possible-recs
end

;recommend a document by chosing randomly from amont the documents that this agent knows that it likes.
to-report get-random-recommendation-for [agnt]
    let possible-recs 0
  
    set possible-recs item who documents-liked
    if (0 = length possible-recs)[
        report -1]
    report item (random length possible-recs) possible-recs
end

;ask other-agent to recommend a document
;update my info on him
;read document if I have not already
to exchange-recommendations-with [other-agent]
    if (other-agent = nobody) [stop]
    ;set the rec of both me and ag-who to be the document we each recommend each other
    let the-rec 0
    ask other-agent [
      set the-rec get-recommendation-for myself]
    set rec the-rec
;    ask other-agent [set ([rec] of myself) get-recommendation-for myself]
    set the-rec get-recommendation-for other-agent
    ask other-agent [
      set rec the-rec]
;    set ([rec] of other-agent) get-recommendation-for other-agent

    ;handle the received recommendations, reading documents if neccessary.
    process-recommendation other-agent
    ask other-agent [process-recommendation myself]
    
    set utility utility - cost-message
end

;updates my history of the recommendations from other agents
;value=0 if I had already read the document recommended or there was no recommendation
;value=1 if the recommendation was for something I had not read (regardless of wether I liked it or not).
to update-recommendations [other-agent-who value]
    set recommendations replace-item other-agent-who recommendations (fput value (item other-agent-who recommendations))
    if (length item other-agent-who recommendations > 10)[
        set recommendations replace-item other-agent-who recommendations (butlast item other-agent-who recommendations)]   
end

;handle a recommendation from other-agent saying that he likes rec
to process-recommendation [other-agent]
    let other-agent-who 0
  
    set other-agent-who [who] of other-agent
    ifelse (rec = -1)[
        update-recommendations other-agent-who 0] ;treat no-rec as a rec for a document I read
        [
        set documents-liked replace-item other-agent-who documents-liked (fput rec (item other-agent-who documents-liked))
        ifelse (i-have-read-it? rec)[
            update-recommendations other-agent-who 0]
            [   
            update-recommendations other-agent-who 1
            read-document (turtle rec) true]]
    set x replace-item other-agent-who x (generate-xi-value other-agent-who)
end

;reports the utility I can expect from engaging with agent-who
;r = (#documents he recommended that I had not read)/(#documents he recommended)
;
;r *[ (|L_i n L_j| / |L_j|)*(R_r - C_r) + (1 - (|L_i n L_j| / |L_j|))*(0 - C_r) ]
to-report generate-xi-value [agent-who]
    let r 0
  let i-like 0
  let he-likes 0
  
    set r .5
    set i-like item who documents-liked
    set he-likes item agent-who documents-liked
    if (length item agent-who recommendations > 0 and length he-likes > 0) [
        set r (sum (item agent-who recommendations)) / length item agent-who recommendations
        report r * (((length disjoint i-like he-likes) / (length he-likes)) * (reward-reading - cost-reading) +
                    (1 - ((length disjoint i-like he-likes) / (length he-likes)))*(0 - cost-reading)) ]
     report -1
end

;;;
;documents member functions

;Im not using this function
to die-if-read-by-all
    if (length read-by = num-agents)[ ;I have been read by all
        ask agents [
            set documents-read remove ([who] of myself) documents-read
            set documents-liked replace-item who documents-liked (remove ([who] of myself) (item who documents-liked))]
        die]
end
@#$#@#$#@
GRAPHICS-WINDOW
250
10
561
342
150
150
1.0
1
10
1
1
1
0
1
1
1
-150
150
-150
150
0
0
1
ticks
30.0

BUTTON
4
42
85
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
1

BUTTON
86
42
167
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
1

BUTTON
168
42
249
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
1

SLIDER
4
75
176
108
num-agents
num-agents
1
100
50
1
1
NIL
HORIZONTAL

SLIDER
4
175
176
208
like-radius
like-radius
0
100
50
1
1
NIL
HORIZONTAL

SLIDER
4
208
190
241
document-explore
document-explore
0
1
0.1
0.01
1
NIL
HORIZONTAL

SLIDER
4
142
179
175
num-documents
num-documents
0
1000
1000
1
1
NIL
HORIZONTAL

SLIDER
6
282
178
315
reward-reading
reward-reading
0
10
10
0.1
1
NIL
HORIZONTAL

SLIDER
6
314
178
347
cost-reading
cost-reading
0
10
2
0.1
1
NIL
HORIZONTAL

SLIDER
6
347
178
380
cost-message
cost-message
0
10
0.1
0.1
1
NIL
HORIZONTAL

SLIDER
4
242
176
275
agent-explore
agent-explore
0
1
0.1
0.01
1
NIL
HORIZONTAL

MONITOR
561
42
618
87
gu
sum [utility] of agents
0
1
11

PLOT
561
92
761
242
utility
NIL
NIL
0.0
100.0
0.0
100.0
true
false
"" ""
PENS
"default" 1.0 0 -16777216 true "" ""

SLIDER
4
109
176
142
clustering-prob
clustering-prob
0
1
1
0.01
1
NIL
HORIZONTAL

PLOT
562
243
762
393
gain
NIL
NIL
0.0
10.0
0.0
1.0
true
false
"" ""
PENS
"default" 1.0 0 -16777216 true "" ""

@#$#@#$#@
# Distributed Recommender System  

## WHAT IS IT?

A prototype implementation of a distributed recommender system as seen in

 * Jose M. Vidal. [An Incentive-Compatible Distributed Recommendation Model](http://jmvidal.cse.sc.edu/papers/vidal03b.pdf"). In _Proceedings of the Sixth International Workshop on Trust, Privacy, Deception, and Fraud in Agent Societies_, p. 84--91, 2003.


## CREDITS

Jose M Vidal

## CHANGES 

20100623
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

sheep
false
15
Rectangle -1 true true 90 75 270 225
Circle -1 true true 15 75 150
Rectangle -16777216 true false 81 225 134 286
Rectangle -16777216 true false 180 225 238 285
Circle -16777216 true false 1 88 92

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

wolf
false
0
Rectangle -7500403 true true 15 105 105 165
Rectangle -7500403 true true 45 90 105 105
Polygon -7500403 true true 60 90 83 44 104 90
Polygon -16777216 true false 67 90 82 59 97 89
Rectangle -1 true false 48 93 59 105
Rectangle -16777216 true false 51 96 55 101
Rectangle -16777216 true false 0 121 15 135
Rectangle -16777216 true false 15 136 60 151
Polygon -1 true false 15 136 23 149 31 136
Polygon -1 true false 30 151 37 136 43 151
Rectangle -7500403 true true 105 120 263 195
Rectangle -7500403 true true 108 195 259 201
Rectangle -7500403 true true 114 201 252 210
Rectangle -7500403 true true 120 210 243 214
Rectangle -7500403 true true 115 114 255 120
Rectangle -7500403 true true 128 108 248 114
Rectangle -7500403 true true 150 105 225 108
Rectangle -7500403 true true 132 214 155 270
Rectangle -7500403 true true 110 260 132 270
Rectangle -7500403 true true 210 214 232 270
Rectangle -7500403 true true 189 260 210 270
Line -7500403 true 263 127 281 155
Line -7500403 true 281 155 281 192

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
NetLogo 5.0beta2
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
