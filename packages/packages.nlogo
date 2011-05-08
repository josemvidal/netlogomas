; package delivery problem
; by
; Jose M. Vidal

breed [ deliverators ]
 
;"The Deliverator stands tall, your pie in thirty minutes or you can have it free, 
; shoot the driver, take his car, file a class-action suit."--Snow Crash.

deliverators-own [dist deliveries steps balance average-cost accepted-package packages helping behavior]
;dist is the total distance the agent has travelled
;deliveries are the set of deliveries left. They are given in [spoke distance] pairs.
;steps is used to keep track of the steps left towards the goal
;balance is a list of the balances (B in the paper). The i'th element refers to who-of agent = i.
;behavior is
; 0 - philanthropic
; 1 - selfish
; 2 - reciprocative
; 3 - individual
;
patches-own [name]

globals [max-distance delivery-distances spoke-mult distance-mult num-asks-multiplier average-total-cost average-total-dist num-deliverators]

;num-deliveries- deliveries are referred to as "tasks" in the paper.
;num-spokes- is the variable "R" in the paper


to setup
  ca
  initialize
end

to initialize
  set max-distance 3 ;This is variable "D" in the paper
  set spoke-mult 360 / num-spokes 
  set distance-mult 5
  set num-asks-multiplier 10
  set num-deliverators num-philanthropic + num-selfish + num-reciprocative + num-individual
  create-deliverators num-philanthropic + num-selfish + num-reciprocative + num-individual [
    ifelse (who < num-philanthropic) [
      set behavior 0 ]
    [
      ifelse (who < num-philanthropic + num-selfish) [
        set behavior 1 ]
      [
        ifelse (who < num-philanthropic + num-selfish + num-reciprocative)[
          set behavior 2]
        [
          set behavior 3]]]]       
  set delivery-distances make-list num-deliveries
  ask deliverators [
    set-deliveries
    set heading 0]
  ask (patch 0 0) [set name "depot"]
  set-plot-pen-interval 1
  print "Setup done."
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

;Makes a list of len items by cycling over the possible distances of delivery.
;We assume only integer distances are allowed.
to-report make-list [len]
  let i 0
  let lst 0
  let c 0
  
  set lst []
  set i 0
  set c round (max-distance / 2)
  while [i < len] [
    set lst fput c lst
    set i i + 1
    set c (c + 1) mod max-distance + 1]
  report lst
end

to-report make-deliveries
  let i 0
  let res 0
  
  set res []
  while [i < length delivery-distances][
    set res fput list (random num-spokes) (item i delivery-distances) res
    set i i + 1]
  report randomize-list res
end


;agents in the depot exchange tasks
;I had to do this using a double loop because the parallelism introduced synchronization problems.
;without-interruption is not good enough to solve this.
to exchange-tasks
  let i 0
  let j 0
  let owners 0
  let traders 0
  

  ask deliverators with [[name] of patch-here = "depot" and not empty? deliveries][
    set accepted-package false ;true if agent has accepted a package from someone. If so, he will not try to trade his package away
    set packages fput (item 1 first deliveries) [] ;A list of distances for the packages Im delivering, the first is my original one
    set helping [] ] ;A list of who-of for the agents for whom agent has agreed to take a package
  
  set owners [who] of deliverators with [[name] of patch-here = "depot" and 
                                            (not empty? deliveries) ]
  set owners randomize-list owners
  while [not empty? owners][
    set i (turtle first owners)
    if (not [accepted-package] of i and ;If i has not accepted a package then it can still try to give his away.
        [behavior] of i != 3)[ ;individual agents don't ask for help
      set traders [who] of deliverators with [[name] of patch-here = "depot" and
                                                 not empty? deliveries and
                                                 who != [who] of i and
                                                 first first deliveries = first first [deliveries] of i]
      while [not empty? traders][
        set j (turtle first traders)
        ask i [
          try-to-give-next-task-to-agent j]
        ifelse (empty? [packages] of i)[ ;if I gave it then Im done
          set traders [] ]
          [
          set traders butfirst traders]
      ]
    ]
    ask i [
      if (not empty? packages) [
        set steps max packages
        set heading (first first deliveries) * spoke-mult
        set deliveries but-first deliveries]]
    set owners butfirst owners
  ]
end


to update
  ;turn of the infinity button if set
  if (count deliverators with [empty? deliveries] = num-deliverators and
      count deliverators with [[name] of patch-here = "depot"] = num-deliverators) 
    [stop]

  ;agents in the depot
  exchange-tasks
  
  ;Ask all deliverators to move one step, in parallel
  ask deliverators [
    deliver]
    
  ;Calculate average distance travelled
  set average-total-dist mean [dist] of deliverators
  plot average-total-dist
end

to evolve
  ct cp
  initialize
  while [count deliverators with [empty? deliveries] != num-deliverators or
         count deliverators with [[name] of patch-here = "depot"] != num-deliverators][
    update]
  ask deliverators [evolve-behavior]
  set num-philanthropic count deliverators with [behavior = 0]
  set num-selfish count deliverators with [behavior = 1]
  set num-reciprocative count deliverators with [behavior = 2]
  set num-individual count deliverators with [behavior = 3]
end

to do-plot-deliveries
  let inc 0
  
  set inc 10
  set-current-plot "plot1"
  clear-plot
  set num-deliveries 10
  repeat 25 [
    ct cp
    initialize
    while [count deliverators with [empty? deliveries] != num-deliverators or
           count deliverators with [[name] of patch-here = "depot"] != num-deliverators][
           update]
    set-current-plot "plot1"
    type num-deliveries
    type " - "
    print average-total-dist
    plot average-total-dist
    set num-deliveries num-deliveries + inc]
end
  
to do-plot-agents
  let inc 0
  
  set inc 10
  set-current-plot "plot1"
  clear-plot
  set num-reciprocative 10
  set num-philanthropic 0
  set num-selfish 0
  set num-individual 0
  repeat 25 [
    ct cp
    initialize
    while [count deliverators with [empty? deliveries] != num-deliverators or
           count deliverators with [[name] of patch-here = "depot"] != num-deliverators][
           update]
    set-current-plot "plot1"
    type num-reciprocative
    type " - "
    print average-total-dist
    plot average-total-dist
    set num-reciprocative num-reciprocative + inc
    set num-deliverators num-reciprocative]
end

to-report get-costs-from [delivrs]
  let res 0
  
  set res []
  while [not empty? delivrs][
    set res lput (item 1 first delivrs * 2) res
    set delivrs butfirst delivrs]
  report res
end

;Report the extra cost that agnt will incurr if it accepts task, given its current task (first deliverables)
;This method is explained in the 1996 paper.
to-report extra-cost [agnt task]
  if (first first [deliveries] of agnt != first task)[
    report 0] ;only report cost if they are both on the same fin
  ifelse (item 1 task <= item 1 first [deliveries] of agnt)[ 
    report item 1 task] ;if task is closer than mine then cost is just cost to get there
    [
    report 2 * item 1 task - item 1 first [deliveries] of agnt] ;if task is farther than mine
end


;;;;;;
; Member functions for the Deliverators.
;;;;;;

to set-deliveries
  let i 0
  
  set deliveries make-deliveries
  set average-cost mean get-costs-from deliveries
  set steps 0
  set balance []
  while [i < num-deliverators][
    set i i + 1
    set balance lput 0 balance]
end

;returns true if I am willing to take the next task from ag
;It implements eq.1 from the paper
to-report willing-to-take-next-task-from [ag]
  let the-extra-cost 0
  let his-task 0
  let prob 0
   ; C_{ij}^{kl} from the paper
  report true ;philantropic behavior
  set his-task first [deliveries] of ag
  set the-extra-cost max list 0.0 (item 1 his-task - item 1 first deliveries)
  set prob 1.0 / (1.0 + exp ((the-extra-cost - beta * average-cost - item ([who] of ag) balance)/ tau))
;  type "Prob of "
;  type who
;  type " taking from "
;  type who-of ag
;  type " is "
;  print prob
  if (random-float 1.0 <= prob)[
    report true]
  report false
end

to-report reciprocating-agent-accepts-my-next-task? [ag]
  let his-extra-cost 0
  let his-task 0
  let prob 0
   ; C_{ij}^{kl} from the paper
  set his-task first [deliveries] of ag
  set his-extra-cost extra-cost ag first deliveries
  set prob 1.0 / (1.0 + exp ((his-extra-cost - beta * [average-cost] of ag - item who ([balance] of ag))/ tau))
  if (random-float 1.0 <= prob)[
    report true]
  report false
end    

to give-my-next-task-to-agent [to-agent]
  let his-extra-cost 0
  let task 0
  
;  type who
;  type " giving package to "
;  print who-of to-agent
  set task first deliveries ;the task I am giving to-agent
  set his-extra-cost extra-cost to-agent task
  ask to-agent [
    set packages lput (item 1 task) packages
    set accepted-package true
    set helping lput ([who] of myself) helping
    set balance replace-item ([who] of myself) balance
                                      ((item ([who] of myself) balance) - his-extra-cost)
  ]
;  set ([packages] of to-agent) lput (item 1 task) ([packages] of to-agent)
;  type "packages ="
;  print (packages-of to-agent)
;  set ([accepted-package] of to-agent) true
;  set ([helping] of to-agent) lput who ([helping] of to-agent)
;  set ([balance] of to-agent) replace-item who ([balance] of to-agent) 
;                                      ((item who [balance] of to-agent) - his-extra-cost)
  set balance replace-item ([who] of to-agent) balance
                                      ((item ([who] of to-agent) balance) + (2 * item 1 task)) ;update my balance of him..nice guy.
  set deliveries butfirst deliveries
  set packages []
end

;Assumes that the next task for me and ag is on the same spoke.
to try-to-give-next-task-to-agent [ag]
 let my-cost 0
  let his-cost 0
  
 if (empty? [packages] of ag) [stop] ;if ag already gave away its package then we will not give it to him
 if ([behavior] of ag = 1 or [behavior] of ag = 3)[ ;selfish or individuals do not accept packages
   stop]
 set my-cost 2 * item 1 first deliveries
 if (my-cost > extra-cost ag first deliveries)[ ;as per the 1996 paper
   ifelse ([behavior] of ag = 0)[ ;philanthropic
     give-my-next-task-to-agent ag]
   [ ;reciprocating
     if (reciprocating-agent-accepts-my-next-task? ag)[
       give-my-next-task-to-agent ag]]]
end

to deliver
  let willing-to-help 0
  
  if (empty? packages and [name] of patch-here = "depot") [ ;nothing to deliver
    stop] 
  if (steps = 0)[ ;at delivery site, turn around
    set heading heading + 180
    set packages [] ]
  fd distance-mult
  set dist dist + 1
  set steps steps - 1
end

to evolve-behavior
  let distances 0
  let ap 0
  let randa 0
  let randb 0
  let a 0
  let b 0
  let total-dist 0
  
  set total-dist sum [dist] of deliverators
  set distances  [list who dist] of deliverators 
  set randa random-float 1.0
  set randb random-float 1.0
  set ap 0
  while [not empty? distances][
    set ap ap + (item 1 first distances / total-dist)
    if (randa < ap) [
      set a first distances
      set randa 10]
    if (randb < ap)[
      set b first distances
      set randb 10]
    set distances butfirst distances
  ]
  ifelse (1.0 / item 1 a) > (1.0 / item 1 b) [
    set behavior [behavior] of (turtle first a) ]
  [
    set behavior [behavior] of (turtle first b) ]    
end

@#$#@#$#@
GRAPHICS-WINDOW
256
10
581
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

BUTTON
2
49
83
82
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
49
165
82
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
166
49
247
82
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
2
214
174
247
num-deliveries
num-deliveries
0
500
50
1
1
NIL
HORIZONTAL

SLIDER
2
247
174
280
num-spokes
num-spokes
1
20
4
1
1
NIL
HORIZONTAL

SLIDER
2
280
174
313
tau
tau
0
1
0.75
0.01
1
NIL
HORIZONTAL

SLIDER
2
313
174
346
beta
beta
0
1
0.5
0.01
1
NIL
HORIZONTAL

MONITOR
584
16
719
61
NIL
average-total-dist
3
1
11

PLOT
584
68
784
218
plot1
x
dist
0.0
100.0
0.0
100.0
true
false
PENS
"default" 1.0 0 -16777216 true

BUTTON
2
346
126
379
Plot deliveries
do-plot-deliveries
NIL
1
T
OBSERVER
NIL
NIL
NIL
NIL

BUTTON
126
346
230
379
Plot agents
do-plot-agents
NIL
1
T
OBSERVER
NIL
NIL
NIL
NIL

SLIDER
2
115
182
148
num-philanthropic
num-philanthropic
0
100
0
1
1
NIL
HORIZONTAL

SLIDER
2
82
174
115
num-selfish
num-selfish
0
100
0
1
1
NIL
HORIZONTAL

SLIDER
2
148
181
181
num-reciprocative
num-reciprocative
0
100
40
1
1
NIL
HORIZONTAL

SLIDER
2
181
174
214
num-individual
num-individual
0
100
0
1
1
NIL
HORIZONTAL

BUTTON
174
214
255
247
NIL
evolve
NIL
1
T
OBSERVER
NIL
NIL
NIL
NIL

BUTTON
174
246
255
279
NIL
evolve
T
1
T
OBSERVER
NIL
NIL
NIL
NIL

@#$#@#$#@
Title: Reciprocity in Package Delivery
Author: Jose M Vidal
Description:
In the package delivery problem a the agents must deliver a set 
of packages from a central depot. The destinations lie along one of several spokes
emaniting from the central depot. The agents can exchange packages only at the depot.
The cost to an agent is proportional to the number of packages carried.<br/>
The optimal solution to this problem is to choose one agent to go on each
spoke and deliver all the packages scheduled for it. However, this would 
require completely selfless agents. In this program we study the dynamics that
emerege when using different
populations of selfish, philantropic, reciprocal, and individual agents behave.
These results were first presented in
<ul>
<li>
Sandip Sen. 
<a href="http://jmvidal.cse.sc.edu/lib/sen96a.html">
Reciprocity: a foundational principle for promoting cooperative behavior among self-interested agents.</a>
In Proceedings of the Second International Conference on Multiagent Systems,  p. 322--329, AAAI Press, Menlo Park, CA. 1996.
</li>
<li>
Sandip Sen and Partha Sarathi Dutta. 
<a href="http://jmvidal.cse.sc.edu/lib/sen02a.html">
The evolution and stability of cooperative traits</a>. I
n Proceedings of the First Intenational Joint Conference on Autonomous Agents and Multiagent Systems, pages 1114-1120. ACM Press, NY, NY, 2002.
</li>
<li>
Sandip Sen. 
<a  href="http://jmvidal.cse.sc.edu/lib/sen02b.html">
Believing others: Pros and cons</a>. Artificial Intelligence, 142(2):179-203, December 2002.
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

link
true
0
Line -7500403 true 150 0 150 300

link direction
true
0
Line -7500403 true 150 150 30 225
Line -7500403 true 150 150 270 225

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
