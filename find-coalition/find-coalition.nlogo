breed [ agents agent ]
breed [ tasks task ]
;breed [ links link ]

;my-skills: Vector which contains a number (0-1) that depicts an agent's performance with a specific skill.
;           *Skills are enumerated.
;W*: list of values from other agents
;S*: list of coalitions from other agents
;joined: the coalition the agent is associated with
agents-own [ my-skills W S joined ]

;requirements: Vector of requirements for a task that identifies the need of each skill.
;              *Size determined by num-skills and enumerated in the same manner as my-skills.
;coalition-in-use: As coalitions are associated with a task, they are added to this list for record-keeping
tasks-own [ requirements coalition-in-use ]

;L: global list of coalitions
globals [ L ]

to setup
  ca
  create-agents num-agents
  create-tasks num-tasks
  setup-agents
  setup-tasks
  setup-L
end

to find-co
  foreach ([who] of tasks) [ ask agents [ find-coalition who ? ] ]
end

to setup-agents
  output-show "my-skills:"
  ask agents [ 
    set my-skills build-list-randomly num-skills 1
    set W []
    set S []
    set label who
    set joined []
    set shape "airplane"
    set size 3
    set xcor -13
    set ycor (30 / num-agents) * ceiling(who / 2) * (-1)^(who)
    output-show my-skills ]
end

to setup-tasks
  output-show "requirements:"
  ask tasks [
    set coalition-in-use []
    set label coalition-in-use
    set requirements build-list-randomly num-skills max-need
    set shape "box"
    set size 3
    set color (color - 20) ;make it darker for readability
    set xcor 13
    set ycor (30 / num-tasks) * ceiling((who - num-agents) / 2) * (-1)^(who)
    output-show requirements ]
end

to setup-L
  set L []
  ask agents [ set L lput (list who) L ] ;add the single agent sets to L
  repeat num-agents [
    ask agents [
      without-interruption [
        foreach L [ 
          if( (not member? who ?) ) [ set L lput (sort(lput who ?)) L ]
          set L remove-duplicates L] ] ] ]
end

to-report add-to-list [ i lst ]
  report lput i lst
end

to-report build-list-randomly [ sz limit ]
  ;lst: The list returned.
  let lst 0
  
  set lst []
  ifelse (limit <= 1) [ repeat sz [ set lst lput (((random (limit * 100 - 1)) + 1) / 100) lst ] ] ;need to normalize if limit is less than 1
  [ repeat sz [ set lst lput (random limit) lst ] ]
  report lst
end

;find-coalition algorithm.
to find-coalition [ agent-i task_i ]
  ;L_i: list of coalitions containing agent-i
  ;Max_i: [S_i* w_i*] retrieved after evaluating all coalitions in L_i
  ;w_max: the maximum value in W*
  ;S_max: the coalition corrisponding to w_max (W*)
  let L_i 0
  let Max_i 0
  let w_max 0
  let S_max 0
  

  set L_i coalitions agent-i

  while [ not( empty? L_i ) ] [
    if ( trace ) [
      show (word "trace> L_i: "  L_i )]
    set W []
    set S []
    set Max_i  max-coalition L_i task_i
    if( trace ) [
      show (word "trace> S*(" agent-i "): " item 0 Max_i)
      show (word "trace> w*(" agent-i "): " item 1 Max_i)]
    broadcast Max_i
    set w_max item (find-index-of-max [W] of (agent agent-i)) W
    set S_max item (position w_max W) S
    if ( trace ) [
      show (word "trace> w_max list: " W)
      show (word "trace> coalitions: " S) ]
    foreach S_max [ ;look at members of the coalition
      if( ? = agent-i ) [ 
        set joined S_max
        if ( trace ) [
          show (word "trace> Agent " agent-i " added to the coalition.")]
        create-link-with (task task_i) [ set color (((position S_max L_i) mod 7) * 10 + 35) ]
        set heading towards task task_i 
        ask task task_i [ 
          set coalition-in-use lput S_max coalition-in-use
          set coalition-in-use remove-duplicates coalition-in-use ;a coalition of n-size will add to the list n times, cleanup
          set label coalition-in-use ] ] ] ;if current agent is in the coalition, join it
    set L_i remove S_max L_i
    foreach S_max [
      let i ?
      foreach L_i [
        if( member? i ? ) [ 
          if ( trace ) [
            show (word "trace> Removing (" ? ") from L_i") ]
          set L_i remove ? L_i ] ] ] ]
end

;Fetches the coalitions that contain member i
to-report coalitions [ i ]
  ;lst: the list returned
  let lst 0
  
  set lst []
  foreach L [ if( member? i ? ) [ set lst lput ? lst ] ]
  report lst 
end

;Fetches the coalition with the highest value in a given set
;c_lst: list of possible coalitions
;task_i: current task
to-report max-coalition [ c_lst task_i ]
  ;maximum-value: v(S*)
  ;maximum-col: S*
  ;sum_vector: temporary space to hold the sum of my-skills for each agent in the coalition
  ;index: for moving through the elements in the my-skills vector
  let maximum-value 0
  let maximum-col 0
  let sum_vector 0
  let current_skills 0
  let index 0
   

  set maximum-value -1000 ;preset low

  foreach c_lst [  ;iterate through each coalition
    set sum_vector []
    repeat num-skills [ set sum_vector lput 0 sum_vector ] ;reset summation vector

    ;fetch the my-skills vector from each agent
    foreach ? [
      set index 0
      set current_skills [my-skills] of (agent ?)
      repeat num-skills [ ;sum up the values in the my-skills vectors for each agent in the current coalition
        set sum_vector replace-item index sum_vector precision ( (item index sum_vector) + (item index current_skills) ) 2
        set index ( index + 1 ) ] ]
      set index 0

    ;calculate the value for the coalition
    repeat num-skills [
      set sum_vector replace-item index sum_vector (-1 * abs( precision ( (item index [requirements] of (task task_i)) - (item index sum_vector) ) 2 ))
      set index ( index + 1 ) ]
    if ( ((sum sum_vector) / (length ?)) >= maximum-value ) [  ;check v(S)/|S| against current maximum
      set maximum-value precision ((sum sum_vector) / (length ?)) 2
      set maximum-col ? ] ] 

    report list maximum-col maximum-value
end

to broadcast [ pair ]
  ask agents [ 
    set W lput (item 1 pair) W
    set S lput (item 0 pair) S ]
end

to-report find-index-of-max [ lst ]
  let index 0
  let t 0
  
  set t -1000
  
  foreach lst [
    if( ? > t ) [
      set t ?
      set index (position ? lst) ] ]
  
  report index
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
0
0
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
12
10
184
43
num-skills
num-skills
1
15
4
1
1
NIL
HORIZONTAL

SLIDER
12
76
184
109
num-agents
num-agents
1
15
4
1
1
NIL
HORIZONTAL

SLIDER
12
43
184
76
num-tasks
num-tasks
1
15
5
1
1
NIL
HORIZONTAL

BUTTON
11
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

OUTPUT
749
10
1133
255
12

SLIDER
202
10
294
43
max-need
max-need
0
7
3
1
1
NIL
HORIZONTAL

BUTTON
79
116
184
149
Find Coalition
find-co
NIL
1
T
OBSERVER
NIL
NIL
NIL
NIL

SWITCH
88
150
178
183
trace
trace
0
1
-1000

@#$#@#$#@
Title: Task Allocation
Author: William Wiles
Description:

Solves a task allocation problem with the coalition formation algorithm from
<ul><li>Onn Shehory and Sarit Kraus. <a href="http://jmvidal.cse.sc.edu/lib/shehory98a.html">Methods for Task Allocation via Agent Coalition Formation</a>. <i>Artificial Intelligence,</i> May; 101(1-2):165--200, 1998.</li></ul>

------

HOW IT WORKS
------------
From the problem set description,

There are num-skills (slider) available to the agents. Each agent (there are num-agents of them) is born with a my-skills vector which contains a number between 0 a 1 for each one of these skills. These vectors represent how well the agent can perform each one of the skills.

There is also a breed of tasks, of which there are num-tasks. Each task has a requirements variable which is a vector of size num-skill where each entry is a number randomly chosen between 0 and max-need. 

L is the list of all possible coalitions, and is found by each state adding the possible combinations of itself to the current sets in L, while pruning duplicates.  Since each member of a coalition will add itself, the number of duplicates increases linearly as the coalition size increases.  However, the code is direct and short.


Once this initialization takes place, the v(S) is calculated by each agent for its related coalitions, i.e. all possible coalitions it can be involved with, against each task.  The v(S) are calculated during the find-coalition implementation instead of beforehand to cut down on global variables, since message-passing will take care of any agent's information needs.

find-coalition is executed after setup.

HOW TO USE IT
-------------
To visualize the algorithm, links indicate an association between an agent and a task.  Tasks are in column formation on the right (Q1/Q4) hemisphere of the world; while, agents are arranged similarly on the left (Q2/Q3) hemisphere of the world.  

Agents in the same coalition for a task will have the same color link connecting them to that specific task.

The requirements and my-skills vectors are displayed in the output window to the right of the world.  

The tasks show their coalition solution as their label.  The agents show their ID.

The trace toggle will allow you to view the find-coalition algorithm in a step-by-step basis.  Expect alot of output for more than 3 agents or tasks.

THINGS TO NOTICE
----------------
If the max-need of the tasks is set above the number of agents, the problem becomes super-additive.


THINGS TO TRY
-------------
Setting the max-need above the number of agents.

Setting the mas-need to zero.


EXTENDING THE MODEL
-------------------
Performance analysis of the L constructor and find-coalition's max-arg (max-coalition in the implementation) for large agentsets.  Possible future pruning and optimization in these areas


CREDITS AND REFERENCES
----------------------
Problem Set Description:
http://jmvidal.cse.sc.edu/csce782/index.html

Fundamentals of Multiagent Systems:
http://jmvidal.cse.sc.edu/library/vidalfmas.pdf

Netlogo Handbook:
http://ccl.northwestern.edu/netlogo/docs/
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
