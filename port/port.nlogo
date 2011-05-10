breed [containers container]
containers-own [
  z-cor ;0..3, the z-coordinate of this container
  my-group ;my position
  my-stack ;my position
  my-row   ;my position
  my-truck ;the truck that wants me, or is carrying me
]
breed [cranes crane]
cranes-own [
  goal; [#ticks-to-wait "goal-position" group stack] of the goal position for the crane, or ["deliver-containter" container-who], or []
]
breed [trucks truck]
trucks-own [
  cargo ;the container that I want to get
  my-group
  my-stack
  my-start-time ;creation time of each truck
  waiting ; true if I am waiting outside the port, waiting to get in because there is another truck in my stack
]

;ticks: each tick is one second
globals [
  crane-road-xcors ; list of the x-coordinates where the crane road travels N<->S
  crane-road-ycors ; list of the y-coordinates where the crane road travels E<->W
  num-trucks-serviced ; total number of trucks served
  total-wait-time ; total wait time of all trucks that have been served
  decommitment-penalty ;
  ticks-to-rehandle
  ticks-to-deliver
  ticks-to-move 
  
] 

to setup
  ;; (for this model to work with NetLogo's new plotting features,
  ;; __clear-all-and-reset-ticks should be replaced with clear-all at
  ;; the beginning of your setup procedure and reset-ticks at the end
  ;; of the procedure.)
  __clear-all-and-reset-ticks
  ;each tick is one second, below 
  set ticks-to-rehandle 40 ; number of ticks it takes for crane to move container from one place to another in the same stack.
  set ticks-to-deliver 50 ;number of ticks it takes for crane to move container from stack to truck
  set ticks-to-move 6 ;number of ticks it takes for the crane to move to an adjacent container
  set decommitment-penalty 1000
  set crane-road-xcors (list 0 41 82)
  set crane-road-ycors (list 7 15)
  ask patches [
    set pcolor white]
  
  ;the road in which the crane travels is grey
  ask patches with [member? pycor crane-road-ycors][
    set pcolor grey]
  ask patches with [(pycor >=  item 0 crane-road-ycors and pycor <= item 1 crane-road-ycors) and (member? pxcor crane-road-xcors)][
    set pcolor grey]
  
  create-containers 1000 [
    set z-cor 0
    set shape "square"
    set color blue
    set size .6
    set my-truck nobody
    find-random-empty-position
  ]
  create-cranes 1 [
    set shape "arrow"
    set heading 0
    set color blue
    set goal []
    set-my-position position-in-yard 0 0 -1
  ]
  create-cranes 1 [
    set shape "arrow"
    set heading 0
    set color orange
    set goal []
    set-my-position position-in-yard 4 41 -1
  ]
end

to go
  if ticks >= 14400 [ stop ]  ; stop after 4800 ticks which is equal to 4 hours
  let trucks-per-tick truck-arrival / 60 ;truck-arrival is in trucks/minute, convert to trucks/second since each tick is 1 second
  create-trucks (random-poisson trucks-per-tick) [ ;Poisson arrival rate
    set shape "truck"
    set color brown
    set waiting false
    set my-start-time ticks
    set cargo one-of containers with [my-truck = nobody]
    if (cargo = nobody) [die stop] ;all stacks are full!!
    ask cargo [
      set color one-of (list red blue orange green)
    ]
    goto-container
    set my-group [my-group] of cargo
    set my-stack [my-stack] of cargo
    set label-color black
    set heading 270
    ifelse show-start-time?
    [ set label my-start-time ] ;; the label is set to be the value of the my-start-time
    [ set label "" ]     ;; the label is set to an empty text value    
    if (any? other trucks-here) [ ;someone is already here, go to the waiting spot
      setxy 0 16
      set waiting true
    ] 
    ask cargo [
      set color red
      set size 1
      set my-truck myself]
  ]
  ask cranes [go-crane]
  tick
  set-current-plot "Number of Trucks"
  plot count trucks
  set-current-plot "Number of Trucks Serviced"
  plot num-trucks-serviced
  set-current-plot "Average Wait Time"
  if num-trucks-serviced > 1 [plot total-wait-time / num-trucks-serviced]
end

to set-my-position [position-vector]
  setxy (item 0 position-vector) (item 1 position-vector)
end

;Translates group#, stack#, row# pairs into x,y coordinates in the model
;Returns the position-vector [x y] where x,y are the patch coordinates
; of group,stack,row
; group: 0..3 is the group number
; stack: 0..39 is the stack number
; row: 0..5 is the row number
to-report position-in-yard [group stack row]
  let y-pos int (group / 2)
  ifelse (y-pos = 0) [
    set y-pos 14
   ][
    set y-pos 6
  ]
  set y-pos (y-pos - row)
  let x-pos ((group mod 2) * 41 + 1) + stack
  report list x-pos y-pos
end

;========================================
;crane functions

;Gets called at every tick, makes the crane do what it needs to do.
;An opportunistic crane will re-evaluate its goal-position goal at every tick
;A non opportunistic crane picks a goal-position and sticks to it until it delivers the container to that truck.
to go-crane
  if (not empty? goal and item 0 goal != 0)[ ;not time yet, countdown
    set goal replace-item 0 goal (item 0 goal - 1)
    stop  ]
  if (empty? goal or (opportunistic? and item 1 goal = "goal-position")) [;no goal position or opportunistic, set new goal
    ifelse (any? trucks with [not waiting])[
      let goalp []
      if (crane-pick-goal-function = "random") [set goalp pick-goal-position]
      if (crane-pick-goal-function = "longest") [set goalp pick-goal-position-longest]
      if (crane-pick-goal-function = "closest") [set goalp pick-goal-position-closest]
      if (crane-pick-goal-function = "closest-longest") [set goalp pick-goal-position-closest-longest]
      if (crane-pick-goal-function = "eq-1") [
        ifelse (semi-committed? and not empty? goal) [
          set goalp pick-goal-position-eq-1-commited (list item 2 goal item 3 goal)
        ][ 
          set goalp pick-goal-position-eq-1
        ]
        ]
      if (crane-pick-goal-function = "eq-2") [set goalp pick-goal-position-eq-2]
      ifelse (goalp != nobody) [ ; if a valid group and stack values are returned
        set goal (sentence ticks-to-move "goal-position" item 0 goalp item 1 goalp)
      ][
        set goal [] ; reset goal
        stop ; crane stay put until next tick
      ]      
    ][
      stop
    ]
  ]
  if (item 1 goal = "goal-position") [ ;move towards goal-position
    let goal-position-xy position-in-yard (item 2 goal) (item 3 goal) -1
    goto-position (item 2 goal) (item 3 goal)
    if (not any? trucks-on (patch (item 0 goal-position-xy) (item 1 goal-position-xy - 7))) [ ;if there is no truck at the goal then reset goal
        set goal []
        stop
    ]
    if (item 0 goal-position-xy = xcor and item 1 goal-position-xy = ycor) [;we are at the goal, next time deliver container
      let the-truck trucks-in-this-stack
      set goal (list ticks-to-deliver "deliver-container" (item 0 [cargo] of the-truck))
    ] 
    stop
  ]
  if (item 1 goal = "deliver-container") [
    if (item 2 goal = nobody) [ ;if another cranes just delivered this container
      set goal []
      stop
    ]
    deliver-container (item 2 goal)
  ]
end

;moves the-container to the truck it belongs to, if the-container has no other containers on top of itself
;if the-container has another container on top then the top container is moved to the lowest pile in the stack
to deliver-container [the-container]
  let pile-height max ([z-cor] of containers-on the-container)
  ifelse ([z-cor] of the-container = pile-height) [ ;the-container is at the top
    let the-truck trucks-in-this-stack
    let the-containers-in-stack []
    ask the-truck [
      set the-containers-in-stack containers-in-stack 
      set total-wait-time total-wait-time + (ticks - my-start-time)
      die]
    set num-trucks-serviced num-trucks-serviced + 1
    set goal []
    ask the-container [die]
    let containers-with-truck the-containers-in-stack with [my-truck != nobody]
    if (any? containers-with-truck) [ 
      ask (one-of [my-truck] of containers-with-truck) [ ;if any trucks are waiting for this spot, pick one and mobe him here
        goto-container
        set waiting false
      ]
    ]
    stop
  ][ ;the-container is not at the top, move top container to smallest column in this stack
    let the-container-column ([ycor] of the-container - ycor)
    let other-columns remove the-container-column (list -1 -2 -3 -4 -5 -6)
    let min-column-height min map [count containers-at 0 ?] other-columns
    let min-columns filter [count containers-at 0 ? = min-column-height] other-columns
    let destination one-of min-columns     
    ;move the top container to destination
    ask max-one-of (containers-at 0 the-container-column) [z-cor] [
      move-to-position ([ycor] of myself + destination)
    ]
    ;to make the moving of each container take 3 steps uncomment the following line
    set goal (list ticks-to-rehandle "deliver-container" the-container)
  ]
end
    
to-report trucks-in-this-stack
  report trucks with [xcor = [xcor] of myself and ycor = ([ycor] of myself - 7)]
end

;Decide which truck to service
;returns the [group stack] of a random truck
to-report pick-goal-position
  let chosen-truck one-of trucks with [not waiting]
  if (chosen-truck = nobody) [
    report nobody]
  ask chosen-truck [set color yellow]
  report [group-stack] of chosen-truck
end

;returns the [group stack] of the truck that has waited the longest
to-report pick-goal-position-longest
  let chosen-truck min-one-of (trucks with [not waiting]) [my-start-time]
  if (chosen-truck = nobody) [
    report nobody]
  ask chosen-truck [set color yellow]
  report [group-stack] of chosen-truck
end

;returns the [group stack] of the truck that is closest (in terms of time, not distance) to crane
to-report pick-goal-position-closest
  let chosen-truck min-one-of (trucks with [not waiting]) [distance-to-crane myself]
  if (chosen-truck = nobody) [
    report nobody]
  ask chosen-truck [set color yellow]
  report [group-stack] of chosen-truck
end

;returns the [group stack] of the truck that are among the closest to crane, but has longest waiting time
to-report pick-goal-position-closest-longest
  let range 40 ; range of search area.  experimental variable.  will it have an affect on the truck wait time?
  let trucks-in-range trucks with [not waiting and distance-to-crane myself < range]
  let chosen-truck min-one-of trucks-in-range [my-start-time]
  if (chosen-truck = nobody) [
    report nobody]
  ask chosen-truck [set color yellow]
  report [group-stack] of chosen-truck
end

;Pick the truck that maximizes utility-eq-1  
to-report pick-goal-position-eq-1 
  let chosen-truck max-one-of (trucks with [not waiting]) [utility-eq-1 myself]
  if (chosen-truck = nobody) [
    report nobody]
  ask chosen-truck [set color yellow]
  report [group-stack] of chosen-truck
end

;Pick the truck that maximizes utility-eq-1  
; but only if its utility is higher than the one at current-goal by at least decommitment-penalty
to-report pick-goal-position-eq-1-commited [current-goal] 
  let chosen-truck max-one-of (trucks with [not waiting]) [utility-eq-1 myself]
  if (chosen-truck = nobody) [
    report current-goal]
  let utility-of-chosen-truck [utility-eq-1 myself] of chosen-truck
  
  let current-truck one-of trucks with [my-group = item 0 current-goal and my-stack = item 1 current-goal]
  if (current-truck = nobody) [
    report [group-stack] of chosen-truck]
  let utility-of-current-truck [utility-eq-1 myself] of current-truck
  
  ask current-truck [set color brown]
  ifelse (utility-of-chosen-truck > utility-of-current-truck + decommitment-penalty) [ ;is the new goal that much better than the current?
    ask chosen-truck [set color yellow]
    report [group-stack] of chosen-truck
  ][
    ask current-truck [set color yellow]
    report current-goal
  ]
end

;Pick the truck that maximizes utility-eq-2  
to-report pick-goal-position-eq-2 
  let chosen-truck max-one-of (trucks with [not waiting]) [utility-eq-2 myself]
  if (chosen-truck = nobody) [
    report nobody]
  ask chosen-truck [set color yellow]
  report [group-stack] of chosen-truck
end

;Returns a list of the patch coords the crane must follow to go from xstart,ystart to xend,yend
;Assumes that either xstart=xend or ystart=yend
;The return path omits xstart,ystart but includes xend,yend
to-report make-path [xstart ystart xend yend]
  if (xstart = xend) [
    let increment ifelse-value (ystart > yend) [1][-1]
    let result []
    let p yend
    repeat abs (ystart - yend) [
      set result fput (list xstart p) result
      set p p + increment
    ]
    report result
  ]
  if (ystart = yend) [
    let increment ifelse-value (xstart > xend) [1][-1]
    let result []
    let p xend
    repeat abs (xstart - xend) [
      set result fput (list p ystart) result
      set p p + increment
    ]
    report result
  ]
  ;both x & y coords are different
  show (word "ERROR: make-path:" xstart "," ystart "  " xend "," yend)
  report []
end

;reports the shortest path from our current xcor,ycor to goal-x,goal-y
;returns list [[x1 y1][x2 y2]....] where xi yi are the positions (patch coordinates) the crane must follow, in order.
to-report path-to-xy [goal-x goal-y]
   if (ycor = goal-y) [;I am in the same W<->E as the goal
    report make-path xcor ycor goal-x goal-y
  ]
  if (member? xcor crane-road-xcors) [ ; I am traveling N<->S
    report (sentence (make-path xcor ycor xcor goal-y) (make-path xcor goal-y goal-x goal-y))
  ]
  ;I am not in the same ycor as the goal, find shortest route
  let all-distances map [(abs (xcor - ?)) + abs (goal-x - ?)] crane-road-xcors
  let best-crossroad item (position (min all-distances) all-distances) crane-road-xcors
  let other-ycor first filter [? != ycor] crane-road-ycors
  report (sentence (make-path xcor ycor best-crossroad ycor) (make-path best-crossroad ycor best-crossroad other-ycor) (make-path best-crossroad other-ycor goal-x goal-y))
end

to-report path-to-group-stack [group stack]
  let goal-pos position-in-yard group stack -1
  let goal-x first goal-pos
  let goal-y item 1 goal-pos
  report path-to-xy goal-x goal-y
end

to-report path-to-truck [the-truck]
  report path-to-xy [xcor] of the-truck [ycor + 7] of the-truck
end


;returns the length of the minimum path to goal-x,goal-y. This function is similar to path-to-xy but does not create the path, so its faster.
to-report distance-to-xy [goal-x goal-y]
  if (ycor = goal-y) [;I am in the same W<->E as the goal
    report abs (xcor - goal-x)
  ]
  if (member? xcor crane-road-xcors) [ ; I am traveling N<->S
    report abs (ycor - goal-y) + abs (xcor - goal-x)
  ]
  ;I am not in the same ycor as the goal, find shortest route
  let all-distances map [(abs (xcor - ?)) + abs (goal-x - ?)] crane-road-xcors
  let best-crossroad item (position (min all-distances) all-distances) crane-road-xcors
  let other-ycor first filter [? != ycor] crane-road-ycors
  report abs (xcor - best-crossroad) + abs (ycor - other-ycor) + abs (best-crossroad - goal-x)
end

to-report distance-to-group-stack [group stack]
  let goal-pos position-in-yard group stack -1
  report distance-to-xy (first goal-pos) (item 1 goal-pos)
end 

to-report distance-to-truck [the-truck]
  report distance-to-xy [xcor] of the-truck [ycor + 7] of the-truck
end

;Moves at most one step towards group,stack using the shortest route.
;But, if there is a crane in the position that I want to go then I say put
to goto-position [group stack]
  let path path-to-group-stack group stack
  if (length path = 0) [stop]
  let next first path
  set heading towardsxy (first next) (item 1 next)
  if (not any? cranes-on patch-ahead 1) [
    forward 1
  ]
end

;reports true if the crane does not need to change its heading to follow path
to-report path-in-heading? [path]
  if (length path = 0) [ ;if path is empty then we don't need to change heading
    report true
  ]
  let pos first path
  report (heading = towardsxy (first pos) (item 1 pos))
end


;===========================================================
;container functions

;Tries random positions until it finds one where the container can be placed, the the container is placed there.
to find-random-empty-position
  loop [
    set my-group random 4 ;a group is the set of 40 stacks
    set my-stack random 40; 
    set my-row random 6
    set-my-position (position-in-yard my-group my-stack my-row)
    let others-here other turtles-here
    if ((not any? others-here) or count other turtles-here < 4)[ ;position works, put me here
      ifelse (not any? others-here)[
        set z-cor 0
      ][
        set z-cor 1 + max [z-cor] of other containers-here
      ]
      stop
    ]
  ] 
end

;container moves to ypos, resets his z-cor to be at the top of the new column
to move-to-position [ypos]
  set my-row my-row + (ycor - ypos)
  set ycor ypos
  ifelse (any? other containers-here) [
    set z-cor 1 + max [z-cor] of other containers-here
  ][
    set z-cor 0
  ]
end

;===========================================================
;truck functions

;move the truck to the position where it can pick up cargo (container)
to goto-container
  setxy ([xcor] of cargo) ([ycor] of cargo)
  set ycor (ycor - (6 - [my-row] of cargo))
end

;return an agentset of all the containers in the stack above me
to-report containers-in-stack
  report (turtle-set containers-at 0 1 containers-at 0 2 containers-at 0 3 containers-at 0 4 containers-at 0 5 containers-at 0 6)
end

to-report path-to-crane [the-crane]
  report [path-to-truck myself] of the-crane
end

to-report distance-to-crane [the-crane]
  report [distance-to-truck myself] of the-crane
end

to-report group-stack
  report (list my-group my-stack)
end

;utility functions: these are truck functions. Each one takes the-crane as argument and returns the utility to the crane for delivering a container to this truck

;utility eq-1: distance-based utility.  The further a truck, the lower its utility.
;utility = 0 - distance(the-crane) - 1000 (if some other crane is on the path to the-crane) - 1000 (if a turn is required)
;  "turn is required" means that the current heading of the crane is NOT the same heading required for the first move in path-to-the-crane
to-report utility-eq-1 [the-crane]
  let path-to-the-crane path-to-crane the-crane
  let other-cranes-coords [(list xcor ycor)] of (cranes with [self != the-crane])
  let other-crane-in-path? reduce [?1 or ?2] (map [member? ? path-to-the-crane] other-cranes-coords)
  let turn-required? false
  if ([ycor] of the-crane != [ycor + 7] of self) [set turn-required? true]
  let keep-heading? [path-in-heading? path-to-the-crane] of the-crane
  report 0 - distance-to-crane the-crane - ifelse-value (other-crane-in-path?)[10000][0] - ifelse-value (turn-required?)[1000][0] - ifelse-value (keep-heading?)[0][1000] 
end

;utility eq-2: time-based utility.  The longer a truck waits, the higher its utility.
;utility = truck wait time - 1000 (if some other crane is on the path to the-crane) - 1000 (if a turn is required)
to-report utility-eq-2 [the-crane]
  let path-to-the-crane path-to-crane the-crane
  let other-cranes-coords [(list xcor ycor)] of (cranes with [self != the-crane])
  let other-crane-in-path? reduce [?1 or ?2] (map [member? ? path-to-the-crane] other-cranes-coords)
  let turn-required? false
  if ([ycor] of the-crane != [ycor + 7] of self) [set turn-required? true]
  let reverse-heading? false
  if (heading = 90 and (xcor - [xcor] of the-crane) < 0) [set reverse-heading? true]
  if (heading = 270 and (xcor - [xcor] of the-crane) > 0) [set reverse-heading? true]
  report (ticks - my-start-time) - ifelse-value (other-crane-in-path?)[1000][0] - ifelse-value (turn-required?)[1000][0] - ifelse-value (reverse-heading?)[1000][0] 
end
@#$#@#$#@
GRAPHICS-WINDOW
210
10
1548
313
-1
-1
16.0
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
82
0
16
1
1
1
ticks
30.0

BUTTON
2
14
73
47
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
138
14
201
47
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

BUTTON
74
14
137
47
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

SLIDER
1
74
206
107
truck-arrival
truck-arrival
0
10
1
.1
1
trucks/minute
HORIZONTAL

PLOT
214
320
451
470
Number of Trucks
NIL
NIL
0.0
10.0
0.0
10.0
true
false
"" ""
PENS
"default" 1.0 0 -16777216 true "" ""

PLOT
483
320
716
470
Number of Trucks Serviced
NIL
NIL
0.0
10.0
0.0
10.0
true
false
"" ""
PENS
"default" 1.0 0 -955883 true "" ""

PLOT
752
319
952
469
Average Wait Time
Tick
Avg. Wait Time (ticks)
0.0
10.0
0.0
10.0
true
false
"" ""
PENS
"default" 1.0 0 -16777216 true "" ""

SWITCH
32
129
181
162
show-start-time?
show-start-time?
0
1
-1000

CHOOSER
7
184
201
229
crane-pick-goal-function
crane-pick-goal-function
"random" "longest" "closest" "closest-longest" "eq-1" "eq-2"
4

SWITCH
23
249
179
282
opportunistic?
opportunistic?
0
1
-1000

SWITCH
24
303
196
336
semi-committed?
semi-committed?
1
1
-1000

@#$#@#$#@
# Container Port Simulation  

## WHAT IS IT?
A simulation of a container port. We model the movement of the cranes using various utility functions to determine what is the best behavior for the crane operators. The trucks have random arrival times. More details can be found at   

 1. Nathan Huynh and Jose M. Vidal. [An Agent-Based Approach to Modeling Yard Cranes at Seaport Container Terminals](http://jmvidal.cse.sc.edu/papers/huynh10a.pdf). In _Proceedings of the Symposium on Theory of Modeling and Simulation_, 2010. 

 2. Jose M Vidal and Nathan Huynh. [Building Agent-Based Models of Seaport Container Terminals](http://jmvidal.cse.sc.edu/papers/vidal10a.pdf). In _Proceedings of 6th Workshop on Agents in Traffic and Transportation_, 2010. 

## CREDITS
Jose M Vidal and Nathan Huynh

## CHANGES

20110303

Last change.
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

sheep
false
0
Rectangle -7500403 true true 151 225 180 285
Rectangle -7500403 true true 47 225 75 285
Rectangle -7500403 true true 15 75 210 225
Circle -7500403 true true 135 75 150
Circle -16777216 true false 165 76 116

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
