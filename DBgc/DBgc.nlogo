breed [ nodes ]
breed [ edges ]
breed [ weights ]


;each undirected edge goes from a to b
edges-own [a b weight-a weight-b wa-turtle wb-turtle]

;the-links list of neighbor nodes
;the-neighbors the same but as an agentset
;done boolean that says if node is done or not
;message-queue is the message queue. New messages are added at the tail end.
;mode is either "wait-ok?" or "wait-improve"
;agent-view agent's beliefs about others' color. A list indexed by their 'who'.
;ok?-wait-list improve-wait-list are lists of the 'who' of my neighbors from whom I have not
;     not received an ok? or improve, respectively.
;improve-messages list of improve messages I have received.
;Instead of current-value we use 'color'.
nodes-own [the-links the-neighbors done message-queue mode agent-view ok?-wait-list improve-wait-list improve-messages my-improve new-value my-constraints messages-handled]

;color-listlist of colors to be used
globals [x-max y-max diam friction tot-edges filename stoptick init-power-edges color-list] 

to setup-globals  ; separate procedure for setting globals
  set diam 4
  set x-max max-pxcor - (diam / 2) + 1; 0.5
  set y-max max-pycor - (diam / 2) + 1; 0.5
  set filename "" ; change to collect images (or just use command center after setup)
;  set tick 0
  set stoptick -2  ; set to some number to stop, generally for image collections
  set-default-shape nodes "circle"
  set-default-shape edges "line"
  set friction .25
  set init-power-edges 2
  set tot-edges min list round (number-of-nodes * edge-ratio) 
                         ((number-of-nodes ^ 2 - number-of-nodes) / 2)
  set color-list [red green blue]
end

to setup  ; Setup the model for a run, build a graph.
  ca
  clear-output
  set-default-shape weights "none"
  setup-globals
  setup-patches
  setup-turtles
  setup-random-graph-zero-cost
  graph-edges
end

to setup-patches
  ask patches [set pcolor white]
end

to setup-turtles
  create-nodes number-of-nodes [
    set color one-of color-list
    set label-color black
    set size diam
    set the-links []
    set label who
    set done false
    set message-queue []
    setxy random-float (x-max) * (2 * (random 2) - 1)
          random-float (y-max) * (2 * (random 2) - 1)
  ]
end

to setup-power-graph  ; Build a powerlaw graph
  let n 0
  let prob 0
  let p 0
  let elist 0
  let t1 0
  let t2 0
  
  set prob (list turtle 0)
  repeat min list init-power-edges (floor number-of-nodes / 3) [
    ask last prob [connect-edge turtle (length prob)]
    set prob lput turtle (length prob) prob
  ]

  set elist n-values (number-of-nodes - length prob) [1]
  while [reduce [?1 + ?2] elist < (tot-edges - count edges)] [
      set n random length elist
      set elist replace-item n elist (1 + item n elist)
  ]
  while [length elist > 0] [
    set t1 turtle (number-of-nodes - length elist)
    set p prob
    repeat min list [who] of t1 first elist [
      set t2 one-of p
      ask t1 [connect-edge t2]
      set p remove t2 p
      set prob lput t1 prob
      set prob lput t2 prob
    ]
    set elist but-first elist
  ]
end

to setup-random-graph  ; Build a random graph
  let t 0
  let t1 0
  let g 0
  
  set g (list turtle 1)
  while [length g < number-of-nodes] [
    set t1 one-of nodes with [not member? self g]
    set t item random length g g
    ask t1 [connect-edge t]
    set g subgraph turtle 1
  ]
  while [count edges < tot-edges] [
    set t  one-of nodes
    set t1 one-of nodes with [self != t and not member? t the-links]
    if t1 != nobody [ask t1 [connect-edge t]]
  ]
end

to setup-random-graph-zero-cost  ; Build a random graph that has zero cost (no violations).
  let t 0
  let t1 0
  let g 0
  let tries 0
  
  set g (list turtle 1)
  while [length g < number-of-nodes] [
    set t item random length g g
    set t1 one-of nodes with [not member? self g and color != [color] of t]
    if (t1 != nobody) [
      ask t1 [connect-edge t]
      set g subgraph turtle 1]
  ]
  set tries number-of-nodes * 10
  while [count edges < tot-edges and tries > 0] [
    set t  one-of nodes
    set t1 one-of nodes with [self != t and not member? t the-links and color != [color] of t]
    if t1 != nobody [ask t1 [connect-edge t]]
    set tries tries - 1
  ]
  if tries = 0 [
    show "Ooops."
    ask edges [die]
    ask weights [die]
    ask nodes [
      set the-links []
      set color one-of color-list]
    setup-random-graph-zero-cost]
end

to-report subgraph [n]  ; report the complete connected subgraph containing n1
  let stack 0
  let graph 0
  
  set graph (list n)
  set stack (list n)
  while [length stack > 0] [
    foreach [the-links] of first stack [
      if not member? ? graph [
        set graph lput ? graph
        set stack lput ? stack
      ]
    ]
    set stack but-first stack
  ]
  report graph
end

; The run procedure which makes the model take one step.
; It moves the nodes so that we get a better layout. You can also click on a node and move it by hand.
to go  
  let t 0
  
  if filename = 0 [setup] ; an attempt to work even tho user forgets setup
  if stoptick = -1 [stop]
  no-display
  step
  display
  if mouse-down? [
    set t closest-xy mouse-xcor mouse-ycor nodes ;
    while [mouse-down?] [
      ask t [setxy mouse-xcor mouse-ycor]
      no-display
      ask edges with [a = t or b = t][adjust-edge]
      step
      display
    ]
  ]
  check-movie
  if (stoptick = ticks) [stop]
end

to step  ; Adjust the edges and nodes for one step of the model
  let delta 0
  
without-interruption [
  ask edges [
    set delta (spring-force * (size - spring-length)) / 2.0
    ask a [set heading towards-nowrap [b] of myself jump-nowrap delta]
    ask b [set heading towards-nowrap [a] of myself jump-nowrap delta]
  ]
  ask nodes [
    ask nodes with [self != myself] [
      set delta distance-nowrap myself
      set delta mutual-repulsion / (delta * delta)
      set heading towards-nowrap myself
      jump-nowrap (- delta)
    ]
  ]
  ask edges [adjust-edge]
]
end

to graph-edges  ; Make a simple edge histogram
  set-current-plot "edge-distribution"
  set-plot-x-range 1  1 + max [length the-links] of nodes 
  histogram [length the-links] of nodes
end

to check-movie  ; if filename is non-empty, make another snapshot
  if length filename > 0 [
    export-view (word filename (substring "0000" (int log ticks 10) 3) ticks ".png")
  ]
end

to-report total-cost
  report sum [ifelse-value ([color] of a = [color] of b) [1][0]] of edges
end

;;;; Edge & Node utilities
to connect-edge [other-node] ; node proc: attach an edge between self and other
  hatch 1 [
    set breed edges
    set a myself
    set b other-node
    set weight-a 1
    set weight-b 1
    hatch 1 [
      set breed weights
      ask myself [
        set wa-turtle myself]
;      set ([wa-turtle] of myself) self
      set label ([weight-a] of myself)]
    hatch 1 [
      set breed weights
      ask myself [
        set wb-turtle myself]
;      set ([wb-turtle] of myself) self
      set label ([weight-b] of myself)]
    ask a [set the-links lput [b] of myself the-links]
    ask b [set the-links lput [a] of myself the-links]
    set color black
    set label ""
    adjust-edge
  ]
end

to-report sign [num]
  ifelse num < 0 [report -1][report 1]
end

to-report closest-xy [x y agent-set]  ; Return closest agent to x, y
  report min-one-of agent-set [distancexy-nowrap x y]
end

to jump-nowrap [dist] ; turtle proc: jump but don't wrap, bounce w/ friction instead
  let x 0
  let y 0
  
  set x xcor + dist * dx
  set y ycor + dist * dy
  if (abs x) > x-max [set x sign x * (x-max - (1 - friction) * ((abs x) - x-max))]
  if (abs y) > y-max [set y sign y * (y-max - (1 - friction) * ((abs y) - y-max))]
  setxy x y
end

to adjust-edge ; edge proc: reattach to a & b nodes
  setxy [xcor] of b [ycor] of b
  set heading towards-nowrap a
  fd diam / 2 + 1
  ask wb-turtle [
    set xcor ([xcor] of myself)
    set ycor [ycor] of myself]
;  set ([xcor] of wb-turtle) xcor
;  set ([ycor] of wb-turtle) ycor
  
  setxy [xcor] of a [ycor] of a
  set size distance-nowrap b - diam
  set heading towards-nowrap b
  
  fd diam / 2 + 1
  ask wa-turtle [
    set xcor [xcor] of myself
    set ycor [ycor] of myself]
;  set ([xcor] of wa-turtle) xcor
;  set ([ycor] of wa-turtle) ycor
  
  setxy [xcor] of a [ycor] of a
  jump (size / 2) + (diam / 2)
end

to-report make-list [num element]
  let i 0
  let result 0
  
  set i 0
  set result []
  while [i < num] [
    set result lput element result
    set i i + 1
    ]
  report result
end

;;;;;;;;;;
;;;breakout algorithm

;Initialize the breakout algorithm
to setup-breakout
  ask nodes [
    set color one-of color-list
    set the-neighbors nodes with [member? self ([the-links] of myself)]
    set my-constraints edges with [a = myself or b = myself]
    set ok?-wait-list the-links ;;;
    set improve-wait-list the-links ;;;
    set mode "wait-ok?"
    set agent-view (make-list number-of-nodes -1) ;-1 means "I don't know".
    set agent-view replace-item who agent-view color
    set messages-handled 0
    set-current-plot "Messages"
    create-temporary-plot-pen (word "n-" who)
    set-plot-pen-color (who * 10 + 5) mod 140
;    show "ok? " + who + " " + color
    ask the-neighbors [
      set message-queue lput (list "ok?" ([who] of myself) ([color] of myself)) message-queue]
    ]
end

to go-breakout
  if (total-cost = 0) [stop] ;yes, this is cheating, but the 2000 algorithm does not have a termination condition.
  tick
  ask nodes [
    set message-queue lput "done" message-queue]
  ask nodes [handle-messages-until-done] ;or get rid of "done" message and call handle-message to do one at a time.
  set-current-plot "Cost"
  plot total-cost
  set-current-plot "Messages"
  ask nodes [
    set-current-plot-pen (word "n-" who)
    plot messages-handled]
end

;In the 2000 paper they consider one cycle to be the handling of *all* messages in the 
;queue. This seems unrealistic since agents could (and do) have varying numbers of messages.
to handle-message
  ifelse (mode = "wait-ok?")[
    handle-ok-message
    ][
    handle-improve-message
    ]
end

to handle-messages-until-done
  while [first message-queue != "done"][
    handle-message]
  set message-queue butfirst message-queue
end

to-report get-next-message [msg-type]
  let i 0
  let msg 0
  
  set i 0
  set msg []
  while [i < length message-queue][
    if (first item i message-queue = msg-type)[
      set msg item i message-queue
      set message-queue remove-item i message-queue
      set i length message-queue]
    set i i + 1]
  report msg
end
   
to handle-ok-message
  let msg 0
  
  set msg (get-next-message "ok?")
  if (empty? msg) [stop]
  set messages-handled messages-handled + 1
  set agent-view replace-item (item 1 msg) agent-view (item 2 msg)
  set ok?-wait-list remove (turtle (item 1 msg)) ok?-wait-list
  if (empty? ok?-wait-list)[
    set ok?-wait-list the-links ;;;
    send-improve
    set improve-messages []
    set mode "wait-improve"]   
end

;reports the cost for the agent using its current agent-view
to-report total-cost-local
  report sum [
    ifelse-value ((item ([who] of a) ([agent-view] of myself)) = (item ([who] of b) ([agent-view] of myself))) [
      ifelse-value (self = a) [weight-a][weight-b]][0]] of my-constraints
end

to send-improve
  let current-eval 0
  let current-color 0
  let best-cost 0
  let c 0
  
  set current-eval total-cost-local
  set best-cost 10000
  set current-color color
  foreach color-list [
    set agent-view replace-item who agent-view ?
    set c total-cost-local
    if (c < best-cost) [
      set best-cost c
      set new-value ?]
  ]
  set agent-view replace-item who agent-view current-color
  set my-improve current-eval - best-cost ;will be >= 0
;  show "improve " + who + " " + my-improve + " " + current-eval
  ask the-neighbors [
    set message-queue lput (list "improve" ([who] of myself) ([my-improve] of myself) current-eval) message-queue] ;Algorithm never uses current-eval. Bug in the paper.
end

to handle-improve-message
 let msg 0
  
 set msg (get-next-message "improve")
 if (empty? msg) [stop]
 set messages-handled messages-handled + 1
 set improve-wait-list remove (turtle (item 1 msg)) improve-wait-list
 set improve-messages fput msg improve-messages
 if (empty? improve-wait-list)[
    set improve-wait-list the-links ;;;
    send-ok
    set agent-view (make-list number-of-nodes -1)
    set agent-view replace-item who agent-view color
    set mode "wait-ok?"]   
end


to send-ok
  let max-improve 0
  
  set max-improve max map [item 2 ?] improve-messages
;  show "maxi=" + max-improve + " myi=" + my-improve
;  show "im=" + improve-messages
;  show "otherw=" + min map [item 1 ?] (filter [item 2 ? = max-improve] improve-messages)

  ;"when its improvement is largest among neighbors" actually means: (from 1996)
  ;if improvement is 0 and either my improvement is strictly larger than all neighbors or
  ;                               there is a tie but my 'who' is smallest.
  if ((my-improve > 0) 
      and
      ((my-improve = max-improve and
         who < min map [item 1 ?] (filter [item 2 ? = max-improve] improve-messages)) ;;tie, I win if my who is smaller
         or
         (my-improve > max-improve)))[ ;;my improvement is better
    show "setting color"
    set color new-value
    set agent-view replace-item who agent-view color]
 
  ;In the 1996 paper they define quasi-local minimum as: (it is not well-defined in 2000 paper):
  ; x is violating some constraint, and the possible improvements of x and all of its neighbors is 0.
  if (total-cost-local > 0 and my-improve = 0 and reduce [?1 and ?2] (map [0 = item 2 ?] improve-messages))[
    ask edges with [a = myself or b = myself] [ ;increase weights by 1.
      if ((item ([who] of a) ([agent-view] of myself)) = (item ([who] of b) ([agent-view] of myself))) [;if this edge is a constraint violation
        ifelse (a = myself) [
          set weight-a weight-a + 1
          ask wa-turtle [
            set label [weight-a] of myself ]
;          set [label] of wa-turtle weight-a
          ][
          set weight-b weight-b + 1
          ask wb-turtle [
            set label [weight-b] of myself]
          ;set [label] of wb-turtle weight-b
          ]]]]
;  show "ok? " + who + " " + color
  ask the-neighbors [
      set message-queue lput (list "ok?" ([who] of myself) ([color] of myself)) message-queue]
end
@#$#@#$#@
GRAPHICS-WINDOW
194
10
555
392
19
19
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
-19
19
-19
19
0
0
1
ticks

BUTTON
7
64
62
97
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
68
65
142
98
Layout
go
T
1
T
OBSERVER
NIL
NIL
NIL
NIL

SLIDER
6
29
193
62
number-of-nodes
number-of-nodes
2
100
20
1
1
NIL
HORIZONTAL

SLIDER
6
177
182
210
spring-force
spring-force
0
2
0.1
0.1
1
NIL
HORIZONTAL

SLIDER
6
218
182
251
spring-length
spring-length
0
10
8.5
0.25
1
NIL
HORIZONTAL

SLIDER
6
256
182
289
mutual-repulsion
mutual-repulsion
0
10
5.25
0.25
1
NIL
HORIZONTAL

SLIDER
7
103
182
136
edge-ratio
edge-ratio
0.8
5
3.2
0.1
1
NIL
HORIZONTAL

MONITOR
558
10
615
63
NIL
ticks
3
1
13

PLOT
8
294
180
414
edge-distribution
edges/node
count
1.0
1.0
0.0
1.0
true
false
PENS
"default" 1.0 1 -16777216 true

MONITOR
620
10
677
63
edges
count edges
3
1
13

MONITOR
677
10
734
63
Cost
total-cost
0
1
13

BUTTON
557
60
693
93
NIL
setup-breakout
NIL
1
T
OBSERVER
NIL
NIL
NIL
NIL

BUTTON
557
94
667
127
NIL
go-breakout
NIL
1
T
OBSERVER
NIL
NIL
NIL
NIL

BUTTON
557
128
673
161
NIL
go-breakout
T
1
T
OBSERVER
NIL
NIL
NIL
NIL

TEXTBOX
569
175
721
286
1- setup\n2- Layout until it's pretty\n3- setup-breakout\n4- go-breakout
13
0.0
0

PLOT
561
255
761
405
Cost
NIL
NIL
0.0
1.0
0.0
1.0
true
false
PENS
"default" 1.0 0 -16777216 true

PLOT
563
411
763
561
Messages
NIL
NIL
0.0
10.0
0.0
1.0
true
false
PENS
"default" 1.0 0 -16777216 true

@#$#@#$#@
Title: Graph Coloring using Distributed Breakout
Author: Jose M Vidal
Description:
An implementation of the distributed breakout algorithm from
<ul>
<li>Makoto Yokoo and Katsutoshi Hirayama. <a href="http://jmvidal.cse.sc.edu/lib/yokoo00a.html">Algorithms for Distributed Constraint Satisfaction: A Review</a>. <i>Autonomous Agents and Multi-Agent Systems,</i> 3(2):185--207, 2000. 

<li>Makoto Yokoo and Katsutoshi Hirayama. <a href="http://jmvidal.cse.sc.edu/lib/yokoo96a.html">Distributed Breakout Algorithm for Solving Distributed Constraint Satisfaction Problems</a>. In <i>Proceedings of the Second International Conference on Multiagent Systems,</i> p. 401--408, 1996.</li>
</ul>
@#$#@#$#@
default
true
0
Polygon -7500403 true true 150 5 40 250 150 205 260 250

circle
false
0
Circle -7500403 true true 35 35 230

line
true
0
Line -7500403 true 150 0 150 301

none
true
0

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
