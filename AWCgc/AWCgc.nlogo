;
; Asynchronous weak-commitment search for the graph-coloring problem
; by Ionel Muscalagiu ( mionel@fih.utt.ro )
; Jose M. Vidal


breed [ nodes ]
breed [ edges ]
breed [ weights ]


;each undirected edge goes from a to b
edges-own [a b wa-turtle wb-turtle  number-of-edges ]

;the-links list of neighbor nodes (but the-links is a list of the 'who' of all nodes that have a constraint with me)
;the-neighbors the same but as an agentset
;neighbors-list is a list of the initial neighbors nodes 
;done boolean that says if node is done or not
;domain-color-list is the list of allowed colors
;message-queue contains the incoming messages. We take new ones out from the head.
;current-view is a list indexed by node number [[color0 priority0] [color1 priority1] ...] colorl = -1 and priority = -1 if unknown.
;nogoods is a list of inconsistent colors [color0 color11 ... ] 
;messages-recieved is the number of messages this vertice has received.
;nogood_list is the list of nogood received
;nogood_sent_list is the list of nogood sent

nodes-own [the-links neighbors-list the-neighbors message-queue priority nogood_list nogood_sent_list
             colorn current-view messages-received_ok messages-received_nogood nr_constraintc  messages-received nrn]

globals [x-max y-max diam friction tot-edges filename time stoptick init-power-edges domain-color-list no-more-messages done tmp nr_cicluri nn] 

to setup-globals  ; separate procedure for setting globals
  let i 0
  
  set diam 4
  set time 0
  set stoptick -2  ; set to some number to stop, generally for image collections
  set x-max max-pxcor - (diam / 2) + 1; 0.5
  set y-max max-pycor - (diam / 2) + 1; 0.5
  set filename "" ; change to collect images (or just use command center after setup)
  
  set-default-shape nodes "circle"
  set-default-shape edges "line"
  set friction .25
  set init-power-edges 2
  set tot-edges min list round (number-of-nodes * edge-ratio) 
                         ((number-of-nodes ^ 2 - number-of-nodes) / 2)
  set domain-color-list []
  set i 0
  while [i < num-colors][
        set domain-color-list lput item i [15 105 64 125 45 85 35 55 5] domain-color-list
        set i i + 1
    ]
end


to setup  ; Setup the model for a run, build a graph.
  ;; (for this model to work with NetLogo's new plotting features,
  ;; __clear-all-and-reset-ticks should be replaced with clear-all at
  ;; the beginning of your setup procedure and reset-ticks at the end
  ;; of the procedure.)
  __clear-all-and-reset-ticks
  file-close
  clear-output
  set-default-shape weights "none"
  setup-globals
  setup-patches
  setup-turtles
  setup-random-graph
  graph-edges
 
end

to setup-patches
  ask patches [set pcolor white]
end

to setup-turtles
  create-nodes number-of-nodes [
    set color one-of domain-color-list
    ;set color item (random-int-or-float num-colors) domain
    set colorn position color domain-color-list
    set label-color black
    set size diam
    set the-links []
    set label who
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

to graph-edges  ; Make a simple edge histogram
  set-current-plot "edge-distribution"
  set-plot-x-range 1  1 + max [length the-links] of nodes 
  histogram [length the-links] of nodes
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
  if stoptick = time [stop]
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



to check-movie  ; if filename is non-empty, make another snapshot
  if length filename > 0 [
    export-view (word filename (substring "0000" (int log time 10) 3) time ".png")
  ]
end


;;;; Edge & Node utilities
to connect-edge [other-node] ; node proc: attach an edge between self and other
  hatch 1 [
    set breed edges
    set a myself
    set b other-node
    hatch 1 [
      set breed weights
      ask myself [
        set wa-turtle myself]
;      set ([wa-turtle] of myself) self
      set label -1]
    hatch 1 [
      set breed weights
      ask myself [
        set wb-turtle myself]
;      set ([wb-turtle] of myself) self
      set label -1]
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
    move-to myself]
;  set ([xcor] of wb-turtle) xcor
;  set ([ycor] of wb-turtle) ycor
  
  setxy [xcor] of a [ycor] of a
  set size distance-nowrap b - diam
  set heading towards-nowrap b
  
  fd diam / 2 + 1
  ask wa-turtle [
    move-to myself]
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

to-report copy-list [l]
  let r 0
  
  set r []
  foreach l [
    set r lput ? r]
  report r
end

;;;;;;;;;;
;;;awcs algorithm

;Initialize the awcs algorithm
to setup-awcs
  ask nodes [
    set the-neighbors nodes with [member? self ([the-links] of myself)]
    set neighbors-list []
    ask the-neighbors
    [
     set neighbors-list lput [who] of myself []
    ]
    set messages-received 0
    set messages-received_ok 0
    set messages-received_nogood 0
    set nr_constraintc 0
    set current-view make-list number-of-nodes [-1 -1]
    set priority 0 ;initial value is 0 for all.
    set nogood_list []
    set nogood_sent_list []
    set message-queue []
    set-current-plot "Messages"
    create-temporary-plot-pen (word "n-" who)
    set-plot-pen-color (who * 10 + 5) mod 140
    ]
  set nn nogood-number
  show nn
  set done false
  set nr_cicluri 0
  ask nodes [
    initialize
    set nrn 0
    update-weight-labels
    set label priority
 ]
end


to go-awcs
  set no-more-messages true
  set nr_cicluri nr_cicluri + 1
  ask nodes [
    if (not empty? message-queue)[
      set no-more-messages false]]
  if (no-more-messages) [
    WriteSolution
    stop
   ]
  if (done)
   [stop]
  ask nodes [handle-message]
  ask nodes [
    set-current-plot "Messages"
    create-temporary-plot-pen (word "q" who)
    plot messages-received_nogood + messages-received_ok
    update-weight-labels
    set label priority
   ]
end

 
to WriteSolution
ask  nodes [
show (word colorn " ")
]
end



;initialize the current-view's and start sending messages
to initialize
ask the-neighbors 
    [set current-view (replace-item ([who] of myself) current-view (list ([colorn] of myself) ([priority] of myself)) )
    set message-queue lput (list "ok?" (sentence (list ([who] of myself) ([colorn] of myself)) ([priority] of myself))) message-queue
    ]
 end

;get the next message from message-queue
to-report retrieve-message
  let msg 0
  
  without-interruption [
    set msg first message-queue
    set message-queue butfirst message-queue]
  report msg
end

;Get the msg and dispatch it to the appropiate function
to handle-message
  let msg 0
  
  if (empty? message-queue) [stop]
  set msg retrieve-message
  ;set messages-received messages-received + 1
  if (first msg = "ok?")[
    set messages-received_ok messages-received_ok + 1
    handle-ok-message item 1 msg]
  if (first msg = "nogood")[
    set messages-received_nogood messages-received_nogood + 1
    handle-nogood-message (first item 1 msg)  (last item 1 msg)]
end

;when received ok? messages
to handle-ok-message [content]
    set current-view (replace-item (first content) current-view (but-first content))
    check-agent-view
end

;when received nogood messages
to handle-nogood-message [content_colors content_priorities ]
 let i 0
  
    set nogood_list (lput content_colors nogood_list)
    set nrn nrn + 1
    set i 0
    while [i < number-of-nodes ]
     [
     if  (i != who) and (not member? turtle i the-links)  and ((item i content_colors ) != -1) 
      [
       set current-view (replace-item i current-view (list item i content_colors item i content_priorities))
       set the-links lput turtle i the-links
       set the-neighbors nodes with [member? self ([the-links] of myself)]
       ]
        set i (i + 1)
     ]
    check-agent-view
end

to-report nogood-number
let i 0
  let nmr 0
  
  set i number-of-nodes - num-colors + 1
  set nmr 1
  while [i <= number-of-nodes] 
  [
   set nmr nmr * i
   set i i + 1  
  ] 
  report nmr 
end 


;reports a list with the values from current view (without the priorities)
to-report current-view-values-only [color-value]
    let i 0
  let result 0
  
    set i 0
    set result []
    repeat (number-of-nodes)
    [
        ifelse (i = who)
            [set result (lput color-value result)]
            [set result (lput (first (item i current-view)) result)]
        set i i + 1
    ]
    report result
end

;reports a list with the priorities from current view 
to-report current-view-priorities-only [priority-value]
    let i 0
  let result 0
  
    set i 0
    set result []
    repeat (number-of-nodes)
    [
        ifelse (i = who)
            [set result (lput priority-value result)]
            [set result (lput (last (item i current-view)) result)]
        set i i + 1
    ]
    report result
end

to-report conflict [who1 who2 colorn1 colorn2]
    let result 0
  
    set result false
    set nr_constraintc nr_constraintc + 1 
    if (colorn1 = colorn2) and member? who1 [neighbors-list] of turtle who2
        [set result true]
    ;show result
    report result
end

;report true if priority p2 of agent with who2=w2 is greater than that of agent with who=w1
to-report greater-priority [w1 p1 w2 p2]
    ifelse (p2 > p1) 
        [report true]
        [
        ifelse (p2 = p1) and (w2 > w1)
                [ 
                report true]
        [report false]
        ]
end

;reports true if the value color-value is inconsistent with the current view
to-report inconsistent-view [color-value node-number]
   let i 0
  let result 0
  
   ifelse (node-number > -1) 
       [
           report conflict who node-number color-value  first (item node-number current-view); colorn-of (one-of nodes with [who = node-number])
       ]
   [
   set i 0
   set result false
   while [ i < number-of-nodes and result = false]
   [ 
      if (i != who and (greater-priority who priority i (last (item i current-view)) ) and member? i  neighbors-list )
       [
         if (conflict who i color-value first (item i current-view))
           [
               set result true
           ]
       ]
       set i i + 1
   ]
   if (member? (current-view-values-only color-value) nogood_list) [set result true]
   ]
   report result
end


;finds a value for the node that minimizes the number of 
;constraints violations with lower priority nodes
to-report findValue
   let i 0
  let temp-val 0
  let violations 0
  let temp-violations 0
  let result 0
  
   set i 0
   set temp-val 0
   set violations number-of-nodes + 1
   set temp-violations 0
   set result -1
   repeat (num-colors)
   [   
       if (temp-val != colorn)
       [
       if  not (inconsistent-view temp-val -1)  
       [
           repeat (number-of-nodes)
           [ 
               if (i != who and (greater-priority i last (item i current-view) who priority) and member? i neighbors-list )
               [
                   if (inconsistent-view temp-val i)
                   [
                       set temp-violations temp-violations + 1
                   ]
               ]
               set i i + 1
           ]
           if (temp-violations <= violations) 
           [set violations temp-violations
            set result temp-val]           
       ]
       ]
       set temp-val temp-val + 1
       set i 0
       set temp-violations 0
   ]
   ;show result
   report result
end

;sends msg to the-neighbors 
to send [msg]
    let i 0
  let aux 0
  
    ask the-neighbors [
      set message-queue lput msg message-queue 
    ]  
     
end


;does a backtrack
to backtrack
    let nogoods 0
  let priorities 0
  let msg 0
  let newValue 0
  
    set nogoods current-view-values-only colorn
    set priorities current-view-priorities-only priority
    if (nrn >= nn) 
    [
        show "no solution"
        set done true
        stop
    ]
    if not (member? nogoods nogood_sent_list)
    [        
        set msg list "nogood"  (list nogoods priorities)
        send msg
        set nogood_list (lput nogoods nogood_list)        
        set nogood_sent_list lput nogoods nogood_sent_list
        set priority (max priorities)  + 1
        set newValue findValue
        if (newValue != -1)
        [
        set colorn newValue
        set color item colorn domain-color-list
        set current-view replace-item who current-view (list colorn priority)
        set msg list "ok?" (fput who (item who current-view))
        send msg
        ]
        
    ]
end


to check-agent-view
   let newValue 0
  let msg 0
  
   if (inconsistent-view colorn -1)
   [
       set newValue findValue
       ifelse (newValue = -1) 
       [backtrack]
       [           
           set colorn newValue 
           set color item colorn domain-color-list
           set current-view replace-item who current-view (list colorn priority)
           set msg list "ok?" (fput who (item who current-view))
           send msg
       ]
   ]
end

;Use the weight agents to display what each agent belives everyone else's priorities are
to update-weight-labels
  let c-view current-view
  ask edges with [a = myself][
    ask wa-turtle[
      set label (last (item ([who] of ([b] of myself)) c-view))]]
;    set [label] of wa-turtle (last (item ([who] of b) ([current-view] of myself)))]
  ask edges with [b = myself][
    ask wb-turtle [
      set label (last (item ([who] of [a] of myself) c-view))]]
;    set [label] of wb-turtle (last (item ([who] of a) ([current-view] of myself)))]
end
@#$#@#$#@
GRAPHICS-WINDOW
207
10
624
448
19
19
10.44
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
30.0

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
1

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
1

SLIDER
6
29
193
62
number-of-nodes
number-of-nodes
2
100
17
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
0.2
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
8.75
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
4
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
3
0.1
1
NIL
HORIZONTAL

MONITOR
705
10
762
63
NIL
time
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
"" ""
PENS
"default" 1.0 1 -16777216 true "" ""

MONITOR
767
10
824
63
edges
count edges
3
1
13

BUTTON
704
60
840
93
NIL
setup-awcs
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
704
94
814
127
NIL
go-awcs
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
704
128
820
161
NIL
go-awcs
T
1
T
OBSERVER
NIL
NIL
NIL
NIL
1

TEXTBOX
684
175
868
286
1- setup\n2- Layout until it's pretty\n3- setup-awcs\n4- go-awcs
13
0.0
0

PLOT
646
262
909
448
Messages
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

SLIDER
6
139
178
172
num-colors
num-colors
0
10
3
1
1
NIL
HORIZONTAL

MONITOR
838
11
926
64
Msgs nogood
sum ([messages-received_nogood] of nodes)
3
1
13

MONITOR
933
10
992
63
Msgs ok
sum ([messages-received_ok] of nodes)  
3
1
13

MONITOR
848
64
905
117
Cycles
nr_cicluri
3
1
13

MONITOR
845
120
958
173
Constraint checks
sum ([nr_constraintc] of nodes)
3
1
13

@#$#@#$#@
# Graph Coloring using Asynchronous Weak-Commitment  
  
## WHAT IS IT?
![screenshot](AWCgc.png) 

This is the implementation of the Asynchronous Weak-Commitment Search for the graph coloring problem. We solve the graph coloring problem using the AWCS algorithm from   

 1. Makoto Yokoo and Katsutoshi Hirayama. [Algorithms for Distributed Constraint Satisfaction: A Review](http://jmvidal.cse.sc.edu/lib/yokoo00a.html). _Autonomous Agents and Multi-Agent Systems,_ 3(2):185--207, 2000.  

## CREDITS
Ionel Muscalagiu, Jose M. Vidal 

## CHANGES

20100623
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
