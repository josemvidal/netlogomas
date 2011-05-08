;ABTkernel for the graph-coloring problem
; by Ionel Muscalagiu ( mionel@fih.utt.ro )
; Jose M. Vidal

breed [ nodes ]
breed [ edges ]
breed [ weights ]


;each undirected edge goes from a to b
edges-own [a b weight-a weight-b wa-turtle wb-turtle  number-of-edges ]

;the-links list of neighbor nodes (but links is a list of the 'who' of all nodes that have a constraint with me)
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
             ChildrenA ParentA  MyValue_colorn MyContext nogoods messages-received_ok messages-received_nogood nr_constraintc 
             AgentC_Cost agent_nogood messages-received messages-received_nogoodold ]

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
  ca
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
    set MyValue_colorn position color domain-color-list
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
    set weight-a 1
    set weight-b 1
    hatch 1 [
      set breed weights
      set ([wa-turtle] of myself) self
      set label ([weight-a] of myself)]
    hatch 1 [
      set breed weights
      set ([wb-turtle] of myself) self
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
  set ([xcor] of wb-turtle) xcor
  set ([ycor] of wb-turtle) ycor
  
  setxy [xcor] of a [ycor] of a
  set size distance-nowrap b - diam
  set heading towards-nowrap b
  
  fd diam / 2 + 1
  set ([xcor] of wa-turtle) xcor
  set ([ycor] of wa-turtle) ycor
  
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

; n is length of list
; el is the element
to-report get-list [n el]
  let i 0
  let lst 0
  
  set i 0
  set lst []
  while [i < n] [
    set lst fput el lst
    set i i + 1]
  report lst
end
;;;;;;;;;;
;;;ABTkernel algorithm

;Initialize the ABT kernel algorithm
to setup-ABTkernel

  ask nodes [
    set the-neighbors nodes with [member? self ([the-links] of myself)]
    
    set neighbors-list []
    ask  the-neighbors
    [
     set neighbors-list lput [who] of myself neighbors-list
    ]
    set messages-received 0
    set messages-received_ok 0
    set messages-received_nogood 0
    set messages-received_nogoodold 0
    set nr_constraintc 0
    set AgentC_Cost 0
    set MyContext get-list number-of-nodes -1
    set nogoods get-list number-of-nodes 0
    set message-queue []
    set-current-plot "Messages"
    create-temporary-plot-pen (word "n-" who)
    set-plot-pen-color (who * 10 + 5) mod 140
    ]
  ask nodes [
  ComputeParentA_ChildrenA
  initialize
  ]
  set done false
  set nr_cicluri 0
 
end

to ComputeParentA_ChildrenA
let i 0
  
set ChildrenA []
set ParentA []
;show the-neighbors
foreach neighbors-list
[
  if (? > who )  
    [set ChildrenA lput ? ChildrenA]
  if (? < who )  
    [set ParentA lput ? ParentA]
]
end

to WriteSolution
ask nodes [
show (word MyValue_colorn " ")
]
end

to go-ABTkernel
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
   ]
 
end

to initialize
 let msg 0
  
   foreach childrenA
    [
     set msg (list "info" (list who MyValue_colorn) AgentC_Cost)
     ask turtles with [who = ? ]
         [receive-message msg]
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

to receive-message [msg]
  without-interruption [
    set message-queue lput msg message-queue]
end

;this is the ABTkernel procedure in the paper
to handle-message
  let msg 0
  let xj 0
  let dj 0
  let SenderC_Cost 0
  
  if (empty? message-queue) [stop]
  set msg retrieve-message
  ;show msg
  if (first msg = "stop")
   [set done true
   stop]
  if (first msg = "info")
    [set messages-received_ok messages-received_ok + 1
    ProcessInfo msg
    ]
  if (first msg = "back")
    [ 
    set messages-received_nogood messages-received_nogood + 1
    set SenderC_Cost item 3 msg
    if SenderC_Cost > AgentC_Cost
    [ 
      set AgentC_Cost SenderC_Cost
    ]
    ResolveConflict item 1 msg item 2 msg
    
    ] 
end

 to ProcessInfo [msg ]
   let xj 0
  let dj 0
  let SenderC_Cost 0
  
   set xj item 0 (item 1 msg)
   set dj item 1 (item 1 msg)
   set SenderC_Cost item 2 msg
   if SenderC_Cost > AgentC_Cost
   [ 
      set AgentC_Cost SenderC_Cost
    ]
   UpdateContextInfo xj dj
   CheckAgentView  
 end
 
   ;this is the CheckAgentView procedure in the ABTkernel algorithm  
 to CheckAgentView
  let TempValue 0
  let msg 0
   
    if (not Is-Consistent);1  MyValue_colorn )
       [
       set TempValue ChooseValue
       ifelse (TempValue > -1 )
        [ 
        set MyValue_colorn TempValue
        set color item MyValue_colorn domain-color-list
        foreach childrenA
         [
          set msg (list "info" (list who MyValue_colorn) AgentC_Cost)
          ask turtles with [who = ? ]
              [receive-message msg]
         ]
         ]
         [Backtrack]
      ]
  end       

to CheckAgentView1
  let TempValue 0
  let msg 0
   
    
       set TempValue ChooseValue
       ifelse (TempValue > -1 )
        [ 
        set MyValue_colorn TempValue
        set color item MyValue_colorn domain-color-list
        foreach childrenA
         [
          set msg (list "info" (list who MyValue_colorn) AgentC_Cost)
          ask turtles with [who = ? ]
              [receive-message msg]
         ]
         ]
         [Backtrack]
      
  end       
to UpdateContextInfo [Qj Vj ]
 let i 0
  let j 0
  
    set MyContext replace-item Qj MyContext Vj
    ;set i Qj 
    set nogoods get-list num-colors 0
    set i 0
    while [ i < who ] ;for each vertice
        [set j 0
         while [ j < num-colors ] ;for each color 
            [
            set nr_constraintc nr_constraintc + 1
            set AgentC_Cost AgentC_Cost + 1
             if ((neighbors? i who) and ( j = (item i MyContext)) and (item i MyContext != -1 ))
                [set nogoods replace-item j nogoods 1]   
             set j (j + 1)]
         set i (i + 1)]    
      
end

to-report Is-Consistent
    let i 0
  let consistent 0
  
    set consistent true
    if (item  MyValue_colorn nogoods = 1)
     [ 
      set consistent false
     ]   
    report consistent               
end

to-report Is-Consistent1 [value_colorn]
    let i 0
  let consistent 0
  
    set consistent true
    set i 0
    while [ i < who  ] ;for each vertice
    [
     set nr_constraintc nr_constraintc + 1
     set AgentC_Cost AgentC_Cost + 1
      if ((item i MyContext != -1 ) and (neighbors? i who) and ( Value_colorn = (item i MyContext)))
             [ 
             set consistent false
             ]   
      set i i + 1
     
    ]
    report consistent               
end
;this is the Coherent function in the ABTkernel algorithm    
to-report Is-obsolete [msgNogood Sender]
    let i 0
  let res 0
  
    set i 0
    set res false
    while [i < who]
     [if  ((item i MsgNogood != -1 ) and (item i MyContext != -1 ) and (item i msgNogood) != (item i MyContext) and member? i ParentA) 
    ; [if  ((item i msgNogood) != (item i MyContext) and member? i ParentA) 
     [set res true]            
      set i (i + 1)]
    if ( (item who MsgNogood != -1 ) and ((item who msgNogood) != MyValue_colorn))
    ;if ( ((item who msgNogood) != MyValue_colorn))
      [set res true]
   report res  
end


to ResolveConflict [msgNogood Sender]
    let i 0
  let j 0
  let msg 0
  let TempValue 0
  
   
   
    ;if it is consistent with the agent view, otherwise it is discarded due to obsolescence.
   ifelse ( Not Is-obsolete msgNogood Sender)
    ;An accepted nogood is used to update the agent view of agents not in PArentA
    ;Update MyContext
    [set i 0
    while [i <  who]
     [
     if (((item i msgNogood) != -1) and (not member? i ParentA)) 
       [set MyContext (replace-item i MyContext (item i msgNogood))]            
     set i (i + 1)
     ]
    ;UpdateContextNogood
    ;The nogood is stored
    set nogoods replace-item MyValue_colorn nogoods 1 ;mark current position as nogood
    
    CheckAgentView1
   ]
    [ set messages-received_nogoodold messages-received_nogoodold + 1
      if (member? Sender childrenA) and  ( (item who msgNogood = MyValue_colorn)); or (item who MsgNogood != -1 ))
      [ 
         ;show "se trimite lui " + Sender
         set msg (list "info" (list who MyValue_colorn) AgentC_Cost)
         ask turtles with [who = Sender ]
            [receive-message msg]
       ]
     ]
     
 end 

to-report agents-in-context [cont]
    let i 0
  let res 0
  
    set i 0
    set res []
    
    while [i < who]
    [
      
        if (item i cont != -1) and (item (item i cont) nogoods) = 1; and member? i parentA 
        [
           set res lput i res
        ]
        set i i + 1
    ]
    report res
end


;;this is the BackTrack procedure in the ABT algorithm    
 to BackTrack
    let msg 0
  let i 0
  let j 0
  let wng 0
  let m 0
  let newNogood 0
  let LAgents 0
   
    set LAgents agents-in-context MyContext    
    ifelse (who = 0) or empty? LAgents ;newNogood=empty; 
        [set done true 
        set msg "stop"
        receive-message msg
        stop
        ]
       [ 
         set wng last Lagents
         set newNogood MyContext
         set m item wng MyContext
         set i 0
         while [i < who ]
           [ if (item i newNogood != -1) and (item (item i newNogood) nogoods = 0 ) 
              [
                set newNogood (replace-item i newNogood -1)  ]
            set i i + 1
           ]  
         set msg (list "back" newNogood who AgentC_Cost )     
         ask turtles with [who = wng ]
            [receive-message msg]
         set MyContext (replace-item wng MyContext -1)
         UpdateContextConflict 
         UpdateContextConflict1 m 
         CheckAgentView1
        ] 
end


to UpdateContextConflict1 [culors] 
   let i 0
  let j 0
  let msg 0
  let TempValue 0
  
   set nogoods replace-item culors nogoods 0
   set i 0
   while [ i < who ] ;for each vertice
        [ set nr_constraintc nr_constraintc + 1
          set AgentC_Cost AgentC_Cost + 1
          if ((neighbors? i who) and ( culors = (item i MyContext)) and (item i MyContext != -1 ))
                [set nogoods replace-item culors nogoods 1]   
          set i (i + 1)
        ]    
end

to UpdateContextNogood 
 let i 0
  let j 0
  

    set nogoods get-list num-colors 0
    set i 0
    while [ i < who ] ;for each vertice
        [set j 0
         while [ j < num-colors ] ;for each color 
            [set nr_constraintc nr_constraintc + 1
             if ((neighbors? i who) and ( j = (item i MyContext)) and (item i MyContext != -1 ))
                [set nogoods replace-item j nogoods 1]   
             set j (j + 1)]
         set i (i + 1)]    
      
end
 
 to UpdateContextConflict 
 let i 0
  let j 0
  

    set nogoods get-list num-colors 0
    set i 0
    while [ i < who ] ;for each vertice
        [set j 0
         while [ j < num-colors ] ;for each color 
            [set nr_constraintc nr_constraintc + 1
             set AgentC_Cost AgentC_Cost + 1            
             if ((neighbors? i who) and ( j = (item i MyContext)) and (item i MyContext != -1 ))
                [set nogoods replace-item j nogoods 1]   
             set j (j + 1)]
         set i (i + 1)]    
      
end

to-report ChooseValue
 let i 0
  let j 0
  
   set i 0
   set j 0
   while [ i < num-colors and j = 0]
        [if ( item i nogoods = 0 )
            [
             ifelse Is-Consistent1 i 
             [set j 1
             report i
             ]
             [
             set nogoods replace-item i nogoods 1 
             ]
             ]
         set i (i + 1)]
  ; setxy (i - screen-edge-x) (screen-edge-y - who)
   report -1      
end 


to-report ChooseValue1
 let i 0
  let j 0
  
   set i 0
   set j 0
   while [ i < num-colors and j = 0]
        [if ( item i nogoods = 0 )
            [
             set j 1
             report i
             ]
          set i (i + 1)]
  ; setxy (i - screen-edge-x) (screen-edge-y - who)
   report -1      
end 


to-report neighbors? [v1 v2]
     report (member? v2 [neighbors-list] of turtle v1) or (member? v1 [neighbors-list] of turtle v2)
end



@#$#@#$#@
GRAPHICS-WINDOW
240
7
651
439
19
19
10.3
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

CC-WINDOW
5
458
1001
553
Command Center
0

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
2
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
6
319
178
439
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
setup-ABTkernel
NIL
1
T
OBSERVER
NIL
NIL
NIL
NIL

BUTTON
704
94
814
127
NIL
go-ABTkernel
NIL
1
T
OBSERVER
NIL
NIL
NIL
NIL

BUTTON
704
128
820
161
NIL
go-ABTkernel
T
1
T
OBSERVER
NIL
NIL
NIL
NIL

TEXTBOX
716
175
862
286
1- setup\n2- Layout until it's pretty\n3- setup-ABTkernel \n4- go-ABTkernel 
13
0.0
0

PLOT
690
258
953
444
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

SLIDER
12
146
184
179
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

MONITOR
846
176
957
229
C-ccks
max ([AgentC_Cost] of nodes)
3
1
13

SLIDER
13
295
185
328
num-agents
num-agents
0
50
5
1
1
NIL
HORIZONTAL

@#$#@#$#@
DESCRIPTION
-----------
In this paper, it is  presented a general framework for solving DCSP-s, framework which we will refer to as ABT kernel. The ABT kernel algorithm, a new ABT-based algorithm that does not require to add communication links between initially unconnected agents.  The ABT kernel algorithm is sound but may not terminate (the ABT kernel may store obsolete information). We start at this procedure, which forms the basic centre or the unifying framework, we could reach the known algorithms or versions close to this ( such as Yokoo's ABT or a correct version of DIBT, DisDB).

	In this centre it is considered that the agents are in a static order, based on a certain criteria. Therefore, an agent having a higher priority than other will be considered at a higher level in a tree that represents the hierarchy of the agents. If self is a generic agent, than G-(self) is the group of agents that have self constraints and that occur at higher levels in this hierarchy (called the parents group) and with  G+(self) it is the group of agents that have self constraints that occur at inferior levels in hierarchy (called the children group). The agents, along with their constraints form a graph acyclic oriented, in which the constraints are oriented from the higher priority agents to lower priority agents.

	Each agents needs to record certain information relative to the global state of searching. This state implies that the values affected to the higher level agents to be known (these values are called agent view) and they form the context of local work and we should also know the lists of unsatisfactory values (called nogood). The agents exchange these affected values and nogoods by, practically, searching through a simple curl that continues until a solution is found or an inconsistence is found. This mechanism is what we meet in all the asynchronous techniques.

The algorithm uses three types of messages, resembling to the ABT technique:
<ul>
<li> Info type messages, sent to children (resembling to the ok type messages), in 		order to inform the attribute of a new value</.li>
<li>Back type messages ( resembling to the nogood type messages), that occur in 		the situation of an agent not finding a consistent value (practically any value 	is inconsistent with the local context). </li>
<li>Stop system message</li>
</li>
</ul>
CREDITS
--------

Title: Graph Coloring using ABT kernel

Author: Ionel Muscalagiu, Jose M. Vidal

Description: This is the implementation of the ABT kernel for the graph coloring problem. We solve the graph coloring problem using the ABT kernel algorithm from 
<ul>
<li>C. Bessiere, I. Brito, A. Maestre, P. Meseguer. 
<a href="http://jmvidal.cse.sc.edu/lib/bessiere05a.html ">
"Asynchronous Backtracking without Adding Links: A New Member in the ABT Family" 
</a>. 
Artificial Intelligence, volume 161, pages 7-24. 2005.</li>
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
NetLogo 4.0
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
