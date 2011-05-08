;Graph coloring using distributed algorithms
;Uses Adopt, from AAMAS03

breed [ vertices ]
breed [ context-agents ]


;edges is a list of lists: a 2D array indexed by who. The value is 1 if there is a link, 0 otherwise.
;num-colors
;domain is the list of allowed colors
globals [edges num-colors domain time]

;parent is the 'who' of the parent
;parent-agent is the actual parent
;children is a list of the 'who' of the children
;children-agent is an agentset of the children
;level in the tree. root is 0
;descendant-neighbors is a list of the 'who' of all vertices that are descendants and have a constraint with me
;the-neighbors is a list of the 'who' of all vertices that have a constraint with me
;dfs-neighbors is like above but might include others, it is used by the dfs-tree algorithm of Modi.

;threshold- as in paper
;current-context is CurrentContext from the paper
;lbounds is lb(d_i,x_l)
;ubounds is ub(d_i,x_l)
;t 
;context is context(d_i,x_l)
;terminate is set to true if received a terminate message
;dead is set to true if vertice has terminated execution
;d_i from the paper is represented by 'color'
;last-threshold-message is a list indexed by who were each item is the last threshold msg sent to that agent. [val current-context]
vertices-own [message-queue parent parent-agent children children-agent level descendant-neighbors 
                the-neighbors dfs-neighbors dfs-change threshold current-context lbounds ubounds t context
                terminate dead messages-handled messages-received last-threshold-message context-display need-to-backtrack]

;returns a list of legnth n where each item is v
to-report make-list [n v]
    let res 0
  
    set res []
    repeat n [
        set res fput v res]
    report res
end

to-report lists-equal? [l1 l2]
    let i 0
  
    if (length l1 != length l2) [report false]
    set i 0
    while [i < length l1][
        if (item i l1 != item i l2)[report false]
        set i i + 1
    ]
    report true
end

to make-link [v1 v2]
    let tmp 0
  
    if (v2 < v1)[
        set tmp v2
        set v2 v1
        set v1 tmp]
    if (not neighbors? v1 v2) [
        set edges replace-item v1 edges (lput v2 (item v1 edges))
    ]
end

to-report neighbors? [v1 v2]
    let tmp 0
  
    if (v2 < v1) [
        set tmp v2
        set v2 v1
        set v1 tmp]
    report member? v2 (item v1 edges)
end

;reports true if edges and num-vertices represent a connected graph
to-report connected?
    let clique 0
  let i 0
  let j 0
  
    set i 0
    set clique []
    set clique lput i clique
    set i 1
    set j 0
    while [j < num-vertices][
         while [i < num-vertices][
             if (not member? i clique)[
                 if (not empty? filter [neighbors? ? i] clique)[
                     set clique lput i clique
                     if (length clique = num-vertices)[
                         report true
                     ]
                  ]
             ]
             set i i + 1
          ]
          set j j + i
    ]
    report false
end

to make-edges-2
  let v1 0
  let v2 0
  let tmp 0
  
  set edges make-list num-vertices []
  make-link 0 1
  make-link 0 4
  make-link 0 5
  make-link 0 6
  make-link 1 2
  make-link 1 5
  make-link 1 6
  make-link 2 3
  make-link 2 4
  make-link 2 5
  make-link 3 4
  make-link 3 6
  make-link 5 6
  set num-vertices 7
  set v1 0
  while [v1 < num-vertices][
      set tmp (item v1 edges)
      set ([the-neighbors] of (turtle v1)) []
      set v2 0
      while [v2 < num-vertices][
          if (neighbors? v1 v2)[
              set ([the-neighbors] of (turtle v1)) lput v2 [the-neighbors] of (turtle v1)
          ]
          set v2 v2 + 1
      ]
      set v1 v1 + 1
  ]
end
;sets 'edges' and 'the-neighbors'
to make-edges
    let edges-created 0
  let v1 0
  let v2 0
  let tmp 0
  
    set edges make-list num-vertices []
    set edges-created 0
    while [edges-created < num-vertices * edge-density][
        set v1 (random num-vertices)
        set v2 (random num-vertices)
        if (v1 > v2) [
            set tmp v2
            set v2 v1
            set v1 tmp
        ]
        if (v1 != v2 and (not neighbors? v1 v2))[
            make-link v1 v2
            set edges-created (edges-created + 1)
        ]
    ]
    set v1 0
    while [v1 < num-vertices][
        set tmp (item v1 edges)
        set ([the-neighbors] of (turtle v1)) []
        set v2 0
        while [v2 < num-vertices][
            if (neighbors? v1 v2)[
                set ([the-neighbors] of (turtle v1)) lput v2 [the-neighbors] of (turtle v1)
            ]
            set v2 v2 + 1
        ]
        set v1 v1 + 1
    ]
;    if (any vertices with [empty? the-neighbors]) [
;        make-edges]
    if (not connected?)[
        print "Graph not connected. Generating again..."
        make-edges]
end

to set-level
    ifelse (parent = -1)[
        set level 0
    ][
        set level ([level] of parent-agent) + 1
    ]
end

;vertice function
to find-best-position
    let possible-patches 0
  let siblings 0
  let best-patch 0
  
    ifelse (parent = -1) [
        set xcor 0
    ][
        set siblings vertices with [self != myself and (level = [level] of myself or who = [parent] of myself)]
        set possible-patches patches with [distance-nowrap myself < 20 and (pycor = round [ycor] of myself)]
        set best-patch max-one-of possible-patches [sum [log (distance-nowrap myself + .1) 2] of siblings]
        set xcor ([pxcor] of best-patch)
        if (xcor < 0 + min-pxcor + 50) [set xcor 0 + min-pxcor + 50]
        if (xcor > max-pxcor - 10) [set xcor max-pxcor - 10]
    ]          
end

;vertices function. Draws an edge from this vertice to other-vertice
to draw-edge [other-vertice col]
    let dista 0
  let oc 0
  let ox 0
  let oy 0
  
    set oc color
    set ox xcor
    set oy ycor
    set color col
    set dista distance-nowrap other-vertice
    set heading towards-nowrap other-vertice
    pen-down
    fd dista
    pen-up
    set color oc
    setxy ox oy
    set heading 0
end

to draw-edges
    let v1 0
  
    set v1 0
    while [v1 < num-vertices][
        no-display
        ask (turtle v1) [
            without-interruption [
                foreach the-neighbors [
                    draw-edge (turtle ?) blue
                ]
            ]
        ]
        display
        set v1 v1 + 1
    ]
    ;color tree differently
    no-display
    ask vertices [
         if (parent >= 0)[
             draw-edge parent-agent cyan
         ]
    ]
    display
end      

to setup
    let i 0
  
    ca
    clear-output
    show "Setup"
    set num-colors 3
    set domain []
    set time 1
    set i 0
    while [i < num-colors][
        set domain lput item i [15 105 64 125 45 85 35 55 5] domain
        set i i + 1
    ]
    create-vertices num-vertices
    
    ask vertices [
        set size 12
        set shape "circle"
        set label who
        set parent -2
        set terminate false
        set dead false
        set lbounds make-map
        set ubounds make-map
        set t make-map
        set context make-map
        if (who != 0)[
            set-current-plot "Messages"
            create-temporary-plot-pen (word who)
            set-plot-pen-color (5 + 10 * who) mod 140
            set-current-plot "Message-queue"
            create-temporary-plot-pen (word who)
            set-plot-pen-color (5 + 10 * who) mod 140

        ]
;        set color item (random-int-or-float num-colors) domain
        set color item 0 domain
        set messages-handled 0
    ]
    make-edges-2
    set-dfs-tree-2
    repeat num-vertices [
        ask vertices [
            set-level
        ]
    ]
    
    ;If there is only one root (level =0) then we have a tree.
    if (not (count (vertices with [level = 0]) = 1))[
         show "ERROR: more than 1 root...."]
   
    set i max [level] of vertices
    ask vertices [
        set ycor (max-pycor - 20) - (level * ((world-height - 30) / i))]
    repeat 8 [
        ask vertices [
            find-best-position
        ]
    ]
    draw-edges
    
    ;;set up the context display
    create-context-agents num-vertices * num-vertices [
            set size 8
            set shape "circle"
            set color black
    ]
    ask vertices [
        set context-display context-agents with [who >= (([who] of myself + 1) * num-vertices) 
                                                    and who < (([who] of myself) + 2) * num-vertices]
    ]
    set i 0
    while [i < num-vertices][
        ask ([context-display] of turtle i) [
            set ycor ([ycor] of turtle i) + 15
            set xcor ([xcor] of turtle i) - (5 * num-vertices) + 10 * (who - ((i + 1) * num-vertices))
            set label(who - ((i + 1) * num-vertices))
        ]
        set i i + 1
    ]
    
    ;;adopt::initialize
    ask vertices [
        set messages-received 0
        initialize]
    show "All initialized"
end

to update-contexts
    let i 0
  let c 0
  
    set i 0
    while [i < num-vertices][
        ask ([context-display] of turtle i)[
            set c item (who - (i + 1) * num-vertices) [current-context] of turtle i
            ifelse (c = -1) [
                set color black
            ][
                set color c
            ]
        ]
        set i i + 1
    ]
end

to go
    let done 0
  
    set time time + 1
    if (show-messages) [print "===================="]
    ask vertices [
        handle-message
        set label (word LB "-" threshold "-" UB ":" who)
        set-current-plot "Messages"
        set-current-plot-pen (word who)
        plot messages-received]
     update-contexts
     ask vertices [
        if (need-to-backtrack) [backtrack]
        set-current-plot "Message-queue"
        set-current-plot-pen (word who)
        plot length message-queue
    ]     
    set done reduce [?1 and ?2] [dead] of vertices
    type time 
    type "- "
    ask vertices [
      type position color domain
    ]
    if (done) [stop]
end

to go-cycle
    let done 0
  
    if (show-messages) [print "===================="]
    set time time + 1
    ask vertices [
        set need-to-backtrack false
        set message-queue lput list "cycle" "done" message-queue]
    set done (reduce [?1 and ?2] [dead or empty? message-queue or first first message-queue = "cycle"] of vertices)
    while [not done][
         ask vertices [
             handle-message
             set label (word LB "-" threshold "-" UB ":" who)
             set-current-plot "Messages"
             set-current-plot-pen (word who)
             plot messages-received]
         set done (reduce [?1 and ?2] [dead or empty? message-queue or first first message-queue = "cycle"] of vertices)
         update-contexts
    ]
    ask vertices [
        if (not empty? message-queue and first first message-queue = "cycle") [
            set message-queue butfirst message-queue
        ]
        if (need-to-backtrack) [backtrack]
        set-current-plot "Message-queue"
        set-current-plot-pen (word who)
        plot length message-queue
    ]
    set done reduce [?1 and ?2] [dead] of vertices
    type time 
    type "- "
    ask vertices [
      type position color domain
      type " "
    ]
    print ""
    if (done) [stop]
end

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;Map data structure 

to-report make-map
    report []
end

to-report get-triple-index [map-name key1 key2]
    let i 0
  
    set i 0
    while [i < length map-name][
        if (item 0 item i map-name = key1 and
            item 1 item i map-name = key2) [
            report i]
        set i i + 1
    ]
    report -1
end
    
to-report set-map-value [map-name key1 key2 value]
    let triple-index 0
  
    set triple-index get-triple-index map-name key1 key2
    ifelse (triple-index = -1) [
        report lput (list key1 key2 value) map-name
    ][
        report replace-item triple-index map-name (list key1 key2 value)
    ]
end

to-report get-map-value [map-name key1 key2]
    let i 0
  
    set i 0
    while [i < length map-name][
        if (item 0 item i map-name = key1 and
            item 1 item i map-name = key2) [
            report item 2 item i map-name]
        set i i + 1
    ]
    report []
end 

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;Context data structure .

to-report  make-context
     report make-list num-vertices -1 
end

to-report add-to-context [cont v val]
    report replace-item v cont val
end

to-report remove-from-context [cont v]
    report replace-item v cont -1
end 

;a vertice function
to-report add-my-value-to-context [cont]
    report replace-item who cont color
end


to-report compatible? [a b]
    let i 0
  
    set i 0
    while [i < num-vertices][
            if (item i a != -1 and item i b != -1 and item i a != item i b)[
                    report false]
            set i i + 1
    ]
    report true
end

;assumes a and b are compatible
to-report union [a b]
    let c 0
  let i 0
  
    set c make-context 
    set i 0
    while [i < num-vertices][
        if (item i a != -1) [
            set c replace-item i c (item i a)]
        if (item i b != -1) [
            set c replace-item i c (item i b)]
        set i i + 1
   ]
   report c
end


;returns a list of the who of the agents in the context
to-report agents-in-context [cont]
    let i 0
  let res 0
  
    set i 0
    set res []
    while [i < num-vertices][
        if (item i cont != -1)[
           set res lput i res
        ]
        set i i + 1
    ]
    report res
end

;;;Adopt procedures

;Reports wether or not v is an ancestor of i or is i.
to-report ancestor? [v i]
    if ([who] of v) = ([who] of i) [report true]
    if ([parent] of i = -1 or [parent] of i = -2) [report false]
    report ancestor? v ([parent] of i)
end


;netlogo bug: "if (reduce ...)" fails even when the reduce returns a boolean.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;DFS Algorithm by Jay Modi with a change by jmv
;It does not always work (there might not be a constraint between parent-child)
;but we "fix" this by adding a constraint between all parent-childs (they are all random graphs anyway ;-)
to send-parent-message 
    let parents 0
  
    set parents filter [? < who] dfs-neighbors
    ifelse (empty? parents) [
        set parent -1 ;the root
    ][
        if (dfs-change) [ ;jmv added dfs-change variable
            set parent max parents
            send-message parent (filter [? < who] dfs-neighbors)
         ]
    ]
end

to handle-dfs-message
    set dfs-change false
    if (not empty? message-queue)[
        if (not empty? filter [not member? ? dfs-neighbors] (first message-queue))[
            set dfs-change true
            set dfs-neighbors remove-duplicates sentence dfs-neighbors (first message-queue)
        ]
        set message-queue butfirst message-queue]
end

to set-dfs-tree
    ask vertices [
        set dfs-neighbors the-neighbors
        set parent -2
        set children []
        set message-queue []
        set dfs-change true
        send-parent-message]
    while [any? vertices with [not empty? message-queue]][
        ask vertices [
            handle-dfs-message
            send-parent-message
        ]
    ]
    
    ;ugly hack: add an edge between any parent-child that does not have one
    ask vertices [
        if (parent != -1 and (not member? parent the-neighbors))[
            make-link who parent
            set the-neighbors lput parent the-neighbors
            set ([the-neighbors] of (turtle parent)) lput who ([the-neighbors] of (turtle parent))
            show (word "Added extra edge " who " - " parent)
        ]
    ]
    
    ;sets the parent-agent and children        
    ask vertices [
        if (parent != -1) [
            set parent-agent (turtle parent)
            set [children] of parent-agent fput who ([children] of parent-agent)
        ]
    ]
    
    ;sets children-agent and descendant-neigbors
    ask vertices [
        set children-agent (vertices with [member? who ([children] of myself)])
        set-descendant-neighbors
    ]
end

to set-dfs-tree-2
  set [parent] of (turtle 6) -1
  set [parent] of (turtle 1) 6
  set [parent] of (turtle 5) 1
  set [parent] of (turtle 2) 5
  set [parent] of (turtle 4) 2
  set [parent] of (turtle 0) 4
  set [parent] of (turtle 3) 4
  
  ask vertices [
    set children []
    set message-queue []
  ]  
  
  ;sets the parent-agent and children        
  ask vertices [
      if (parent != -1) [
          set parent-agent (turtle parent)
          set [children] of parent-agent fput who ([children] of parent-agent)
      ]
  ]
  
  ;sets children-agent and descendant-neigbors
  ask vertices [
     set children-agent (vertices with [member? who ([children] of myself)])
     set-descendant-neighbors
    ]
end

;;;Tree utility functions

;report true if id is a descendant of node
to-report is-descendant? [node id]
    if (member? id ([children] of (turtle node))) [
        report true]
    if (not empty? ([children] of (turtle node)))[
        report reduce [?1 or ?2] map [is-descendant? ? id] ([children] of (turtle node))
    ]
    report false
end

to set-descendant-neighbors
    set descendant-neighbors []
    foreach the-neighbors [
        if (is-descendant? who ?) [
            set descendant-neighbors lput ? descendant-neighbors
         ]
    ]
end

;vertice procedures
to send-message [receiver msg]
    if (show-messages) [show (word "sendto " receiver "-" msg)]
    set [message-queue] of (turtle receiver) lput msg ([message-queue] of (turtle receiver))
    set [messages-received] of (turtle receiver) [messages-received] of (turtle receiver) + 1
end

to send-priority-message [receiver msg]
    if (show-messages) [show (word "sendto " receiver "-" msg)]
    set [message-queue] of (turtle receiver) fput msg ([message-queue] of (turtle receiver))
end
  
;delta
;The local-cost is the total number of neighbors with color of d
to-report local-cost [d]
    let cost 0
  
    set cost 0
    foreach the-neighbors [
        if ((item ? current-context != -1) and item ? current-context = d)[
            set cost cost + 1
        ]
    ]
    report cost
end

;LB(d) from the paper. 
to-report LBd [d]
    let bound 0
  
    set bound local-cost d
    foreach children [
        set bound bound + get-map-value lbounds d ?
    ]
    report bound
end

;UB(d) from The paper. 
to-report UBd [d]
    let bound 0
  
    set bound local-cost d
    foreach children [
        set bound bound + get-map-value ubounds d ?
    ]
    report bound
end

to-report UBmin
    let minarg 0
  let minval 0
  let val 0
  
    set minval 99
    foreach domain [
        set val UBd ?
        if (val < minval)[
            set minval val
            set minarg ?
         ]
     ]
     report list minarg minval
end

to-report UB
    report item 1 UBmin
end

to-report UBarg
    report first UBmin
end

to-report LBmin
    let minarg 0
  let minval 0
  let val 0
  
    set minval 99
    foreach domain [
        set val LBd ?
        if (val < minval)[
            set minval val
            set minarg ?
         ]
     ]
     report list minarg minval
end

to-report LB
    report item 1 LBmin
end

to-report LBarg
    report first LBmin
end

to initialize
    let d 0
  let xl 0
  
    set threshold 0
    set current-context make-context
    set last-threshold-message make-list num-vertices []
    foreach domain [
        set d ?
        foreach children [
            set xl ?
            set lbounds set-map-value lbounds d xl 0
            set ubounds set-map-value ubounds d xl 99
            set t set-map-value t d xl 0
            set context set-map-value context d xl make-context
        ]
    ]
    
    backtrack
    ;instead of doing backtrack, I just send the messages. This way we make sure they go out even if the color does not change.
;    foreach descendant-neighbors [
;        send-message ? (list "value" who color)
;    ]
;    if (parent != -1) [
;        send-message parent (list "cost" who current-context LB UB)
;    ]
end

to handle-message
    let msg 0
  let mt 0
  
    if (dead) [stop]
    if (not empty? message-queue)[
        set msg first message-queue
        set mt first msg
        if (mt = "cycle") [stop]
        set message-queue butfirst message-queue
        set messages-handled messages-handled + 1         
        if (mt = "threshold")[
            handle-threshold msg
            stop]
        if (mt = "terminate")[
            handle-terminate msg
            stop] 
        if (mt = "value")[
            handle-value msg
            stop]
        if (mt = "cost")[
            handle-cost msg
            stop]
        show (word "ERROR: Bad Message " mt)
   ]
end  

to handle-threshold [msg]
    if (show-messages) [show (word "handle-threshold " msg)]
    if (compatible? (item 2 msg) current-context)[
        set threshold item 1 msg
        maintain-threshold-invariant
        set need-to-backtrack true
     ]
end

to handle-terminate [msg]
    if (show-messages) [show (word "handle-terminate " msg)]
    set terminate true
    set current-context (item 1 msg)
    set need-to-backtrack true
end

to handle-value [msg]
    let d 0
  
    if (show-messages) [show (word "handle-value " msg)]
    if (not terminate)[
        set current-context add-to-context current-context (item 1 msg) (item 2 msg)
        foreach domain [
            set d ?
            foreach children [
                if (not compatible? (get-map-value context d ?) current-context) [
                    set lbounds set-map-value lbounds d ? 0
                    set t set-map-value t d ? 0
                    set ubounds set-map-value ubounds d ? 99
                    set context set-map-value context d ? make-context
                 ]
             ]
         ]
         maintain-threshold-invariant
         set need-to-backtrack true
    ]
end

to handle-cost [msg]
    let d 0
  let xk 0
  let dprime 0
  let msg-context 0
  let i 0
  
    if (show-messages) [show (word "handle-cost " msg)]
    set msg-context (item 2 msg)
    set d (item who msg-context)
    set msg-context remove-from-context msg-context who
    if (not terminate)[
        foreach (filter [not member? ? the-neighbors] (agents-in-context msg-context))[
            set current-context (add-to-context current-context ? (item ? msg-context))
        ]
        foreach domain [
            set dprime ?
            foreach children [
                if (not compatible? (get-map-value context dprime ?) current-context)[
                    set lbounds set-map-value lbounds dprime ? 0
                    set t set-map-value t dprime ? 0
                    set ubounds set-map-value ubounds dprime ? 99
                    set context set-map-value context dprime ? make-context
                 ]
            ]
        ]
    ]
    set xk (item 1 msg)
    if (d != -1 and compatible? msg-context current-context)[ ;jmv added d!=1
        set lbounds set-map-value lbounds d xk (item 3 msg)
        set ubounds set-map-value ubounds d xk (item 4 msg)
        set context set-map-value context d xk msg-context
        maintain-child-threshold-invariant
        maintain-threshold-invariant
    ]
    set need-to-backtrack true
end
                            

to backtrack
    let old-color 0
  let the-UB 0
  
    set the-UB UB
    set old-color color
;    if (show-messages) [show "backtrack"]
    ifelse (threshold = the-UB) [
        set color UBarg   
     ][
        if (LBd color > threshold) [
            set color LBarg
        ]
    ]
;    if (old-color != color)[ ;Added by jmv--not part of the published Adopt
        foreach descendant-neighbors [
            send-message ? (list "value" who color)
;        ]
    ]
    maintain-allocation-invariant
    if (threshold = the-UB) and (terminate or parent = -1) [
        foreach children [
              send-message ? (list "terminate" (add-my-value-to-context current-context))
        ]
        set dead true
        set terminate true
        stop
    ]
    if (parent != -1) [
        send-message parent (list "cost" who current-context LB the-UB)    
    ]
end

to maintain-threshold-invariant
    if (threshold < LB)[
        set threshold LB
    ]
    if (threshold > UB)[
        set threshold UB
    ]
end

to maintain-allocation-invariant
    let sum-of-t 0
  let chosen-child 0
  let last-sent 0
  
    set sum-of-t sum map [get-map-value t color ?] children
    while [threshold > local-cost color + sum-of-t][
        set chosen-child one-of (children-agent with [get-map-value ([ubounds] of myself) ([color] of myself) who > get-map-value ([t] of myself) ([color] of myself) who])
        set t set-map-value t color ([who] of chosen-child) (1 + get-map-value t color ([who] of chosen-child))
        set sum-of-t sum map [get-map-value t color ?] children
    ]
    set sum-of-t sum map [get-map-value t color ?] children
    while [threshold < local-cost color + sum-of-t][
        set chosen-child one-of children-agent with [get-map-value ([t] of myself) ([color] of myself) who > get-map-value ([lbounds] of myself) ([color] of myself) who]
        set t set-map-value t color ([who] of chosen-child) (get-map-value t color ([who] of chosen-child) - 1)
        set sum-of-t sum map [get-map-value t color ?] children
    ]
    foreach children [
        set last-sent item ? last-threshold-message ;last thresold message sent to this agent
;        if (empty? last-sent or first last-sent != (get-map-value t color ?) or (not lists-equal? current-context (item 1 last-sent)))[ ;jmv added. Only send if msg different from previous one
            send-message ? (list "threshold" (get-map-value t color ?) current-context)
;            set last-threshold-message replace-item ? last-threshold-message list (get-map-value t color ?) current-context
;        ]
   ]
end

to maintain-child-threshold-invariant
    let d 0
  
    foreach domain [
        set d ?
        foreach children [
            while [get-map-value lbounds d ? > get-map-value t d ?][
                set t set-map-value t d ? (get-map-value t d ? + 1)
            ]
        ]
     ]
    foreach domain [
        set d ?
        foreach children [
            while [get-map-value t d ? > get-map-value ubounds d ?][
                set t set-map-value t d ? (get-map-value t d ? - 1)
            ]
        ]
     ]
end
    
           
@#$#@#$#@
GRAPHICS-WINDOW
270
10
581
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
1
1
ticks

CC-WINDOW
5
507
590
602
Command Center
0

SLIDER
2
44
174
77
num-vertices
num-vertices
1
100
7
1
1
NIL
HORIZONTAL

BUTTON
2
10
71
43
setup
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
2
78
174
111
edge-density
edge-density
0.5
5
2
0.1
1
NIL
HORIZONTAL

BUTTON
72
10
135
43
go
go
NIL
1
T
OBSERVER
NIL
NIL
NIL
NIL

BUTTON
136
10
199
43
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

SWITCH
2
112
165
145
show-messages
show-messages
1
1
-1000

PLOT
2
146
267
342
Messages
NIL
NIL
0.0
10.0
0.0
10.0
true
true
PENS
"0" 1.0 0 -7566196 true

BUTTON
176
44
264
77
NIL
go-cycle
NIL
1
T
OBSERVER
NIL
NIL
NIL
NIL

BUTTON
176
79
264
112
NIL
go-cycle
T
1
T
OBSERVER
NIL
NIL
NIL
NIL

PLOT
3
343
267
493
Message-queue
NIL
NIL
0.0
2.0
0.0
10.0
true
true
PENS
"0" 1.0 0 -7566196 true

@#$#@#$#@
Title: Graph Coloring using Adopt
Author: Jose M Vidal
Description:
We solve the graph coloring problem using the 
Adopt algorithm from  <ul>
 <li>Pragnesh Jay Modi, Wei-Min Shen, Milind Tambe, and Makoto Yokoo. <a href="http://jmvidal.cse.sc.edu/library/modi03a.pdf">An Asynchronous Complete Method for Distributed Constraint Optimization.</a> In <i>Proceedings of Second International Joint Conference on Autonomous Agents and MultiAgent Systems,</i> July; 2003.</li>
</ul>
<p>
	The tree is rooted at the top of the screen. All the lines between nodes
represent constraints. The light-colored lines (cyan) are also the parent-child
links on the tree. The color of the node is, well, the current color of the node.
The numbers on each node represent the LB-threshold-UB:ID. 
</p><p>
This is only a beta version!
</p>
@#$#@#$#@
default
true
0
Polygon -7566196 true true 150 5 40 250 150 205 260 250

ant
true
0
Polygon -7566196 true true 136 61 129 46 144 30 119 45 124 60 114 82 97 37 132 10 93 36 111 84 127 105 172 105 189 84 208 35 171 11 202 35 204 37 186 82 177 60 180 44 159 32 170 44 165 60
Polygon -7566196 true true 150 95 135 103 139 117 125 149 137 180 135 196 150 204 166 195 161 180 174 150 158 116 164 102
Polygon -7566196 true true 149 186 128 197 114 232 134 270 149 282 166 270 185 232 171 195 149 186
Polygon -7566196 true true 225 66 230 107 159 122 161 127 234 111 236 106
Polygon -7566196 true true 78 58 99 116 139 123 137 128 95 119
Polygon -7566196 true true 48 103 90 147 129 147 130 151 86 151
Polygon -7566196 true true 65 224 92 171 134 160 135 164 95 175
Polygon -7566196 true true 235 222 210 170 163 162 161 166 208 174
Polygon -7566196 true true 249 107 211 147 168 147 168 150 213 150

arrow
true
0
Polygon -7566196 true true 150 0 0 150 105 150 105 293 195 293 195 150 300 150

bee
true
0
Polygon -256 true false 151 152 137 77 105 67 89 67 66 74 48 85 36 100 24 116 14 134 0 151 15 167 22 182 40 206 58 220 82 226 105 226 134 222
Polygon -16777216 true false 151 150 149 128 149 114 155 98 178 80 197 80 217 81 233 95 242 117 246 141 247 151 245 177 234 195 218 207 206 211 184 211 161 204 151 189 148 171
Polygon -7566196 true true 246 151 241 119 240 96 250 81 261 78 275 87 282 103 277 115 287 121 299 150 286 180 277 189 283 197 281 210 270 222 256 222 243 212 242 192
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
Polygon -7566196 true true 2 6 2 39 270 298 297 298 299 271 187 160 279 75 276 22 100 67 31 0

bird2
false
0
Polygon -7566196 true true 2 4 33 4 298 270 298 298 272 298 155 184 117 289 61 295 61 105 0 43

boat1
false
0
Polygon -1 true false 63 162 90 207 223 207 290 162
Rectangle -6524078 true false 150 32 157 162
Polygon -16776961 true false 150 34 131 49 145 47 147 48 149 49
Polygon -7566196 true true 158 33 230 157 182 150 169 151 157 156
Polygon -7566196 true true 149 55 88 143 103 139 111 136 117 139 126 145 130 147 139 147 146 146 149 55

boat2
false
0
Polygon -1 true false 63 162 90 207 223 207 290 162
Rectangle -6524078 true false 150 32 157 162
Polygon -16776961 true false 150 34 131 49 145 47 147 48 149 49
Polygon -7566196 true true 157 54 175 79 174 96 185 102 178 112 194 124 196 131 190 139 192 146 211 151 216 154 157 154
Polygon -7566196 true true 150 74 146 91 139 99 143 114 141 123 137 126 131 129 132 139 142 136 126 142 119 147 148 147

boat3
false
0
Polygon -1 true false 63 162 90 207 223 207 290 162
Rectangle -6524078 true false 150 32 157 162
Polygon -16776961 true false 150 34 131 49 145 47 147 48 149 49
Polygon -7566196 true true 158 37 172 45 188 59 202 79 217 109 220 130 218 147 204 156 158 156 161 142 170 123 170 102 169 88 165 62
Polygon -7566196 true true 149 66 142 78 139 96 141 111 146 139 148 147 110 147 113 131 118 106 126 71

box
true
0
Polygon -7566196 true true 45 255 255 255 255 45 45 45

butterfly1
true
0
Polygon -16777216 true false 151 76 138 91 138 284 150 296 162 286 162 91
Polygon -7566196 true true 164 106 184 79 205 61 236 48 259 53 279 86 287 119 289 158 278 177 256 182 164 181
Polygon -7566196 true true 136 110 119 82 110 71 85 61 59 48 36 56 17 88 6 115 2 147 15 178 134 178
Polygon -7566196 true true 46 181 28 227 50 255 77 273 112 283 135 274 135 180
Polygon -7566196 true true 165 185 254 184 272 224 255 251 236 267 191 283 164 276
Line -7566196 true 167 47 159 82
Line -7566196 true 136 47 145 81
Circle -7566196 true true 165 45 8
Circle -7566196 true true 134 45 6
Circle -7566196 true true 133 44 7
Circle -7566196 true true 133 43 8

circle
false
0
Circle -7566196 true true 35 35 230

person
false
0
Circle -7566196 true true 155 20 63
Rectangle -7566196 true true 158 79 217 164
Polygon -7566196 true true 158 81 110 129 131 143 158 109 165 110
Polygon -7566196 true true 216 83 267 123 248 143 215 107
Polygon -7566196 true true 167 163 145 234 183 234 183 163
Polygon -7566196 true true 195 163 195 233 227 233 206 159

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
Polygon -7566196 true true 150 0 180 135 255 255 225 240 150 180 75 240 45 255 120 135

thin-arrow
true
0
Polygon -7566196 true true 150 0 0 150 120 150 120 293 180 293 180 150 300 150

truck-down
false
0
Polygon -7566196 true true 225 30 225 270 120 270 105 210 60 180 45 30 105 60 105 30
Polygon -8716033 true false 195 75 195 120 240 120 240 75
Polygon -8716033 true false 195 225 195 180 240 180 240 225

truck-left
false
0
Polygon -7566196 true true 120 135 225 135 225 210 75 210 75 165 105 165
Polygon -8716033 true false 90 210 105 225 120 210
Polygon -8716033 true false 180 210 195 225 210 210

truck-right
false
0
Polygon -7566196 true true 180 135 75 135 75 210 225 210 225 165 195 165
Polygon -8716033 true false 210 210 195 225 180 210
Polygon -8716033 true false 120 210 105 225 90 210

turtle
true
0
Polygon -7566196 true true 138 75 162 75 165 105 225 105 225 142 195 135 195 187 225 195 225 225 195 217 195 202 105 202 105 217 75 225 75 195 105 187 105 135 75 142 75 105 135 105

wolf
false
0
Rectangle -7566196 true true 15 105 105 165
Rectangle -7566196 true true 45 90 105 105
Polygon -7566196 true true 60 90 83 44 104 90
Polygon -16777216 true false 67 90 82 59 97 89
Rectangle -1 true false 48 93 59 105
Rectangle -16777216 true false 51 96 55 101
Rectangle -16777216 true false 0 121 15 135
Rectangle -16777216 true false 15 136 60 151
Polygon -1 true false 15 136 23 149 31 136
Polygon -1 true false 30 151 37 136 43 151
Rectangle -7566196 true true 105 120 263 195
Rectangle -7566196 true true 108 195 259 201
Rectangle -7566196 true true 114 201 252 210
Rectangle -7566196 true true 120 210 243 214
Rectangle -7566196 true true 115 114 255 120
Rectangle -7566196 true true 128 108 248 114
Rectangle -7566196 true true 150 105 225 108
Rectangle -7566196 true true 132 214 155 270
Rectangle -7566196 true true 110 260 132 270
Rectangle -7566196 true true 210 214 232 270
Rectangle -7566196 true true 189 260 210 270
Line -7566196 true 263 127 281 155
Line -7566196 true 281 155 281 192

wolf-left
false
3
Polygon -6524078 true true 117 97 91 74 66 74 60 85 36 85 38 92 44 97 62 97 81 117 84 134 92 147 109 152 136 144 174 144 174 103 143 103 134 97
Polygon -6524078 true true 87 80 79 55 76 79
Polygon -6524078 true true 81 75 70 58 73 82
Polygon -6524078 true true 99 131 76 152 76 163 96 182 104 182 109 173 102 167 99 173 87 159 104 140
Polygon -6524078 true true 107 138 107 186 98 190 99 196 112 196 115 190
Polygon -6524078 true true 116 140 114 189 105 137
Rectangle -6524078 true true 109 150 114 192
Rectangle -6524078 true true 111 143 116 191
Polygon -6524078 true true 168 106 184 98 205 98 218 115 218 137 186 164 196 176 195 194 178 195 178 183 188 183 169 164 173 144
Polygon -6524078 true true 207 140 200 163 206 175 207 192 193 189 192 177 198 176 185 150
Polygon -6524078 true true 214 134 203 168 192 148
Polygon -6524078 true true 204 151 203 176 193 148
Polygon -6524078 true true 207 103 221 98 236 101 243 115 243 128 256 142 239 143 233 133 225 115 214 114

wolf-right
false
3
Polygon -6524078 true true 170 127 200 93 231 93 237 103 262 103 261 113 253 119 231 119 215 143 213 160 208 173 189 187 169 190 154 190 126 180 106 171 72 171 73 126 122 126 144 123 159 123
Polygon -6524078 true true 201 99 214 69 215 99
Polygon -6524078 true true 207 98 223 71 220 101
Polygon -6524078 true true 184 172 189 234 203 238 203 246 187 247 180 239 171 180
Polygon -6524078 true true 197 174 204 220 218 224 219 234 201 232 195 225 179 179
Polygon -6524078 true true 78 167 95 187 95 208 79 220 92 234 98 235 100 249 81 246 76 241 61 212 65 195 52 170 45 150 44 128 55 121 69 121 81 135
Polygon -6524078 true true 48 143 58 141
Polygon -6524078 true true 46 136 68 137
Polygon -6524078 true true 45 129 35 142 37 159 53 192 47 210 62 238 80 237
Line -16777216 false 74 237 59 213
Line -16777216 false 59 213 59 212
Line -16777216 false 58 211 67 192
Polygon -6524078 true true 38 138 66 149
Polygon -6524078 true true 46 128 33 120 21 118 11 123 3 138 5 160 13 178 9 192 0 199 20 196 25 179 24 161 25 148 45 140
Polygon -6524078 true true 67 122 96 126 63 144

@#$#@#$#@
NetLogo 4.0beta3
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
