; Combinatorial Auctions
; by
; Jose M Vidal

;
undirected-link-breed [edges edge ]
breed [bids bid]
breed [items an-item]

edges-own []

;bid-amount: the amount of this bid
;cleared: true is bid has been (temporarily) cleared
;num: my id number, used for reading from cats file
bids-own [bid-amount num in-optimal]
items-own []

;diam: size of items/items
;diam-bids: diameter of bids
;max-revenue: maximum revenue that can be achieved, used by branch-and-bid algorithm
;max-revenue-bids: the bids that give us the max revenue
;best: an array whose indexes correspond to item's whos. It contains the highest possible contribution of that item
;         that is best-bid-that-contains-that-item / number-of-items-in-bid
globals [diam diam-bids epsilon done? max-revenue max-revenue-bob max-revenue-bids call-stack best bound-used call-stack-boi max-revenue-boi] 

to setup-globals  ; separate procedure for setting globals
  set diam 3
  set diam-bids 2.5
  set epsilon 1
  set-default-shape items "circle"
  set-default-shape bids "square" ;
end

;Create the items and bids
to setup  ; Setup the model for a run, build a graph.
  ca
  clear-output
  setup-globals
  ask patches [set pcolor white]
  setup-items
  setup-random-bids
  max-revenue-solution
  foreach max-revenue-bids [
    ask ? [set shape "squareoutline"]]
;    set [shape] of ? "squareoutline"]
end

to setup-branch-on-bids
  ask bids [set color green]
  set bound-used 0
  let bids-list [who] of bids ;any ordering will do, for now.
  set max-revenue-bob 0
  set max-revenue-bids []
  set call-stack (list (list (first bids-list) (butfirst bids-list) 0 [] []))
  setup-best-table
end

to setup-branch-on-items
  ask bids [set color green]
  set bound-used 0
  set call-stack-boi []
  let first-item min-one-of items [who]
  set max-revenue-boi 0
  set max-revenue-bids []
  foreach [self] of items [
    setup-bid (count bids) 0 (turtle-set ?)
  ]
  setup-best-table
  ask first-item [
    branch-on-items 0 [] []
  ]
  reset-perspective
end

to-report bids-that-contain [item-who]
  report bids with [member? item-who ([who] of link-neighbors)]
end

to setup-best-table
  set best n-values (count items) [0]
  ask items [
    if (any? link-neighbors) [
      set best replace-item who best [bid-amount / count link-neighbors] of max-one-of (bids-that-contain who) [bid-amount / count link-neighbors]
    ]
  ]
end

;Layout, with user interversion. Must be in a forever button
to layout
  layout-spring (turtle-set items bids) edges spring-force spring-length mutual-repulsion
  if mouse-down? [
    let chosen-one closest-xy mouse-xcor mouse-ycor (turtles with [breed = items or breed = bids])
    while [mouse-down?] [
      ask chosen-one [setxy mouse-xcor mouse-ycor]
      layout-spring (turtle-set items bids) edges spring-force spring-length mutual-repulsion
     ]  
    ]
end

to-report closest-xy [x y agent-set]  ; Return closest agent to x, y
  report min-one-of agent-set [distancexy-nowrap x y]
end
;;;create graph from a flat random distribution

;Creates the items (items).
to setup-items
  create-items number-of-items [
    set color yellow
    set label-color black
    set size diam
    set label who
  ]
end

to setup-random-bids
  let n 0
  repeat number-of-bids [
    let the-items n-of (min-bid-size + random (1 + max-bid-size - min-bid-size)) items
    setup-bid n (1 + random (10 * count the-items)) the-items
    set n n + 1
  ]
end

;creates a new bid and its edges
;n: unique id (num) for the bid
;amount: bid-amount the amount of the bid
;the-items:  the bidset of items the bid is over.
to setup-bid [n amount the-items]
  create-bids 1 [
    create-edges-with the-items [set color black]
    set num n
    set in-optimal false
    set size diam-bids
    set bid-amount amount
    set color green
    set heading 0
    set label-color black
    set label (word "$" round bid-amount)
  ]
end


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;algorithms

;--------------------------------------------
;Centralized winner determination using dfs

;sets max-revenue and max-revenue-bids to the appropiate values
to max-revenue-solution
  set max-revenue 0
  set max-revenue-bids []
  let bids-list [self] of bids 
  ask (first bids-list) [
    find-max-revenue-solution (butfirst bids-list) 0 [] []]
end

;reports true if l1 and l2 do not contain any elements in common
to-report disjoint-lists? [l1 l2]
  foreach l1 [
    if (member? ? l2) [report false]]
  report true
end

;bid function
;Performs a recursive complete search on the branch-on-bids tree. This function cannot be stopped thus it is not the one we use for visualizing the search.
;For that, we use branch-on-bids.
to find-max-revenue-solution [bids-left revenue-thus-far bids-cleared items-cleared]
  ;I am in
  if (disjoint-lists? ([self] of edge-neighbors) items-cleared) [ ;I can be in, my items do not overlap
    let revenue-thus-far-new revenue-thus-far + bid-amount
    let bids-cleared-new lput self bids-cleared
    let items-cleared-new sentence items-cleared ([self] of edge-neighbors)
    if (revenue-thus-far-new >  max-revenue) [
      set max-revenue revenue-thus-far-new
      set max-revenue-bids bids-cleared-new]
    if (not empty? bids-left) [
      ask (first bids-left)[
        find-max-revenue-solution (butfirst bids-left)  revenue-thus-far-new bids-cleared-new items-cleared-new]]]
  ;I am out
  if (not empty? bids-left) [
    ask (first bids-left)[
      find-max-revenue-solution (butfirst bids-left) revenue-thus-far bids-cleared items-cleared]]
end


;In order to display every step of this recursive algorithm I implement my own little call-stack.
;Each call to this function pops a function call from the front stack and runs it. The function them pushes
; new function calls on the stack.
;Using the "run" function (now commented out) works but is waaaayyyy slower
to run-branch-on-bids
  tick
  if (empty? call-stack) [
    ask bids with [member? who max-revenue-bids] [set color red]
    reset-perspective
    stop]
  let next first call-stack
  set call-stack butfirst call-stack
  ask (bid (item 0 next)) [
    branch-on-bids (item 1 next) (item 2 next) (item 3 next) (item 4 next)]
;  run next
  set-current-plot "Revenue: Sum of Cleared Bid Amounts"
  plot max-revenue-bob
end

;Reports the best possible price we could get by selling all the items NOT in items-cleared.
;This price might be impossible to achieve. It is an UPPER bound on the total price.
to-report h-function [items-cleared]
    report sum [item who best] of items with [not member? who items-cleared]
end

;bid function
;bids-left is a list of whos of bids
;bids-cleared is a list of whos of bidss
;items-cleared is a list of whos of items
to branch-on-bids [bids-left revenue-thus-far bids-cleared items-cleared]
  watch-me
  ask bids [set color green]
  ask bids with [member? who bids-cleared] [set color red]
  
  ;I am out. Add this call to the front of the stack.
  if (not empty? bids-left) [
    set call-stack (fput (list (first bids-left) (butfirst bids-left) revenue-thus-far bids-cleared items-cleared) call-stack)
  ]
  
  ;I am in. Add this call to the front of the stack.
  if (disjoint-lists? ([who] of edge-neighbors) items-cleared) [ ;I can be in, my items do not overlap
    let revenue-thus-far-new revenue-thus-far + bid-amount
    let bids-cleared-new lput who bids-cleared
    let items-cleared-new sentence items-cleared ([who] of edge-neighbors)
    if (revenue-thus-far-new >  max-revenue-bob) [
      set max-revenue-bob revenue-thus-far-new
      set max-revenue-bids bids-cleared-new
      ]
    
    ifelse (not empty? bids-left and (revenue-thus-far-new + h-function items-cleared-new > max-revenue-bob)) [
      set call-stack fput (list (first bids-left) (butfirst bids-left) revenue-thus-far-new bids-cleared-new items-cleared-new) call-stack 
      ][
      ;this line is just for keeping track of performance
      if (not empty? bids-left) [set bound-used bound-used + 1]
      ]
    ]
end


to run-branch-on-items
  tick
  if (empty? call-stack-boi) [
    ask bids [set color green]
    ask bids with [member? self max-revenue-bids] [set color red]
    reset-perspective
    stop]
  let next first call-stack-boi
;  show next
  set call-stack-boi butfirst call-stack-boi
  ask (item 0 next) [
    branch-on-items (item 1 next) (item 2 next) (item 3 next)]
  set-current-plot "Revenue: Sum of Cleared Bid Amounts"
  plot max-revenue-boi
end

;bid/item funtion
;reports true if one or more of the item/bids this bid/item is over is a member of items-list
;items-list is a list of who's of items
to-report contains-items-from [items-list]
  let bid-items [who] of link-neighbors 
  foreach bid-items [
    if (member? ? items-list) [
      report true
    ]
  ]
  report false
end

;item function
;bids-cleared is a list of bids
;items-cleared is a list of who's of items
to branch-on-items [revenue-thus-far bids-cleared items-cleared]
  watch-me
  ask bids [set color green]
  ask bids with [member? self bids-cleared] [set color red]
  
  let possible-bids [self] of (link-neighbors with [(not member? who bids-cleared) and not contains-items-from items-cleared])
  let other-uncleared-items (other items with [not member? who items-cleared])
  let next-item nobody
  if (any? other-uncleared-items) [
    set next-item min-one-of other-uncleared-items [who]
  ]
  foreach possible-bids [ 
    let new-revenue revenue-thus-far + ([bid-amount] of ?)
    let new-bids-cleared (sentence bids-cleared ?)
    if (new-revenue > max-revenue-boi) [
      set max-revenue-boi new-revenue
      set max-revenue-bids new-bids-cleared
    ]
    if (next-item != nobody) [
      let new-items-cleared sentence [who] of ([link-neighbors ] of ?) items-cleared
      ifelse (new-revenue + h-function new-items-cleared > max-revenue-boi) [
        set call-stack-boi fput (list next-item new-revenue new-bids-cleared new-items-cleared) call-stack-boi
        ][
        set bound-used bound-used + 1
      ]
    ]
  ]
end



;;;;--------------------------------------------------------------------------------------
;;;Create graph by reading from CATS file. CATS is a separate program for generating bids for a CA.
;;;
;;;These need to be used "by hand" from the command prompt.
to-report string-read [s]
  let pos position "\t" s
  if (is-boolean? pos) [set pos length s]
  let token substring s 0 pos
  if (token = "#") [report "#"]
  report read-from-string token
end

to-report string-remove-head [s]
  report substring s (position "\t" s + 1) (length s)
end

to load-from-file
  setup-from-file "/home/jmvidal/progs/cats20/4/0000"
end

;read from a cats-generated file
to setup-from-file [filename]
  ca
  clear-output
  setup-globals
  ask patches [set pcolor white]
  file-close-all
  file-open (word filename ".txt") 
  let found 0
  set number-of-items 0
  while [found < 3][
    let line file-read-line
    if (line != "" and first line != "%")[
      let space-pos position " " line
      let token substring line 0 space-pos
      if (token = "goods" or token = "dummy") [
        set found found + 1
        set number-of-items number-of-items + read-from-string substring line (space-pos + 1) (length line)]
      if (token = "bids")[
        set found found  + 1
        set number-of-bids read-from-string substring line (space-pos + 1) length line]
      ]
    ]
    setup-items
    let blank file-read-line
    repeat number-of-bids [
      let line file-read-line
      let n string-read line
      set line string-remove-head line
      let amount string-read line
      set line string-remove-head line
      let items-list []
      let the-item string-read line
      while [the-item != "#"][
        set items-list lput the-item items-list
        set line string-remove-head line
        set the-item string-read line
      ]
      setup-bid n amount items with [member? who items-list]
    ]
    file-close
    file-open (word filename ".win")
    while [not file-at-end?][
      let winner-id file-read
      ask (one-of bids with [num = winner-id]) [
        set in-optimal true]]
;      set [in-optimal] of (one-of bids with [num = winner-id]) true]
    file-close
end
@#$#@#$#@
GRAPHICS-WINDOW
194
10
796
633
19
19
15.2
1
10
1
1
1
0
0
0
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
6
156
61
189
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

SLIDER
4
11
191
44
number-of-items
number-of-items
2
50
10
1
1
NIL
HORIZONTAL

SLIDER
4
212
180
245
spring-force
spring-force
0
1
0.09
0.01
1
NIL
HORIZONTAL

SLIDER
4
246
180
279
spring-length
spring-length
0
10
5.25
0.25
1
NIL
HORIZONTAL

SLIDER
4
281
180
314
mutual-repulsion
mutual-repulsion
0
10
1.5
0.25
1
NIL
HORIZONTAL

SLIDER
4
46
191
79
number-of-bids
number-of-bids
1
100
18
1
1
NIL
HORIZONTAL

PLOT
803
319
1131
460
Revenue: Sum of Cleared Bid Amounts
NIL
NIL
0.0
1.0
0.0
1.0
true
false
PENS
"optimal" 1.0 0 -2674135 false
"default" 1.0 0 -16777216 true

SLIDER
5
80
190
113
max-bid-size
max-bid-size
1
10
3
1
1
NIL
HORIZONTAL

SLIDER
6
115
190
148
min-bid-size
min-bid-size
1
10
2
1
1
NIL
HORIZONTAL

BUTTON
66
156
143
189
NIL
layout
T
1
T
OBSERVER
NIL
NIL
NIL
NIL

MONITOR
803
261
913
314
NIL
max-revenue
3
1
13

BUTTON
801
51
950
84
branch-on-bids
run-branch-on-bids
NIL
1
T
OBSERVER
NIL
NIL
NIL
NIL

BUTTON
800
89
949
122
branch-on-bids
run-branch-on-bids
T
1
T
OBSERVER
NIL
NIL
NIL
NIL

MONITOR
917
262
1044
315
NIL
bound-used
0
1
13

BUTTON
803
185
961
218
branch-on-items
run-branch-on-items
NIL
1
T
OBSERVER
NIL
NIL
NIL
NIL

BUTTON
803
224
961
257
branch-on-items
run-branch-on-items
T
1
T
OBSERVER
NIL
NIL
NIL
NIL

BUTTON
802
150
1010
183
NIL
setup-branch-on-items
NIL
1
T
OBSERVER
NIL
NIL
NIL
NIL

BUTTON
801
14
999
47
NIL
setup-branch-on-bids
NIL
1
T
OBSERVER
NIL
NIL
NIL
NIL

@#$#@#$#@
Title: Combinatorial Auction
Author: Jose M Vidal
Description:

Generates and solves a combinatorial auction. The yellow circles represent the items for sale and the green squares represent the bids. Each bid is connected to the items it is over. The square bids with the squares around them are the ones in the revenue-maximizing solution, which we find upon "setup" by running a branch-and-bids algorithm. We implement a branch and bound algorithm on both the branch on items search tree and the branch on bids search tree.

The simple branch and bound algorithms implemented here are described in 
<ul><li>
 Jose M. Vidal. <a href="http://www.multiagent.com/fmas/">Fundamentals of Multiagent Systems: Using NetLogo Models</a>. Unpublished. 2006.
</li></ul>

If you want to learn more about combinatorial auctions I recommend
<ul><li>
Peter Cramton and Yoav Shoham and Richard Steinberg. <a href="http://jmvidal.cse.sc.edu/lib/cramton06a.html">Combinatorial Auctions</a>. MIT Press. 2006.
</li></ul>
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

link
true
0
Line -7500403 true 150 0 150 300

link direction
true
0
Line -7500403 true 150 150 30 225
Line -7500403 true 150 150 270 225

none
true
0

square
true
0
Rectangle -7500403 false true 45 45 255 270
Rectangle -7500403 true true 45 45 255 270

squareoutline
true
0
Rectangle -7500403 false true 45 45 255 270
Rectangle -7500403 true true 60 60 240 255
Rectangle -7500403 false true 30 30 270 285
Rectangle -7500403 false true 15 15 285 300

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
