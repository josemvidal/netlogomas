;Nodes represent the retailers, distributers, and factory.
breed [nodes node]

;The orders are manipulated by the nodes.
breed [orders order]
;sender, receiver: the node who sent/receives this order
;quanity: how much is being asked-for/delivered
;state: "sent", "received", "intransit", "delivered"
orders-own [sender receiver quantity state sent-time] 


;Directed edges from node to parent node
;breed [links link]

;stock: amount of beer in the wharehouse
;seller: the seller I buy from. If self then I am the factory.
;my-orders: used to hold my current orders
;pending: amount of beer that I have ordered but has not arrived (backorder)
;inventory: stock - backorders
;backorders: total number of outstanding orders I have to fill
;cost: stock * 0.5 + backorders * 2.0 ; this equation taken from Densmore's beer game model
;received: total number of orders I received at this time step
;ordered: total number of orders I placed at this time step
;
;customer? factory? distributor?: return true if I am of that type. Set dynamically
;customer? is a leaf node
;factory? is the root of the tree
;distributor? is everyone else, except that a retailer? is a distributor who only has customers as links in.
;node-type: a string that is either "factory" "distributor" "retailer" or "customer"
;height: distance to root in number of edges
;working?: false if this node is broken.
;lost-orders: only customer? has lost-orders. It is the number of orders it failed to place because his seller was not working?
;distances: [ [agent distance][...]] used to calculate min distance to every other agent
;down-until: time at which this node will start working again
;customers: agentset of all my descendant nodes who are a customer?
nodes-own [stock old-stock seller my-orders customer? factory? distributor? retailer? 
           node-type
           pending old-pending
           inventory old-inventory 
           backorders old-backorders ;total number of orders I have yet to fill
           demand old-demand ;number of new orders I received in this time step
           cost old-cost
           received old-received
           ordered old-ordered
           expected old-expected
           height working? lost-orders distances cluster-coefficient
           down-until
           customers]

;time: current ime
;tickN: time at which buyers change their order quantities
;stock-initial: initial amount of stock
;order-initial: initial (existing) order amounts
;node-died?: true if a node died (stopped working) at this timestep
;output-file: file to which we will write experimental data
;list-of-nodes: a list of the nodes, ordered why who, used for writing to file
;vars-to-check: list of variable names that we want to write to file.
globals [tickN time factory mouse-clicked? changed-distances? characteristic-path-length clustering-coefficient
            stock-initial order-initial order-n
            components max-distance size-max-component
            seed node-died?
            output-file
            list-of-nodes
            vars-to-check
            chosen-one]

to printall
 print word "time=" time
 print word "factory=" factory
 print word "si=" stock-initial 
 print word "oi=" order-initial 
 print word "on=" order-n
 print word "c=" components 
end

;just for fun
;__extensions [ "sound.jar" ]

to-report find-preferential-partner
  let total random-float sum ([(count my-in-links) + 1 ] of nodes)
  let partner nobody
  ;; reporter procedures always run without interruption,
  ;; so we don't need "without-interruption" in order to
  ;; make the turtles go one at a time
  ask nodes
  [
    let nc (count my-in-links) + 1
    if partner = nobody
    [
      ifelse nc > total
        [ set partner self ]
        [ set total total - nc ]
    ]
  ]
  report partner
end

to-report find-random-partner
  report one-of nodes
end

to-report find-fixed-links-partner [n]
  let candidates nodes with [count my-in-links = n]
  if (any? candidates) [
    let min-height min ([height] of candidates)
    set candidates candidates with [height = min-height]
    if (any? candidates) [
      report one-of candidates
    ]
  ]
  report find-fixed-links-partner (n - 1)
end

;General function for creating a graph by first creating two nodes, one of which is a factory
;then we repeadetedly add a new node and connect to the one returned by 'find-function'
to setup-preferential [find-function]
  ;start with two nodes
  create-nodes 1
  ;set directed link to factory node
  set factory one-of nodes
  ask factory [
    set seller factory
    set height 0
  ]
  ;create the rest of the nodes
  while [count nodes < num-nodes][
    let partner (run-result find-function)
    create-nodes 1 [
      set height 1 + [height] of partner
      set seller partner
      create-link-to seller [
        set color gray
      ]
    ]
  ]
end

to setup-random-attachment
 setup-preferential "find-random-partner" 
end

to setup-preferential-attachment
  setup-preferential "find-preferential-partner"
end

to setup-fixed-degree-1
  setup-preferential "find-fixed-links-partner 0"
end

to setup-fixed-degree-2
  setup-preferential "find-fixed-links-partner 1"
end

to setup-fixed-degree-3
  setup-preferential "find-fixed-links-partner 2"
end

;Creates a graph with num-facilities nodes that are not customers and
;exactly num-customers customers and retailers. Each customer is connected to 
;exactly one retailer and each retailer has exactly one customer.
to setup-fixed-preferential [function]
  let real-num-nodes num-nodes
  set num-nodes num-facilities
  setup-graph-with-fixed-customers function num-customers
  
  ;now connect customers
  ask nodes with [customer?] [
     hatch-nodes 1 [
      set height 1 + [height] of myself
      set seller myself
      create-link-to seller [
        set color gray
      ]
     ] 
  ]
  set num-nodes num-facilities + num-customers
end

;Creates a graph with num-facilities + num-customers = nodes and makes
;sure that the number of customers is num-customers.
;We do this by simply generating new graphs until we find one with the desired num-customers
to setup-graph-with-fixed-customers [function nc]
  setup-preferential function
  ask nodes [set-type-one]
  ask nodes [set-type-two]
  let c 0
  while [count nodes with [customer?] != nc][
    if (c > 1000) [
      ifelse ("yes" = user-one-of "Could not find suitable graph. Should I try 1000 more times?" ["yes" "no"])[
        set c 0
      ][
      stop]
    ]
    set c c + 1
    ask nodes [die]
    setup-preferential function
    ask nodes [set-type-one]
    ask nodes [set-type-two]
  ]
end

to setup-fixed-random-attachment
  setup-fixed-preferential "find-random-partner"
end 

to setup-fixed-preferential-attachment
  setup-fixed-preferential "find-preferential-partner"
end 

to setup-fixed-cust-random-attachment
  set num-nodes num-facilities + num-customers
  setup-graph-with-fixed-customers "find-random-partner" num-customers
end 

to setup-fixed-cust-preferential-attachment
  set num-nodes num-facilities + num-customers
  setup-graph-with-fixed-customers "find-preferential-partner" num-customers
end 
 
to setup-globals
  set time 0
  set tickN 4
  set order-initial 4
  set order-n 8
  set stock-initial 12
  ask patches [set pcolor white]
  set components []
  set mouse-clicked? false
  set node-died? false
end
 
to setup
  let ofile output-file
  ca
  set output-file ofile
  setup-globals
  no-display
  run (word "setup-" topology)
  ;Set their color, shape, type, etc.
  ask nodes [
    set-type-one
  ]
  ask nodes [
    set-type-two
  ]
  ask nodes [
    setup-node
  ]
  setup-customers
  layout-radial nodes links factory 
  display
  set-current-plot "Edge Distribution"
  histogram [count my-in-links] of nodes 
  
  ;save seed so reset button will reproduce this run exactly
  set seed new-seed
  random-seed seed
  calculate-graph-values
end

;set the customer variable for each node to be an agentset containing all the customers? who are
;descendants of the node.
to setup-customers
  ask nodes with [retailer?] [
    set customers (in-link-neighbors with [customer?])
  ]
  let change? true
  while [change?][
    set change? false
    ask nodes with [distributor? or factory?][
      without-interruption [
       let new-sets [customers] of in-link-neighbors 
       set new-sets fput (in-link-neighbors with [customer?]) new-sets
       let new-customers reduce [merge-agentsets ?1 ?2] new-sets
        if (count new-customers != count customers) [
          set change? true
          set customers new-customers
        ]
      ]
    ]
  ]
end

;like setup but keeps the current topology
to reset
  set time 0
  set factory one-of nodes with [factory?]
  clear-all-plots
  set-current-plot "Edge Distribution"
  histogram [count my-in-links] of nodes 
 
  ask orders [die]
  ask nodes [
    set working? true
  ]
  ask nodes [
    setup-node
    setup-distances
  ]
  setup-customers
  random-seed seed ;set it to same seed we used before to reproduce run exactly
  calculate-graph-values
end

;Calculate the characteristic-path-length, clustering-coefficient and size-max-component
to calculate-graph-values
  ask nodes [
    setup-distances
  ]
  calculate-distances
  set-components
  let max-component get-largest-component
  set characteristic-path-length get-characteristic-path-length max-component
  set clustering-coefficient get-clustering-coefficient max-component
  set max-distance get-max-distance max-component
  set size-max-component get-size-of-largest-component
end

;turn a stock number n into a size that is suitable for viewing
to-report to-size [n]
  report max list .8 ((log (n + 1) 2) - 2)
end

to-report demand-to-color [d]
  let b 1.0 / ((d / 10) + 1)
  report approximate-rgb (1 - b) 0 b
end

to check-kill-node
  ;If user clicks on a node, toggle its working? variable 
  if (mouse-down? and not mouse-clicked?) [
    let chosen closest-xy mouse-xcor mouse-ycor (nodes with [not customer?])
    ask chosen [
      set working? not working?
      update-color
    ]
    set mouse-clicked? true
  ]
  if (not mouse-down?)[
    set mouse-clicked? false
  ]
end

to go
  if (count nodes = 0) [
    user-message "Press 'setup' first, then press 'go'"
    stop
  ]
  if (trace?) [
    print (word "Time=" time " -------------")
    ask nodes [
      set label-color black
      set label who]]
  check-kill-node
  ask nodes [ ;set these here to avoid a race condition when some other agent creates an order for me during this turn.
    set my-orders orders with [receiver = myself] 
  ]
  ask nodes [ ;players consumer orders and produce beer
    process-player
  ]
  
  ;draw plots now before moving orders so we know the correct value of orders placed/received
  draw-plots
  
  ;move orders along: sent -> received; intransit->delivered
  ask nodes [
    move-orders
  ]
  
 
  ;Now, kill some, if its their time to die
  let change? false
  set node-died? false
  ask nodes with [not customer?] [
    if (random-float 1.0 < prob-failure) [
      set working? false
      set color grey
      set node-died? true
      set down-until (time + random-exponential mean-downtime)
      set change? true
    ]
    if (not working? and time > down-until) [
      set working? true
      set color grey
      set change? true
    ]
  ]
  if (random-float 1.0 < prob-targeted-attack) [
    let target max-one-of (nodes with [not customer?]) [count in-link-neighbors]
    set [working?] of target false
    set [down-until] of target (time + random-exponential mean-downtime)
    set [color] of target grey 
    set node-died? true
    set change? true
  ]
  
  ;Re-calculate the new values, if needed, and plot
  if (change?) [
    calculate-graph-values
  ]
  set-current-plot "Graph Attributes"
  set-current-plot-pen "CPL"
  plot characteristic-path-length
  set-current-plot-pen "CC"
  plot clustering-coefficient
  
  set-current-plot "Lost Orders"
  plot sum [lost-orders] of (nodes with [customer?])
  
  set time time + 1
  layout-radial nodes links factory 
end

to draw-plots
  ;Ask nodes to update their values
  ask nodes [
    if (distributor? or retailer? or factory?) [
;    set backorders sum values-from (orders with [receiver = myself and state = "received"]) [quantity]
    set inventory stock - backorders
    set cost (stock * 0.5) + (backorders * 2.0) ;arbitrary cost function from Densmore's model
   ]
  ]
  
  ;Calculate new and old mean values
  let retailers-old-value sum [run-result (word "old-" plot-variable)] of nodes with [retailer?] 
  let retailers-value sum [run-result plot-variable] of nodes with [retailer?] 
  let distributors-value sum [run-result plot-variable] of nodes with [distributor?] 
  let distributors-old-value sum [run-result (word "old-" plot-variable)] of nodes with [distributor?] 
  let factory-value sum [run-result plot-variable] of nodes with [factory?]
  let factory-old-value sum [run-result (word "old-" plot-variable)] of  nodes with [factory?]
  let customers-value sum [run-result plot-variable] of nodes with [customer?] 
  let customers-old-value sum [run-result (word "old-" plot-variable)] of nodes with [customer?]  
  set-current-plot "Phase Plot"
  set-current-plot-pen "R"
  plotxy retailers-value retailers-old-value
  set-current-plot-pen "D"
  plotxy distributors-value distributors-old-value
  set-current-plot-pen "F"
  plotxy factory-value factory-old-value
  set-current-plot-pen "C"
  plotxy customers-value customers-old-value
  set-current-plot "Time Plot"
  set-current-plot-pen "R"
  plot retailers-value
  set-current-plot-pen "D"
  plot distributors-value
  set-current-plot-pen "F"
  plot factory-value
  set-current-plot-pen "C"
  plot customers-value

  ;Move new values to old- variables
  ask nodes [
    set old-inventory inventory
    set old-stock stock
    set old-backorders backorders
    set old-demand demand
    set old-pending pending
    set old-cost cost
    set old-ordered ordered
    set old-received received
  ]
end

;Set their inventories to be the sum of their children's inventories where customers are assumed to have an inventory of stock-initial
;Must be called after types are set
to setup-stock
  let change? true
  while [change?][
    set change? false
    ask nodes with [not customer?][
      let new-stock (stock-initial * count in-link-neighbors with [customer?])  + 
                           (sum [stock] of (in-link-neighbors with [not customer?]))
      if (new-stock != stock) [
        set stock new-stock
        set change? true
      ]
    ]
  ]
end

to-report merge-agentsets [a b]
  report turtles with [(member? self a) or (member? self b)]
end

;turns list l into a string of comma-separated values
to-report in-csv [l]
  report reduce [(word ?1 "," ?2)] l
end


;----------
;global utility/graph functions

;get all nodes to calculate their distances matrix
;implement dijikstra's algorithm
to calculate-distances
  set changed-distances? true
  while [changed-distances?] [
    set changed-distances? false
    ask nodes [update-distances]
  ]
end

;Report a list of all pairs (n choose 2) from the-list. Does not contain duplicates (permutations)
to-report get-all-pairs [the-list]
  let result []
  foreach the-list [
    let a ?
    foreach the-list [
      if (? != a) [
        set result fput list a ? result
      ]
    ]
  ]
  report result
end

to-report get-all-pairs-agents [agent-set]
  report get-all-pairs [self] of agent-set
end

;Calculates the characteristic path length, which is the smallest number of links it takes to connect one node to another, averaged 
;over all pairs of nodes in the network.
;Do this by first having each node calculate its minimun distance to every other node and then we average over all pairs
to-report get-characteristic-path-length [component]
  let pairs get-all-pairs-agents (nodes with [get-component = component])
  if (length pairs = 0) [report 0]
  let sum-distances 0
  foreach pairs [
    ask (first ?) [ ;since Im assuming bidirections it does not matter which one I ask.
;      show get-distance-to (item 1 ?)
      set sum-distances sum-distances + get-distance-to (item 1 ?)
    ]
  ]
  report sum-distances / length pairs
end

to-report get-max-distance [component]
  report max [max map [item 1 ?] distances] of (nodes with [get-component = component])
end

to-report get-clustering-coefficient [component]
  ask nodes with [get-component = component] [
    set cluster-coefficient get-node-clustering-coefficient
  ]
  report mean [cluster-coefficient] of nodes with [get-component = component]
end

to-report closest-xy [x y agent-set]  ; Return closest agent to x, y
  report min-one-of agent-set [distancexy-nowrap x y]
end

to set-components
  ask nodes [
    without-interruption [
      if (get-component = nobody) [
        set components fput self components
      ]
    ]
  ]
end

;reports the agent that is the representative for the largest component
to-report get-largest-component
  let whole-component reduce [ifelse-value (count ?1 > count ?2)[?1][?2]] (map [nodes with [get-component = ?]] components)
  report [get-component] of (one-of whole-component) 
end

to-report get-size-of-largest-component
  report max map [count ?] (map [nodes with [get-component = ?]] components)
end

;==================================
;node functions

to setup-node
  set backorders 0 set old-backorders 0
  set received 0 set old-received 0
  set ordered 0 set old-ordered 0
  set cost 0 set old-cost 0
  set customers nodes with [false] ;an empty agentset
  
  ;This does not seem to be enough for tall trees.
 ; set stock stock-initial * count __in-link-neighbors ;Beer game starts with 12.
 
   setup-stock
  
  ;Fill the pipeline
  if (not factory?) [
    hatch-orders 1 [
      set receiver [seller] of myself
      set sender myself
      set state "received"
      set sent-time -2
      set quantity order-initial
      hide-turtle
    ]
  ]
  hatch-orders 1 [
    set receiver myself
    set sender [seller] of myself
    set state "intransit"
    set sent-time -1
    set quantity order-initial
    hide-turtle
  ]
  hatch-orders 1 [
    set receiver myself
    set sender [seller] of myself
    set state "delivered"
    set sent-time -2
    set quantity order-initial
    hide-turtle
  ]
  set pending order-initial * 3
  set old-pending pending
  set expected 0 set old-expected order-initial
  set demand 0 set old-demand order-initial
  set inventory (stock - backorders)
  set old-inventory inventory
  set working? true
  set size to-size stock
  set heading 0
  set color blue
  if  (factory?) [
    set color red
    set shape "square"
  ]
  if (customer?) [
    set shape "person"
    set stock 0
  ]
  if (distributor?) [
    set shape "circle"
  ]

  set color blue
end


;A node's type depends on its position on the tree
to set-type-one
  set customer? false
  set distributor? false
  set factory? false
  set retailer? false
  ifelse (not any? nodes with [seller = myself]) [ ;I am a customer since no one is buying from me
    set customer? true
    set node-type "customer"
  ][ 
    ifelse (seller = self) [;I am a factory
      set factory? true
      set node-type "factory"
    ][;;I am a middleman
      set distributor? true
      set node-type "distributor"
    ]
  ]
end

;This needs to be called after all nodes run set-type-one
;It sets a distributor? who has no other distributors as children (only customers) to be a retailer?
to set-type-two
  if (distributor?)[
    ifelse (any? (in-link-neighbors with [distributor?]))[
      set retailer? false
    ][
      set retailer? true
      set distributor? false
      set node-type "retailer"
      set shape "pentagon"
    ]
  ]
end

to process-player
  if (working?) [
    refresh-stock
    process-demand ;customer? never gets any demand so this does nothing for him
    place-orders
  ]
  update-color
end

;Move all my orders from sent->received and intransit->delivered
to move-orders
  ; my-orders was set before this function was called so it does NOT contain orders created by the previous calls
  let my-sent-orders my-orders with [state = "sent"]
  ask my-sent-orders [set state "received"]
  let my-intransit my-orders with [state = "intransit"]
  ask my-intransit [set state "delivered"]
end

to update-color
  if (distributor? or retailer? or factory?) [
    ifelse (working?) [
      ;calculate the node's unmet demand now after I have done process-demand.
      let unmet-demand sum [quantity] of (orders with [receiver = myself and state = "received"]) 
      set color (demand-to-color unmet-demand)
      set size (to-size stock) ;use log(stock) to keep the size from exploding
    ][
      set color grey
    ]
;    play-note "Acoustic Grand Piano" (200 - (size * 40)) (min list 80 (size * 20)) .5 ;noisy!
  ]
 if (customer?) [
    set color (demand-to-color pending)
  ]
end

;Only customer? and distributor? receive deliveries. 
;The customer drinks the beer, the distributor adds any new delivered beer to his stock
to refresh-stock
  ;Add delivered orders to my inventory
  let my-delivered-orders my-orders with [state = "delivered"]
  set received sum [quantity] of my-delivered-orders ;received is the quantity that has just arrived
  if (received > 0) [
    if (trace?) [show (word "Received " received " from " ([[who] of sender] of my-delivered-orders))]
  ]
  if (distributor? or retailer? or factory?) [ 
    set stock stock + received
  ]
  set pending pending - received
  ask my-delivered-orders [die]
end


;Fill all orders until stock runs out. 
;We randomly choose the next order to fill and fill it completely. This might not be the fairest method.
to process-demand  
  let my-received-orders my-orders with [state = "received"]
  ;demand is just those orders that arrived now (thus, they were sent at time-2).
  set demand sum [quantity] of (my-received-orders with [sent-time = (time - 2)]) 
  set backorders sum [quantity] of my-received-orders
  
  ;factory: orders received from itself are made into shipments to itself (simulate raw materials processing)  
  if (factory?) [
    let orders-from-myself my-received-orders with [sender = myself]
    ask orders-from-myself [
      set state "intransit"
    ]
    set my-received-orders my-orders with [state = "received"]
  ]
  
  ;fullfill all orders until stock runs out
  while [stock > 0 and any? my-received-orders][
;    let order-to-fill one-of my-received-orders
    let order-to-fill min-one-of my-received-orders [sent-time]
    if ([state] of order-to-fill != "received") [show "ERROR"]
    let quantity-to-deliver min list stock ([quantity] of order-to-fill)
    set stock stock - quantity-to-deliver
    set backorders backorders - quantity-to-deliver
    hatch-orders 1 [
      set receiver ([sender] of order-to-fill)
      set sender myself
      set state "intransit"
      set sent-time time
      set quantity quantity-to-deliver
      
      ifelse (show-packages?) [
        set heading towards receiver
        set size (to-size quantity)
        set color yellow
        set shape "square"
        forward (distance receiver) / 3
      ][
        hide-turtle
      ]    
    ]
    if (trace?) [show (word "Sending " quantity-to-deliver " to " ([sender] of order-to-fill))]
    ifelse (quantity-to-deliver = ([quantity] of order-to-fill)) [ ;fulfilled whole order, delete it
      ask order-to-fill [die]
    ][ ;only filled it partially, adjust quantity 
      set ([quantity] of order-to-fill) ([quantity] of order-to-fill - quantity-to-deliver)
    ]
  ]
end

to-report customer-demand-step report order-n end
to-report customer-demand-square report order-n * ((floor (time / 26)) mod 2) end
to-report customer-demand-sine report round ((order-n / 2) * (1 + sin (360 * time / 52))) end
to-report customer-demand-random report random (order-n + 1) end

to-report calculate-stock-goal report stock-initial + order-initial + backorders - stock - round (pending / 3) end
to-report calculate-customer  report demand end
to-report calculate-sterman
  ifelse visibility?
    [set expected sum [ordered] of customers] ;total number of order all my customers placed at this time
    [set expected theta * old-demand + (1 - theta) * old-expected]
  let indicated expected + alpha * (Q - (stock - backorders) - beta * pending)
  set old-expected max list 0 round expected
;  if (who = 0) [
;    show " expected=" + expected + " indicated=" + indicated
;    show " inventory=" + (stock - backorders) + " pending=" + pending
;  ]
  report round indicated
end

;customer? places orders according to a fixed function
;factory? places shipping orders to itself to simulate a raw materials supplier
;distributor? places orders according to a formula: try to keep inventory at 12.
to place-orders
  let order-quantity 0
  if (customer?) [
    ;set order-quantity run-result ("customer-demand-" + customer-demand)
    set order-quantity ifelse-value (time > tickN) [run-result (word "customer-demand-" customer-demand)][order-initial]
  ]
;  let outstanding-orders my-orders with [state = "received"]
;  let unmet-demand (sum values-from outstanding-orders [quantity])
  ;show "unmet-demand=" + unmet-demand + " pending=" + pending
  if (distributor? or retailer? or factory?) [
    set order-quantity run-result (word "calculate-" order-style)
    ;show demand + " == " + unmet-demand
    ;set order-quantity stock-initial + order-initial + unmet-demand - stock - round (pending / 3) ; from Densmore simulation. their "inventory" = stock - unmet-demand
  ]
  set ordered max list order-quantity 0 ;for plotting later on
  if (order-quantity > 0) [
    if ((customer? and [working?] of seller) or (not customer?)) [;I am a customer with a working seller or I am not a customer.
      hatch-orders 1 [ ;make and place order
        set receiver ([seller] of myself) ;factory orders from itself. It will later turn this order into "intransit" to simulate a supplier.
        set sender myself
        ifelse ([factory?] of myself) [
          set state "sent" ;setting it to sent makes it take 3 steps to get back to the factory as input
;         set state "intransit" ;makes it take 2 steps
        ][
          set state "sent"
        ]
        set quantity order-quantity
        set sent-time time
        ifelse (show-orders? and not (sender = receiver)) [
          set heading towards receiver
          set size (to-size quantity)
          set color green 
          set shape "square"
          forward (distance receiver) / 3
        ][
          hide-turtle
        ]
      ]
      if (trace?) [show (word "Ordering " order-quantity  " from " seller " stock=" stock " demand=" demand)]
      set pending pending + order-quantity
    ]
    if (customer? and not [working?] of seller) [;Im a customer but my seller is down then increase my lost-orders
    set lost-orders lost-orders + order-quantity
    ]  
  ]
end


;-------------
;graph calculations
;
; The second measure is the clustering coefficient. This measures the amount of cliquishness 
; of the network, that is, the fraction of neighbouring nodes that are also connected to one another. For example, in an all-to-all 
; connected network, the clustering coefficient is one.
;
;Calculate distances to every other node using Dijistra's max-flow algorithm

;set distance to myself to be 0
to setup-distances
  set distances fput (list self 0) []
end

to-report assoc [element lst]
  foreach lst [
    if (first ? = element) [
      report ?]
  ]
  report []
end

to-report get-component
  foreach (components) [
    if (get-distance-to ? > -1) [
      report ?
    ]
  ]
  report nobody
end

to-report get-distance-to [theother]
  foreach distances [
    if (first ? = theother) [
      report (item 1 ?)
      ]
  ]
  report -1 
end

to set-distance-to [theother num]
  let already-there false
  set changed-distances? true
  foreach distances [
    if (first ? = theother) [
      without-interruption [ ;don't want someone else reading after delete but before addition
        set distances remove ? distances
        set distances fput (list other num) distances
        set already-there true
      ]
    ]
  ]
  if (not already-there) [
    set distances fput (list theother num) distances
  ]
end

;node updates its "distances" by
;Counting both in and out nodes
to update-distances
  let my-in-distances [distances] of (in-link-neighbors with [working?]) 
  set my-in-distances ifelse-value (empty? my-in-distances) [ [] ] [reduce [sentence ?1 ?2] my-in-distances]
  
  let my-out-distances [distances] of (out-link-neighbors with [working?]) 
  set my-out-distances ifelse-value (empty? my-out-distances) [ [] ][reduce [sentence ?1 ?2] my-out-distances]
  
  let n-distances sentence my-in-distances my-out-distances
;  show n-distances
  foreach n-distances [
    let mine get-distance-to (first ?)
    if (mine = -1 or item 1 ? + 1 < mine) [
      set-distance-to (first ?) (item 1 ? + 1)
    ]
  ]
end

;report my clustering coefficient: the fraction of my neighbors who are also connected to each other
;requires: 'distances' matrix must already be calculated
;assume bidirection, but this should probably change
to-report get-node-clustering-coefficient
  let neighbors-list remove-duplicates sentence ([self] of (in-link-neighbors with [working?])) 
                                                ([self] of (out-link-neighbors with [working?]))
  let pairs get-all-pairs neighbors-list
  let num-pairs-connected 0
  foreach pairs [
    ask (first ?) [
      if (get-distance-to (item 1 ?) = 1) [
        set num-pairs-connected num-pairs-connected + 1
      ]
    ]
  ]
  if (length pairs = 0) [report 1.0]
  report num-pairs-connected / length pairs
end


;;-----------------------------------------------------------------------------------------------
;Writing data to a file

to setup-output-file [f]
  set output-file f
end

;write information about the nodes to the file
to setup-write-to-file
  file-open output-file
  set list-of-nodes sort nodes
  file-print reduce [?1 + ?2] (map [(word ([who] of ?) " node" ",")] list-of-nodes)
  file-print butlast reduce [?1 + ?2] (map [word [node-type] of ? ","] list-of-nodes)
  set vars-to-check (list "inventory" "ordered" "demand" "backorders" "pending" "received" "cost")
  let line "time,"
  foreach vars-to-check [
    let v ?
    set line line + (reduce [?1 + ?2] (map [(word [who] of ? " " v ",")] list-of-nodes))
  ]
  file-print butlast line
end

to go-write-to-file
  file-open output-file
  let line word time ","
  foreach vars-to-check [
    let v ?
    set line line + (reduce [?1 + ?2] (map [run-result (word v "-of " ? ",")] list-of-nodes))
  ]
  file-print butlast line
end

;Exports the current topology (the nodes and their edges, plus height and xcor/ycor) to a file.
;The file contains the netlogo commands needed to re-create the topology.
;Also write file.png which is a picture of the topology.
to export-topology [file]
  file-open file
  foreach sort nodes [
    ask ? [
      ifelse (seller = self) [
        file-write (word "create-custom-nodes 1 [setxy " xcor " " ycor
                                          " set seller self set factory self set height 0]")
      ][
        file-write (word "create-custom-nodes 1 [setxy " xcor " " ycor
                                            " set seller " seller
                                            " __create-link-to " seller " [set color gray]" 
                                            " set height 1 + height-of " seller "]")]
      ]
  ]
  file-close
  export-view (word file ".png")
end

;Load the topology
to import-topology [file]
  ask nodes [die]
  file-open file
  while [not file-at-end?][
    run file-read
    ]
  file-close
end

;Just like setup but instead of creating a new topology we use the one from file
to setup-with-topology [file]
  let ofile output-file
  ca
  set output-file ofile
  setup-globals
  no-display
  import-topology file
  ;Set their color, shape, type, etc.
  ask nodes [
    set-type-one
  ]
  ask nodes [
    set-type-two
  ]
  ask nodes [
    setup-node
  ]
  setup-customers
  layout-radial nodes links factory 
  display
  set-current-plot "Edge Distribution"
  histogram [count my-in-links] of nodes 
  
  ;save seed so reset button will reproduce this run exactly
  set seed new-seed
  random-seed seed
  calculate-graph-values
end

;;;----------------------------------------------------------------------------------------
;Functions for drawing a network

to update-node-type
;Set their color, shape, type, etc.
  ask nodes [ set-type-one ]
  ask nodes [ set-type-two ]
  ask nodes [ setup-node ]
  setup-customers
  layout-radial nodes links factory 
  set-current-plot "Edge Distribution"
  histogram [count my-in-links] of nodes 
  calculate-graph-values
end

to add
  if (count nodes = 0) [
    setup-globals
    create-nodes 1
    ;set directed link to factory node
    set factory one-of nodes
    ask factory [
      set seller factory
      set height 0
    ]
    create-nodes 1 [
      set seller factory
      set height 1 + [height] of seller
      create-link-to seller [
        set color gray
      ]
    ]
    update-node-type
    ask patches [set pcolor white]
  ]
  ifelse (mouse-inside? and mouse-down?) [
    set chosen-one closest-xy mouse-xcor mouse-ycor nodes
    ask chosen-one [
      setxy mouse-xcor mouse-ycor
      set color red
    ]
  ][
    ifelse (chosen-one = 0) [
    ][ ;user has done a mouse-up (or not-inside)
     create-nodes 1 [
      set height 1 + [height] of chosen-one
      set seller chosen-one
      create-link-to chosen-one [
        set color gray
      ]
    ]
    update-node-type
    ask chosen-one [set color blue]
    set chosen-one 0
    set num-nodes count nodes
    ]
 ]
end

to delete
  if (count nodes = 0) [stop]
  ifelse (mouse-inside? and mouse-down?) [
    set chosen-one closest-xy mouse-xcor mouse-ycor nodes
    ask chosen-one [
      setxy mouse-xcor mouse-ycor
      set color red
    ]
  ][
    ifelse (chosen-one = 0) [
    ][ ;user has done a mouse-up (or not-inside)
     ask chosen-one [die]
     while [count nodes with [not is-turtle? seller] > 0][
         ask nodes with [not is-turtle? seller] [die]
     ]
     update-node-type
     set chosen-one 0
     set num-nodes count nodes
    ]
  ]
end
@#$#@#$#@
GRAPHICS-WINDOW
291
10
721
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

CC-WINDOW
5
709
941
804
Command Center
0

BUTTON
10
30
82
63
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
97
30
160
63
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

BUTTON
175
31
238
64
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

PLOT
367
463
611
646
Phase Plot
t
t-1
0.0
10.0
0.0
10.0
true
true
PENS
"F" 0.1 2 -2674135 true
"D" 1.0 2 -13345367 true
"R" 1.0 2 -10899396 true
"C" 1.0 2 -16777216 true

SLIDER
11
112
183
145
num-nodes
num-nodes
2
127
21
1
1
NIL
HORIZONTAL

SWITCH
723
45
882
78
show-orders?
show-orders?
1
1
-1000

SWITCH
723
79
904
112
show-packages?
show-packages?
1
1
-1000

CHOOSER
10
66
287
111
topology
topology
"fixed-degree-1" "fixed-degree-2" "fixed-degree-3" "preferential-attachment" "random-attachment" "fixed-preferential-attachment" "fixed-random-attachment" "fixed-cust-preferential-attachment" "fixed-cust-random-attachment"
3

CHOOSER
15
649
174
694
customer-demand
customer-demand
"step" "sine" "square" "random"
0

SWITCH
723
10
826
43
trace?
trace?
1
1
-1000

PLOT
722
237
926
403
Lost Orders
time
NIL
0.0
10.0
0.0
10.0
true
false
PENS
"default" 1.0 0 -16777216 true

PLOT
11
149
281
309
Edge Distribution
# in edges
#
0.0
7.0
0.0
5.0
true
false
PENS
"default" 1.0 1 -16777216 false

MONITOR
875
462
925
511
CPL
characteristic-path-length
2
1
12

MONITOR
875
513
932
562
CC
clustering-coefficient
2
1
12

CHOOSER
9
419
147
464
plot-variable
plot-variable
"stock" "inventory" "pending" "demand" "cost" "ordered" "received"
1

PLOT
7
463
365
647
Time Plot
time
plot-variable
0.0
10.0
0.0
10.0
true
true
PENS
"F" 1.0 0 -2674135 true
"D" 1.0 0 -13345367 true
"R" 1.0 0 -10899396 true
"C" 1.0 0 -16777216 true

CHOOSER
176
650
314
695
order-style
order-style
"stock-goal" "customer" "sterman"
2

SWITCH
315
650
443
683
visibility?
visibility?
1
1
-1000

SLIDER
443
650
535
683
alpha
alpha
0
1
0.3
0.01
1
NIL
HORIZONTAL

SLIDER
535
650
627
683
beta
beta
0
1
0.15
0.01
1
NIL
HORIZONTAL

SLIDER
627
650
719
683
theta
theta
0
1
0
0.01
1
NIL
HORIZONTAL

SLIDER
719
650
827
683
Q
Q
0
30
15
1
1
NIL
HORIZONTAL

SLIDER
11
345
183
378
prob-failure
prob-failure
0
0.2
0
0.01
1
NIL
HORIZONTAL

SLIDER
10
310
228
343
prob-targeted-attack
prob-targeted-attack
0
0.5
0
0.01
1
NIL
HORIZONTAL

SLIDER
10
382
182
415
mean-downtime
mean-downtime
0
50
1
1
1
NIL
HORIZONTAL

PLOT
613
462
873
644
Graph Attributes
time
NIL
0.0
10.0
0.0
10.0
true
true
PENS
"CPL" 1.0 0 -14835848 true
"CC" 1.0 0 -5825686 true

BUTTON
212
112
279
145
NIL
reset
NIL
1
T
OBSERVER
NIL
NIL
NIL
NIL

SLIDER
730
168
902
201
num-facilities
num-facilities
2
100
16
1
1
NIL
HORIZONTAL

SLIDER
729
202
903
235
num-customers
num-customers
1
100
14
1
1
NIL
HORIZONTAL

TEXTBOX
747
119
918
166
Used only when topology\nis fixed-*
12
0.0
0

BUTTON
225
346
288
379
NIL
add
T
1
T
OBSERVER
NIL
NIL
NIL
NIL

BUTTON
214
381
289
414
NIL
delete
T
1
T
OBSERVER
NIL
NIL
NIL
NIL

@#$#@#$#@
BEERNET
-------

by 
Jose M. Vidal and Anand Nair

An implementation of an automated beer game within a large network.
We test survivability and dynamic behavior of the supply network under various types of attacks or failures.
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
NetLogo 4.0.3
@#$#@#$#@
@#$#@#$#@
@#$#@#$#@
<experiments>
  <experiment name="Example" repetitions="1" runMetricsEveryStep="true">
    <setup>setup</setup>
    <go>go</go>
    <timeLimit steps="500"/>
    <metric>characteristic-path-length</metric>
    <metric>clustering-coefficient</metric>
    <metric>size-max-component</metric>
    <metric>max-distance</metric>
    <metric>sum [inventory] of nodes with [factory?]</metric>
    <metric>sum [inventory] of nodes with [distributor?]</metric>
    <metric>sum [inventory] of nodes with [retailer?]</metric>
    <metric>sum [cost] of nodes with [factory?]</metric>
    <metric>sum [cost] of nodes with [distributor?]</metric>
    <metric>sum [cost] of nodes with [retailer?]</metric>
    <metric>sum [backorders] of nodes with [factory?]</metric>
    <metric>sum [backorders] of nodes with [distributor?]</metric>
    <metric>sum [backorders] of nodes with [retailer?]</metric>
    <metric>sum [ordered] of nodes with [factory?]</metric>
    <metric>sum [ordered] of nodes with [distributor?]</metric>
    <metric>sum [ordered] of nodes with [retailer?]</metric>
    <metric>sum [ordered] of nodes with [customer?]</metric>
    <metric>sum [received] of nodes with [factory?]</metric>
    <metric>sum [received] of nodes with [distributor?]</metric>
    <metric>sum [received] of nodes with [retailer?]</metric>
    <metric>sum [received] of nodes with [customer?]</metric>
    <metric>sum [lost-orders] of nodes with [factory?]</metric>
    <metric>sum [lost-orders] of nodes with [distributor?]</metric>
    <metric>sum [lost-orders] of nodes with [retailer?]</metric>
    <metric>sum [lost-orders] of nodes with [customer?]</metric>
    <metric>sum [quantity] of (orders with [state = "intransit"])</metric>
    <metric>sum [quantity] of (orders with [state = "pending"])</metric>
    <metric>sum [demand] of nodes</metric>
    <metric>node-died?</metric>
    <enumeratedValueSet variable="prob-failure">
      <value value="0"/>
      <value value="0.25"/>
      <value value="0.5"/>
      <value value="0.75"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="trace?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="topology">
      <value value="&quot;fixed-degree-1&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="order-style">
      <value value="&quot;sterman&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="alpha">
      <value value="0.3"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Q">
      <value value="15"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="prob-targeted-attack">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="visibility?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="num-nodes">
      <value value="5"/>
      <value value="10"/>
      <value value="20"/>
      <value value="25"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="mean-downtime">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="show-packages?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="customer-demand">
      <value value="&quot;step&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="plot-variable">
      <value value="&quot;inventory&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="theta">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="show-orders?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="beta">
      <value value="0.15"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="Example with file" repetitions="10" runMetricsEveryStep="true">
    <setup>set output-file "ex1.csv"
setup
setup-write-to-file
</setup>
    <go>go-write-to-file
go</go>
    <timeLimit steps="52"/>
    <metric>characteristic-path-length</metric>
    <metric>clustering-coefficient</metric>
    <metric>size-max-component</metric>
    <metric>max-distance</metric>
    <metric>sum [inventory] of nodes with [factory?]</metric>
    <metric>sum [inventory] of nodes with [distributor?]</metric>
    <metric>sum [inventory] of nodes with [retailer?]</metric>
    <metric>sum [cost] of nodes with [factory?]</metric>
    <metric>sum [cost] of nodes with [distributor?]</metric>
    <metric>sum [cost] of nodes with [retailer?]</metric>
    <metric>sum [backorders] of nodes with [factory?]</metric>
    <metric>sum [backorders] of nodes with [distributor?]</metric>
    <metric>sum [backorders] of nodes with [retailer?]</metric>
    <metric>sum [ordered] of nodes with [factory?]</metric>
    <metric>sum [ordered] of nodes with [distributor?]</metric>
    <metric>sum [ordered] of nodes with [retailer?]</metric>
    <metric>sum [ordered] of nodes with [customer?]</metric>
    <metric>sum [received] of nodes with [factory?]</metric>
    <metric>sum [received] of nodes with [distributor?]</metric>
    <metric>sum [received] of nodes with [retailer?]</metric>
    <metric>sum [received] of nodes with [customer?]</metric>
    <metric>sum [lost-orders] of nodes with [factory?]</metric>
    <metric>sum [lost-orders] of nodes with [distributor?]</metric>
    <metric>sum [lost-orders] of nodes with [retailer?]</metric>
    <metric>sum [lost-orders] of nodes with [customer?]</metric>
    <metric>sum [quantity] of (orders with [state = "intransit"])</metric>
    <metric>sum [quantity] of (orders with [state = "pending"])</metric>
    <metric>sum [demand] of nodes</metric>
    <metric>node-died?</metric>
    <enumeratedValueSet variable="prob-failure">
      <value value="0"/>
      <value value="0.25"/>
      <value value="0.5"/>
      <value value="0.75"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="trace?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="topology">
      <value value="&quot;fixed-degree-2&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="order-style">
      <value value="&quot;sterman&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="alpha">
      <value value="0.3"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Q">
      <value value="15"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="prob-targeted-attack">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="visibility?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="num-nodes">
      <value value="5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="mean-downtime">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="show-packages?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="customer-demand">
      <value value="&quot;step&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="plot-variable">
      <value value="&quot;inventory&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="theta">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="show-orders?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="beta">
      <value value="0.15"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="Example1" repetitions="1" runMetricsEveryStep="true">
    <setup>setup</setup>
    <go>go</go>
    <timeLimit steps="500"/>
    <metric>characteristic-path-length</metric>
    <metric>clustering-coefficient</metric>
    <metric>size-max-component</metric>
    <metric>max-distance</metric>
    <metric>sum [inventory] of nodes with [factory?]</metric>
    <metric>sum [inventory] of nodes with [distributor?]</metric>
    <metric>sum [inventory] of nodes with [retailer?]</metric>
    <metric>sum [cost] of nodes with [factory?]</metric>
    <metric>sum [cost] of nodes with [distributor?]</metric>
    <metric>sum [cost] of nodes with [retailer?]</metric>
    <metric>sum [backorders] of nodes with [factory?]</metric>
    <metric>sum [backorders] of nodes with [distributor?]</metric>
    <metric>sum [backorders] of nodes with [retailer?]</metric>
    <metric>sum [ordered] of nodes with [factory?]</metric>
    <metric>sum [ordered] of nodes with [distributor?]</metric>
    <metric>sum [ordered] of nodes with [retailer?]</metric>
    <metric>sum [ordered] of nodes with [customer?]</metric>
    <metric>sum [received] of nodes with [factory?]</metric>
    <metric>sum [received] of nodes with [distributor?]</metric>
    <metric>sum [received] of nodes with [retailer?]</metric>
    <metric>sum [received] of nodes with [customer?]</metric>
    <metric>sum [lost-orders] of nodes with [factory?]</metric>
    <metric>sum [lost-orders] of nodes with [distributor?]</metric>
    <metric>sum [lost-orders] of nodes with [retailer?]</metric>
    <metric>sum [lost-orders] of nodes with [customer?]</metric>
    <metric>sum [quantity] of (orders with [state = "intransit"])</metric>
    <metric>sum [quantity] of (orders with [state = "pending"])</metric>
    <metric>sum [demand] of nodes</metric>
    <metric>node-died?</metric>
    <enumeratedValueSet variable="prob-failure">
      <value value="0"/>
      <value value="0.25"/>
      <value value="0.5"/>
      <value value="0.75"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="trace?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="topology">
      <value value="&quot;fixed-degree-1&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="order-style">
      <value value="&quot;sterman&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="alpha">
      <value value="0.3"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Q">
      <value value="15"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="prob-targeted-attack">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="visibility?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="num-nodes">
      <value value="5"/>
      <value value="10"/>
      <value value="20"/>
      <value value="25"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="mean-downtime">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="show-packages?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="customer-demand">
      <value value="&quot;step&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="plot-variable">
      <value value="&quot;inventory&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="theta">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="show-orders?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="beta">
      <value value="0.15"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="Example2" repetitions="1" runMetricsEveryStep="true">
    <setup>setup</setup>
    <go>go</go>
    <timeLimit steps="500"/>
    <metric>characteristic-path-length</metric>
    <metric>clustering-coefficient</metric>
    <metric>size-max-component</metric>
    <metric>max-distance</metric>
    <metric>sum [inventory] of nodes with [factory?]</metric>
    <metric>sum [inventory] of nodes with [distributor?]</metric>
    <metric>sum [inventory] of nodes with [retailer?]</metric>
    <metric>sum [cost] of nodes with [factory?]</metric>
    <metric>sum [cost] of nodes with [distributor?]</metric>
    <metric>sum [cost] of nodes with [retailer?]</metric>
    <metric>sum [backorders] of nodes with [factory?]</metric>
    <metric>sum [backorders] of nodes with [distributor?]</metric>
    <metric>sum [backorders] of nodes with [retailer?]</metric>
    <metric>sum [ordered] of nodes with [factory?]</metric>
    <metric>sum [ordered] of nodes with [distributor?]</metric>
    <metric>sum [ordered] of nodes with [retailer?]</metric>
    <metric>sum [ordered] of nodes with [customer?]</metric>
    <metric>sum [received] of nodes with [factory?]</metric>
    <metric>sum [received] of nodes with [distributor?]</metric>
    <metric>sum [received] of nodes with [retailer?]</metric>
    <metric>sum [received] of nodes with [customer?]</metric>
    <metric>sum [lost-orders] of nodes with [factory?]</metric>
    <metric>sum [lost-orders] of nodes with [distributor?]</metric>
    <metric>sum [lost-orders] of nodes with [retailer?]</metric>
    <metric>sum [lost-orders] of nodes with [customer?]</metric>
    <metric>sum [quantity] of (orders with [state = "intransit"])</metric>
    <metric>sum [quantity] of (orders with [state = "pending"])</metric>
    <metric>sum [demand] of nodes</metric>
    <metric>node-died?</metric>
    <enumeratedValueSet variable="prob-failure">
      <value value="0"/>
      <value value="0.25"/>
      <value value="0.5"/>
      <value value="0.75"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="trace?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="topology">
      <value value="&quot;fixed-degree-1&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="order-style">
      <value value="&quot;sterman&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="alpha">
      <value value="0.3"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Q">
      <value value="15"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="prob-targeted-attack">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="visibility?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="num-nodes">
      <value value="5"/>
      <value value="10"/>
      <value value="20"/>
      <value value="25"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="mean-downtime">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="show-packages?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="customer-demand">
      <value value="&quot;step&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="plot-variable">
      <value value="&quot;inventory&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="theta">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="show-orders?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="beta">
      <value value="0.15"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="Example3" repetitions="1" runMetricsEveryStep="true">
    <setup>setup</setup>
    <go>go</go>
    <timeLimit steps="500"/>
    <metric>characteristic-path-length</metric>
    <metric>clustering-coefficient</metric>
    <metric>size-max-component</metric>
    <metric>max-distance</metric>
    <metric>sum [inventory] of nodes with [factory?]</metric>
    <metric>sum [inventory] of nodes with [distributor?]</metric>
    <metric>sum [inventory] of nodes with [retailer?]</metric>
    <metric>sum [cost] of nodes with [factory?]</metric>
    <metric>sum [cost] of nodes with [distributor?]</metric>
    <metric>sum [cost] of nodes with [retailer?]</metric>
    <metric>sum [backorders] of nodes with [factory?]</metric>
    <metric>sum [backorders] of nodes with [distributor?]</metric>
    <metric>sum [backorders] of nodes with [retailer?]</metric>
    <metric>sum [ordered] of nodes with [factory?]</metric>
    <metric>sum [ordered] of nodes with [distributor?]</metric>
    <metric>sum [ordered] of nodes with [retailer?]</metric>
    <metric>sum [ordered] of nodes with [customer?]</metric>
    <metric>sum [received] of nodes with [factory?]</metric>
    <metric>sum [received] of nodes with [distributor?]</metric>
    <metric>sum [received] of nodes with [retailer?]</metric>
    <metric>sum [received] of nodes with [customer?]</metric>
    <metric>sum [lost-orders] of nodes with [factory?]</metric>
    <metric>sum [lost-orders] of nodes with [distributor?]</metric>
    <metric>sum [lost-orders] of nodes with [retailer?]</metric>
    <metric>sum [lost-orders] of nodes with [customer?]</metric>
    <metric>sum [quantity] of (orders with [state = "intransit"])</metric>
    <metric>sum [quantity] of (orders with [state = "pending"])</metric>
    <metric>sum [demand] of nodes</metric>
    <metric>node-died?</metric>
    <enumeratedValueSet variable="prob-failure">
      <value value="0"/>
      <value value="0.25"/>
      <value value="0.5"/>
      <value value="0.75"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="trace?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="topology">
      <value value="&quot;fixed-degree-1&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="order-style">
      <value value="&quot;sterman&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="alpha">
      <value value="0.3"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Q">
      <value value="15"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="prob-targeted-attack">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="visibility?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="num-nodes">
      <value value="5"/>
      <value value="10"/>
      <value value="20"/>
      <value value="25"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="mean-downtime">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="show-packages?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="customer-demand">
      <value value="&quot;step&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="plot-variable">
      <value value="&quot;inventory&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="theta">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="show-orders?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="beta">
      <value value="0.15"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="Example4" repetitions="1" runMetricsEveryStep="true">
    <setup>setup</setup>
    <go>go</go>
    <timeLimit steps="500"/>
    <metric>characteristic-path-length</metric>
    <metric>clustering-coefficient</metric>
    <metric>size-max-component</metric>
    <metric>max-distance</metric>
    <metric>sum [inventory] of nodes with [factory?]</metric>
    <metric>sum [inventory] of nodes with [distributor?]</metric>
    <metric>sum [inventory] of nodes with [retailer?]</metric>
    <metric>sum [cost] of nodes with [factory?]</metric>
    <metric>sum [cost] of nodes with [distributor?]</metric>
    <metric>sum [cost] of nodes with [retailer?]</metric>
    <metric>sum [backorders] of nodes with [factory?]</metric>
    <metric>sum [backorders] of nodes with [distributor?]</metric>
    <metric>sum [backorders] of nodes with [retailer?]</metric>
    <metric>sum [ordered] of nodes with [factory?]</metric>
    <metric>sum [ordered] of nodes with [distributor?]</metric>
    <metric>sum [ordered] of nodes with [retailer?]</metric>
    <metric>sum [ordered] of nodes with [customer?]</metric>
    <metric>sum [received] of nodes with [factory?]</metric>
    <metric>sum [received] of nodes with [distributor?]</metric>
    <metric>sum [received] of nodes with [retailer?]</metric>
    <metric>sum [received] of nodes with [customer?]</metric>
    <metric>sum [lost-orders] of nodes with [factory?]</metric>
    <metric>sum [lost-orders] of nodes with [distributor?]</metric>
    <metric>sum [lost-orders] of nodes with [retailer?]</metric>
    <metric>sum [lost-orders] of nodes with [customer?]</metric>
    <metric>sum [quantity] of (orders with [state = "intransit"])</metric>
    <metric>sum [quantity] of (orders with [state = "pending"])</metric>
    <metric>sum [demand] of nodes</metric>
    <metric>node-died?</metric>
    <enumeratedValueSet variable="prob-failure">
      <value value="0"/>
      <value value="0.25"/>
      <value value="0.5"/>
      <value value="0.75"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="trace?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="topology">
      <value value="&quot;fixed-degree-1&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="order-style">
      <value value="&quot;sterman&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="alpha">
      <value value="0.3"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Q">
      <value value="15"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="prob-targeted-attack">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="visibility?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="num-nodes">
      <value value="5"/>
      <value value="10"/>
      <value value="20"/>
      <value value="25"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="mean-downtime">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="show-packages?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="customer-demand">
      <value value="&quot;step&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="plot-variable">
      <value value="&quot;inventory&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="theta">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="show-orders?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="beta">
      <value value="0.15"/>
    </enumeratedValueSet>
  </experiment>
</experiments>
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
