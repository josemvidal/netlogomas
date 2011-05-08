;global variables
globals [ clock                        ;time steps 
          cars-size                    ;the size (height as well as width) of the vehicles
          lane-width                   ;the width of the lanes
          speed-limit                  ;speed limit for the vehicles
          total-finished-cars          ;number of cars that have finished their trip
          
          total-cars                   ;number of cars that have been spawned (finished and unfinished)
          active-cars                  ;number of cars that are still traveling
          maximum-delay                ;the max value that a car has been delayed
          expected-maximum-delay       ;the expected value for maximum-delay

          total-delay                 ;the sum of the delays of all the cars that have finished their trip
          expected-average-delay       ;the expected value for the average delay                            
          tiles                       ;a matrix representing the tiles in the intersection when using Reservation System model
          tile-width                  ;the width of the tiles in the intersection when using Reservation System model
          tile-height                 ;the height of the tiles in the intersection when using Reservation System model
          green-breed                 ;the turtle breed that has the green light (north, east, or none)
          second                      ;Is 1 / ticks-by-second
          
          ;;Patches working as lights
          north-ligths
          east-lights
          west-lights
          south-ligths

          ;;They are some list used in the experiment 3
          list-Period10
          list-Period30
          list-Period50
          list-reservation          
        ]
turtles-own [ speed max-speed delay acceleration acceleration-step deceleration-step                   ;speed and acceleration related variables
              reservation-made reservation-time reservation-xcor reservation-ycor reservation-speed    ;reservation related variables
              reservation-tiles
              traveled-distance ;;The distance the car has traveled.
            ]
breeds [ east west north south ]



;;;;;;;;;;;;;;;;;;;;;;;
;;SETUP PROCEDURES;;
;;;;;;;;;;;;;;;;;;;;;;; 
   
to setup
  ;;setup button
  ca  
  start-variables
end

to start-variables
  ;;set global variables to their initial values
  set clock 0 
  set lane-width 4  
  set cars-size 4
  set tile-width (lanes * 2 * lane-width) / granularity
  set tile-height tile-width
  set tiles n-values granularity [(list -1)]
  set tiles n-values granularity [tiles]
  set speed-limit 27
  set total-delay 0
  set total-cars 0
  set active-cars 0
  set maximum-delay 0
  set second 1 / ticks-by-second
  set total-finished-cars 0
  set expected-maximum-delay (1 - alpha) * period
  set expected-average-delay (1 / 2) * ((1 - alpha) ^ 2) * period  
  draw-lanes
  let light-height lanes * lane-width
  set north-ligths patches with [ pxcor <= 0 and pxcor >= (- light-height) and pycor = light-height ]
  set east-lights patches with [ pxcor = (- light-height) and pycor <= 0 and pycor >= (- light-height) ]
  set west-lights patches with [ pxcor = light-height and pycor >= 0 and pycor <= light-height ] 
  set south-ligths patches with [ pxcor >= 0 and pxcor <= light-height and pycor = (- light-height) ]  
  if (model = "Trafic Light") [ 
    draw-lights 
    ifelse green-light north  
    [set green-breed north]
    [
      ifelse green-light east [set green-breed east]
      [set green-breed "none"]
    ]
  ]
end
 
to draw-lanes
  ;; draws the lanes
  ask patches
  [ set pcolor green - 1
    if (abs pxcor <= lane-width * lanes or abs pycor <= lane-width * lanes)
    [ set pcolor black ]
  ]
end

to draw-lights
  ;; draws the lights in the intersection when using Traffic Light model
  let color-east-west black
  let color-north-south black
  ifelse green-light north
  [ set color-north-south green 
    set color-east-west red
  ]
  [ 
    ifelse green-light east
    [ set color-north-south red 
      set color-east-west green
    ]
    [if green-light "none" 
    [
      set color-east-west red
      set color-north-south red      
    ]]
  ]
  
  let light-height lanes * lane-width   
  ask east-lights [set pcolor color-east-west] ;draw east light
  ask west-lights [set pcolor color-east-west]         ;draw west light
  ask north-ligths [set pcolor color-north-south]   ;draw north light
  ask south-ligths [set pcolor color-north-south]   ;draw south light
end

to go  
  ;;go button
  set second 1 / ticks-by-second
  without-interruption [ 
  spawn-cars        ;spawns new cars using the spawn probabilities                                           
  move-cars         ;move the cars, performs one time step
  set clock clock + 1  ;increases the time steps counter
  if (model = "Trafic Light") 
  [   
   ; checks when lights have to be redrawn when using Trafic Light model
     if not green-light green-breed [
      ifelse green-light north  
      [set green-breed north]
      [
        ifelse green-light east [set green-breed east]
        [set green-breed "none"]
      ]
      draw-lights
     ]
  ]  
   if plotting [ do-plotting1]
   ]
end

to move-cars
  ;;move the cars, performs one time step
  ask turtles [ move ]
end
   
to spawn-cars
  ;;spawns new cars using the spawn probabilities
  
  ;spawn cars in east direction
    let ycor-east (- ((lane-width / 2) + ((random lanes) * lane-width)))
    if (random-float 100 < spawn-probability-E * 100) and not any? turtles-at (- screen-edge-x) ycor-east
    [ 
      create-custom-east 1
        [ set xcor (- screen-edge-x)
          set ycor ycor-east          
          set heading 90
          set-common-values
          ifelse not should-spawn [ die]
          [ 
            set total-cars total-cars + 1
            set active-cars active-cars + 1
          ]
        ]
    ]

  ;spawn cars in west direction
    let ycor-west ((lane-width / 2) + ((random lanes) * lane-width))    
    if (random-float 100 < spawn-probability-W * 100) and not any? turtles-at (- screen-edge-x) ycor-west
    [ 
      create-custom-west 1
        [ set xcor (screen-edge-x)
          set ycor ycor-west
          set heading 270
          set-common-values
          ifelse not should-spawn [ die]
          [ 
            set total-cars total-cars + 1
            set active-cars active-cars + 1
          ]
        ]

    ]
  
  ;spawn cars in north direction            
  let xcor-north ((lane-width / 2) + ((random lanes) * lane-width))
  if (random-float 100 < spawn-probability-N * 100) and not any? turtles-at xcor-north (- screen-edge-y)
    [ 
      create-custom-north 1
        [ set xcor xcor-north
          set ycor (- screen-edge-y)
          set heading 0
          set-common-values
          ifelse not should-spawn [ die]
          [ 
            set total-cars total-cars + 1
            set active-cars active-cars + 1
          ]
        ]
    ]

  ;spawn cars in south direction
  let xcor-south (- ((lane-width / 2) + ((random lanes) * lane-width)))    ;(- ((random lanes) + 1) * lane-width / 2)
  if (random-float 100 < spawn-probability-S * 100) and not any? turtles-at 0 (- screen-edge-y)
    [ 
      create-custom-south 1
        [ set xcor xcor-south
          set ycor (screen-edge-y)
          set heading 180
          set-common-values
          ifelse not should-spawn [ die]
          [ 
            set total-cars total-cars + 1
            set active-cars active-cars + 1
          ]
        ]
    ]
end

to-report should-spawn
  ;; reports true if the vehicle can be spawn without a collision danger (due to a vehicle ahead too close)
  ;; false otherwise
  report (not vehicle-ahead-too-close)
end

to set-common-values
  ;; set common values for turtles variables
          set shape "car"
          set color blue
          set size cars-size
          set max-speed 50
          set delay 0 
          set speed min (list max-speed speed-limit)
          set acceleration 0
          set acceleration-step 3
          set deceleration-step 15
          set reservation-made false
          set traveled-distance 0
end



;;;;;;;;;;;;;;;;;;;;;;;
;;TURTLES PROCEDURES;;
;;;;;;;;;;;;;;;;;;;;;;;

to move
;;move the car one time step

  ;;behavior of the driver agent (steps in paper)
    coast
    if speed < speed-limit   ;and green-light breed
      [accelerate]
    if vehicle-ahead-too-close
      [decelerate]  
    if (model = "Trafic Light") or (model = "Reservation") [ interact-with-intersection ]
    interact-with-physics
  
  ;; sets delay value for the car
    set delay delay + (second - (speed * second) / min(list max-speed speed-limit))   
  
  ;; sets the speed for the vehicle, according to the acceleration value set in the steps above (behavior of the agent)
    let d1 speed + acceleration * second
    ifelse (d1 > max-speed)
            [
                set speed max-speed
            ]
    [   
            ifelse(d1 < 0.0)  
                [set speed 0.0]            
                [set speed d1] 
    ]  
    
    ;; moves one secondth of second (1 second = tikcs-by-second timesteps) in the paper it is a 1/50 
      fd speed * second
      set traveled-distance traveled-distance + speed * second
      
    ;; checks for vehicles that have to be removed from the screen (finished)
      ;;if (breed = east and pxcor >= screen-edge-x) or (breed = west and pxcor <= (- screen-edge-x)) or (breed = north and pycor >= screen-edge-y) or (breed = south and pycor <= (- screen-edge-y))
      if traveled-distance >= screen-size-x
      [
        ;turtle have left the domain of the simulator        
          set total-finished-cars total-finished-cars + 1 
          set active-cars active-cars - 1  
          set total-delay total-delay + delay
          if delay > maximum-delay [set maximum-delay delay]               
  
        ;; kill the turtle
        die 
      ]   
end

to interact-with-intersection
;; interacts with intersection as specified for each control policy in section 5 of paper
  if speed > 0
  [
  if distance-to-intersection >= 0 
  [
    ifelse (model = "Trafic Light")    
    [
    ;; Traffic Light model control policies
        ifelse (any? turtles-on patch-ahead (cars-size)) and (not green-light breed) ;
        [decelerate]
        [
        ifelse (distance-to-intersection < cars-size) and (not green-light breed) 
          [decelerate]
          [
          if distance-to-intersection > 0
          [
            let d1 distance-to-intersection / speed
            let d2 next-green-period d1
            let current-time (clock / (1 / second))
            if d2 != current-time + d1
            [ 
              if distance-to-intersection <= speed * second + (speed * speed) / (deceleration-step)
                [decelerate]          
            ]      
          ]
        ]]
     ]
     [
     ;; Reservation System model control policies
         ifelse (reservation-made) 
         [
         ifelse (can-keep-reservation) 
           [ coast ]
           [ cancel-reservation    
             make-reservation         
           ]
         ]
         [ make-reservation ]
     ]
   ]]
end
   
to interact-with-physics
;; some needed rules 
        if (speed = max-speed) and (acceleration > 0)
            [coast]
        if (speed = 0) and (acceleration < 0)
            [coast]
        if (speed > speed-limit)
            [decelerate]
end

to-report distance-to-intersection
;; reports the distance from this turtle to the beggining of the intersection
  let result 1000000
  let intersection-width lanes * lane-width
  if breed = north
    [ set result distancexy xcor (- intersection-width)
      if ycor > (- intersection-width) [set result (- result)]
    ]
  if breed = south
    [ set result distancexy xcor intersection-width
      if ycor < intersection-width [set result (- result)]
    ]    
  if breed = east
    [ set result distancexy (- intersection-width) ycor
      if xcor > (- intersection-width) [set result (- result)]
    ]    
  if breed = west
    [ set result distancexy intersection-width ycor
      if xcor < intersection-width [set result (- result)]
    ]   
  report result     
end

to-report can-keep-reservation
;; reports true if the vehicle can keep the reservation it has already made
;; false otherwise
  let result false
  let d1 speed * second
  let d2 distance-to-intersection mod d1
  let j distance-to-intersection / d1
  let i j + clock
  let d speed
  let sigo true
  if (vehicle-ahead != nobody) [if (i <= reservation-time-of vehicle-ahead) [ set sigo false ]]
  if sigo
      [
        ifelse (abs (d - reservation-speed) < 0.001) and  (abs (i - reservation-time) < 1)
          [ set result true ]
          [ set result false ]
       ]
  report result
end

to cancel-reservation
;; called when the vehicle determined that it can not keep the reservation
;; cancels the reservation it has already made
  set reservation-made false
  foreach reservation-tiles
  [
    ;; make the tiles in the intersection available at the times the vehicle had a reservation on them
       set tiles replace-item (item 0 ?) tiles (replace-item (item 1 ?) (item (item 0 ?) tiles) remove (item 2 ?) (item (item 1 ?) (item (item 0 ?) tiles)))
  ]
end

to make-reservation
;; reservation making process
  let vahead vehicle-ahead
  let sigo true
  if (vahead != nobody) [set sigo reservation-made-of vahead]
  if sigo
  [
      ifelse (request-reservation)
        [ coast ]        ;; if the proccess of requesting a reservation to the intersection is successful, the vehicle keep the same speed
        [ decelerate ]   ;; otherwise, decelerate and it will try again at the next time step
  ]
end

to-report request-reservation
;; proccess of requesting a reservation to the intersection 
;; reports true if a reservation can be made
;; false otherwise
        let result false
        let d1 speed * second
        let d2 distance-to-intersection mod d1
        let steps-to-intersection floor (distance-to-intersection / d1)
        let i steps-to-intersection + clock
        let xpoint-before-reaching-intersection xcor
        let ypoint-before-reaching-intersection ycor
        if breed = north
            [ set ypoint-before-reaching-intersection (- (lanes * lane-width + d2)) ]
        if breed = east
            [ set xpoint-before-reaching-intersection (- (lanes * lane-width + d2)) ]
        if breed = south
            [ set ypoint-before-reaching-intersection lanes * lane-width + d2 ]
        if breed = west
            [ set xpoint-before-reaching-intersection lanes * lane-width + d2 ]

        let sigo true
        if (vehicle-ahead != nobody) [if (i <= reservation-time-of vehicle-ahead) [ set sigo false ]]
        if sigo
          [
            let tiles-to-reserve intersection-is-free i speed xpoint-before-reaching-intersection ypoint-before-reaching-intersection
            ifelse empty? tiles-to-reserve
            [ 
              ;; reservation cannot be made
              set result false 
            ]  
            [
              ;; reservation can be made
              ;; sets the reservation variables of the turtle and makes the reservation of the tiles
              set reservation-time i
              set reservation-xcor xpoint-before-reaching-intersection
              set reservation-ycor ypoint-before-reaching-intersection
              set reservation-speed speed
              set reservation-made true
              set reservation-tiles tiles-to-reserve
              reserve tiles-to-reserve      ;makes the reservation of the tiles
              set result true      
            ]            
          ]
        report result
end 

to reserve [list-tiles-to-reserve]
;; reserves the tiles in the intersection at the time the vehicle will pass by them
   foreach list-tiles-to-reserve
   [
     set tiles replace-item (item 0 ?) tiles (replace-item (item 1 ?) (item (item 0 ?) tiles) lput (item 2 ?) (item (item 1 ?) (item (item 0 ?) tiles)))    
   ]
end

to-report vehicle-ahead
;; reports the vehicle ahead of the calling turtle
  let result 0
  let turtles-ahead 0
  let myxcor xcor
  let myycor ycor
  if breed = south
  [
    set turtles-ahead turtles with [(xcor = myxcor) and (ycor < myycor)]
    set result max-one-of turtles-ahead [ycor]
  ]
  if breed = north
  [
    set turtles-ahead turtles with [(xcor = myxcor) and (ycor > myycor)]
    set result min-one-of turtles-ahead [ycor]
  ]
  if breed = east
  [
    set turtles-ahead turtles with [(ycor = myycor) and (xcor > myxcor)]
    set result min-one-of turtles-ahead [xcor]
  ]  
  if breed = west
  [
    set turtles-ahead turtles with [(ycor = myycor) and (xcor < myxcor)]
    set result max-one-of turtles-ahead [xcor]
  ]  
  report result
end 

to-report intersection-is-free [time d xpoint-before-reaching-intersection ypoint-before-reaching-intersection]
;; if the tiles in the intersection that need to be reserved by the calling vehicle are free 
;; at the time the vehicle will cross them, this reporter reports a list containing 
;; the tiles and times of reservation
;; it reports an empty list otherwise
  let result []
  let free true
  let time-test time
  let x (- lanes * lane-width)  ;xcor of top-left corner of intersection
  let y lanes * lane-width ;ycor of top-left corner of intersection
  if breed = north
  [
            let xleft xpoint-before-reaching-intersection - cars-size / 2
            let xright xpoint-before-reaching-intersection + cars-size / 2
            let left-tile-number max (list 0 (floor ((xleft - x) / tile-width)) )
            let right-tile-number min (list (granularity - 1) (floor ((xright - x) / tile-width)) )
            let ytop 0
            let ybottom 0
            let top-tile-number 0
            let bottom-tile-number 0
            let counter1 0
            let counter2 0
            let yaux ypoint-before-reaching-intersection
            while [yaux - cars-size <= y]     ;/ 2
            [       
                set ytop yaux + cars-size / 2
                set ybottom yaux - cars-size / 2
                set top-tile-number max (list 0 floor ((y - ytop) / tile-height))
                set bottom-tile-number min (list (granularity - 1) floor ((y - ybottom) / tile-height))
                set counter1 top-tile-number                
                repeat bottom-tile-number - top-tile-number + 1
                [                                
                  set counter2 left-tile-number                  
                  repeat right-tile-number - left-tile-number + 1
                  [
                    if (counter2 <= granularity - 1) and (counter1 <= granularity - 1) 
                    [
                        set result lput (list counter2 counter1 time-test) result 
                        if (reserved-tiles counter2 counter1 time-test)
                            [set free false]        ;; if there is a tile already reserved, the reservation cannot be made
                    ]
                        set counter2 counter2 + 1
                  ]
                  set counter1 counter1 + 1
                ]
                set yaux yaux + d * second
                set time-test time-test + 1
            ]
  ]
  if breed = south
  [
            let xleft xpoint-before-reaching-intersection - cars-size / 2
            let xright xpoint-before-reaching-intersection + cars-size / 2
            let left-tile-number max (list 0 (floor ((xleft - x) / tile-width))  )
            let right-tile-number min (list (granularity - 1) (floor ((xright - x) / tile-width)) )
            let ytop 0
            let ybottom 0
            let top-tile-number 0
            let bottom-tile-number 0
            let counter1 0
            let counter2 0
            let yaux ypoint-before-reaching-intersection
            while [yaux + cars-size >= y - (lanes * 2 * lane-width)]     ;/ 2
            [                            
                set ytop yaux + cars-size / 2
                set ybottom yaux - cars-size / 2
                set top-tile-number max (list 0 floor ((y - ytop) / tile-height))
                set bottom-tile-number min (list (granularity - 1) floor ((y - ybottom) / tile-height))
                set counter1 top-tile-number                
                repeat bottom-tile-number - top-tile-number + 1
                [
                  set counter2 left-tile-number
                  repeat right-tile-number - left-tile-number + 1
                  [
                    if (counter2 <= granularity - 1) and (counter1 <= granularity - 1) 
                    [
                        set result lput (list counter2 counter1 time-test) result 
                        if (reserved-tiles counter2 counter1 time-test)
                            [set free false]    ;; if there is a tile already reserved, the reservation cannot be made
                    ]
                        set counter2 counter2 + 1
                  ]
                  set counter1 counter1 + 1
                ]
                set yaux yaux - d * second
                set time-test time-test + 1
            ]
  ]  
  if breed = east
  [  
            let xaux xpoint-before-reaching-intersection
            let ytop ypoint-before-reaching-intersection + cars-size / 2
            let ybottom ypoint-before-reaching-intersection - cars-size / 2
            let top-tile-number max (list 0 floor ((y - ytop) / tile-height))
            let bottom-tile-number min (list (granularity - 1) floor ((y - ybottom) / tile-height))
            let xleft 0
            let xright 0
            let left-tile-number 0
            let right-tile-number 0
            let counter1 0
            let counter2 0
            while [xaux - cars-size <= x + lanes * 2 * lane-width]
            [
                set xleft xaux - cars-size / 2
                set xright xaux + cars-size / 2
                set left-tile-number max (list 0 floor((xleft - x) / tile-width))
                set right-tile-number min (list (granularity - 1) floor((xright - x) / tile-width))
                set counter1 top-tile-number                
                repeat bottom-tile-number - top-tile-number + 1
                [
                  set counter2 left-tile-number
                  repeat right-tile-number - left-tile-number + 1
                  [
                    if (counter2 <= granularity - 1) and (counter1 <= granularity - 1) 
                    [
                        set result lput (list counter2 counter1 time-test) result 
                        if (reserved-tiles counter2 counter1 time-test)
                            [set free false]      ;; if there is a tile already reserved, the reservation cannot be made
                    ]
                        set counter2 counter2 + 1
                  ]
                  set counter1 counter1 + 1
                ]
                set xaux xaux + d * second
                set time-test time-test + 1
          ]
  ]

  if breed = west
  [  
            let xaux xpoint-before-reaching-intersection
            let ytop ypoint-before-reaching-intersection + cars-size / 2
            let ybottom ypoint-before-reaching-intersection - cars-size / 2            
            let top-tile-number max (list 0 floor ((y - ytop) / tile-height))
            let bottom-tile-number min (list (granularity - 1) floor ((y - ybottom) / tile-height))
            let xleft 0
            let xright 0
            let left-tile-number 0
            let right-tile-number 0
            let counter1 0
            let counter2 0
            while [xaux + cars-size / 2 >= x]
            [
                set xleft xaux - cars-size / 2
                set xright xaux + cars-size / 2
                set left-tile-number max (list 0 floor((xleft - x) / tile-width))
                set right-tile-number min (list (granularity - 1) floor((xright - x) / tile-width))
                set counter1 top-tile-number   
                repeat bottom-tile-number - top-tile-number + 1
                [
                  set counter2 left-tile-number
                  repeat right-tile-number - left-tile-number + 1
                  [
                    if (counter2 <= granularity - 1) and (counter1 <= granularity - 1) 
                    [
                        set result lput (list counter2 counter1 time-test) result 
                        if (reserved-tiles counter2 counter1 time-test)
                            [set free false]      ;; if there is a tile already reserved, the reservation cannot be made
                    ]
                        set counter2 counter2 + 1
                  ]
                  set counter1 counter1 + 1
                ]
                set xaux xaux - d * second
                set time-test time-test + 1
          ]
  ]
  if not free [set result []]
  report result
end

to-report reserved-tiles [x-num-tile y-num-tile time]
;; reports true if the tile (x-num-tile, y-num-tile) is reserved at the given time
;; false otherwise   
    let times-reserved item y-num-tile (item x-num-tile tiles)
    report member? time times-reserved 
end      
   
to-report next-green-period [param1]
;; reports the next time step at which the light will be green for the caller turtle 
;; param1 = distance-to-intersection / speed
  let result 100000000
  let current-time (clock / (1 / second))
  let d (current-time + param1) mod period
  ifelse (breed = north) or (breed = south)
  [
    ifelse d < (alpha * period)
      [ set result current-time + param1 ]
      [ set result current-time + param1 + period - d ]
  ]
  [ ifelse (d >= ((alpha + beta) * period)) and (d < ((1.0 - beta) * period))
    [ set result current-time + param1 ]
    [
      ifelse (d < (alpha + beta) * period)
      [ set result current-time + param1 + ((alpha + beta) * period - d)]
      [ set result current-time + param1 + (period - d) + (alpha + beta) * period]
    ]
  ]  
  report result
end 

to-report green-light [turtle-breed]
;; reports true if the light is green for the "turtle-breed" direction
;; false otherwise
  let result false
  let current-time (clock / (1 / second))
  let d current-time mod period
  ifelse (turtle-breed = north) or (turtle-breed = south) 
  [set result (d < alpha * period)]
  [
    ifelse (turtle-breed = east) or (turtle-breed = west) 
    [set result ((d >= (alpha + beta) * period) and (d < (1.0 - beta) * period))]
    [if (turtle-breed = "none") [set result (not (d < alpha * period)) and (not ((d >= (alpha + beta) * period) and (d < (1.0 - beta) * period)))]]
  ]    
  report result
end 

to-report vehicle-ahead-too-close
;; reports true if the vehicle ahead of the caller if within "speed" distance (one second) ahead
;; false otherwise
  let result false
  let x 1
  repeat speed 
  [ 
    if (any? turtles-on patch-ahead x)
      [set result true] 
    set x x + 1 
  ] 
  report result
end
       
to accelerate
;; set acceleration to the acceleration-step
  set acceleration acceleration-step
end

to coast
;; set acceleration to 0 
  set acceleration 0
end

to decelerate
;; set acceleration to deceleration-step
  set acceleration (- 1 * deceleration-step)
end


;;;;;;;;;;;;;;;;;;;;;;;;;;
;;EXPERIMENTS PROCEDURES;;
;;;;;;;;;;;;;;;;;;;;;;;;;;

to experiment1
  set lanes  1
  set plotting false
  set model "Trafic Light"
  set period 10
  set beta 0
  set alpha 0.45    
  set spawn-probability-N  .001
  set spawn-probability-S  .001 
  set spawn-probability-E  0
  set spawn-probability-W  0 
  set ticks-by-second 50
  setup  
  set-current-plot "Figure a"  
  set-plot-x-range 10 200
  set-current-plot "Figure b"
  set-plot-x-range 10 200       
  while [period <= 200]   [     
    start-variables       
    ask turtles [die]
     while [clock <= 20000] [
        go      
     ] 
    do-plotting
   set period period + 1
  ]
  
end



to experiment2  
  set lanes  1
  set plotting false  
  set model "Trafic Light"
  set period 30
  set beta 0
  set alpha 0.1
  set spawn-probability-N  .001
  set spawn-probability-S  .001 
  set spawn-probability-E  0
  set spawn-probability-W  0
  set ticks-by-second 50   
  setup
  set-current-plot "Figure a"    
  set-plot-x-range 0.10 .89
  set-current-plot-pen "Theoretical" 
  set-plot-pen-interval 0.1
  set-current-plot-pen "Empirical"  
  set-plot-pen-interval 0.1
  set-current-plot "Figure b"
  set-plot-x-range 0.10 .89   
  set-current-plot-pen "Theoretical" 
  set-plot-pen-interval 0.1
  set-current-plot-pen "Empirical"  
  set-plot-pen-interval 0.1
  while [alpha <= .89]
  [
    start-variables 
    ask turtles [die]
     while [clock <= 20000] [
        go        
     ] 
    do-plotting  
   set alpha alpha + .1
  ]
end

to experiment3 
  set lanes  1
  set plotting false
 ;;Lists with the results of each experiment
  
  set model "Trafic Light"
  set period 10
  set beta 0 
  set alpha 0.45  
  set spawn-probability-N  .001
  set spawn-probability-S  .001 
  set spawn-probability-E  0
  set spawn-probability-W  0
  set ticks-by-second 50  
  setup     
  set list-Period10 []
  while [spawn-probability-N <= .02 ]
  [
    start-variables  
    ask turtles [die]
     while [clock <= 20000] [
        go              
     ] 
   set spawn-probability-N spawn-probability-N + .001
   set spawn-probability-S spawn-probability-N  
   set list-Period10 lput (total-delay / total-finished-cars) list-Period10
  ]
     

  set model "Trafic Light"
  set period 30
  set beta 0
  set alpha 0.45  
  set spawn-probability-N  .001
  set spawn-probability-S  .001 
  set spawn-probability-E  0
  set spawn-probability-W  0
  set ticks-by-second 50  
  ask turtles [die]        
  set list-Period30 []
  while [spawn-probability-N <= .02] 
  [
    start-variables  
    ask turtles [die]
     while [clock <= 20000] [
        go      
     ] 
   set spawn-probability-N spawn-probability-N + .001
   set spawn-probability-S spawn-probability-N + .001
   set list-Period30 lput (total-delay / total-finished-cars) list-Period30
  ] 
  
 
  set model "Trafic Light"
  set period 50
  set beta 0
  set alpha 0.45  
  set spawn-probability-N  .001
  set spawn-probability-S  .001 
  set spawn-probability-E  0
  set spawn-probability-W  0
  set ticks-by-second 50  
  ask turtles [die]    
  set list-Period50 [] 
  while [spawn-probability-N <= .02] 
  [
    start-variables  
    ask turtles [die]
     while [clock <= 20000] [
        go        
     ] 
   set spawn-probability-N spawn-probability-N + .001
   set spawn-probability-S spawn-probability-N 
   set list-Period50 lput (total-delay / total-finished-cars) list-Period50
  ]  
  
 
  set model "Reservation" 
  set granularity 1 
  set period 30
  set beta 0 
  set alpha 0.1
  set spawn-probability-N  .001
  set spawn-probability-S  .001 
  set spawn-probability-E  .0 
  set spawn-probability-W  .0 
  set ticks-by-second 50   
  ask turtles [die]     
  set list-reservation []
  while [spawn-probability-N <= .02]
  [
    start-variables 
    ask turtles [die]
     while [clock <= 20000] [
          go                
     ] 
   set spawn-probability-N spawn-probability-N + .001
   set spawn-probability-S  spawn-probability-N   
   set list-reservation lput (total-delay / total-finished-cars) list-reservation 
  ]
  do-plotting3 
end

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;
to experiment4 
  set lanes  1
  set plotting false
  set model "Reservation" 
  set granularity 1 
  set period 30
  set beta 0 
  set alpha 0.1
  set spawn-probability-N  .001
  set spawn-probability-S  .001 
  set spawn-probability-E  .000 
  set spawn-probability-W  .000  
    set ticks-by-second 50
  setup 
  while [spawn-probability-N <= .02]
  [
    start-variables 
    ask turtles [die]
     while [clock <= 20000] [   
        go                     
     ] 
    do-plotting2a 
   set spawn-probability-N spawn-probability-N + .001
   set spawn-probability-S  spawn-probability-N    
  ]
  
  set granularity 2  
  set period 30 
  set beta 0
  set alpha 0.1
  set spawn-probability-N  .001
  set spawn-probability-S  .001 
  set spawn-probability-E  .001 
  set spawn-probability-W  .001 
  set ticks-by-second 50
  ask turtles [die]     
  while [spawn-probability-N <= .02]
  [
    start-variables 
    ask turtles [die]
     while [clock <= 20000] [
        go        
     ]  
    do-plotting2a
   set spawn-probability-N spawn-probability-N + .001
   set spawn-probability-S  spawn-probability-N   
  ]  
end

;;;;;;;;;;;;;;;;;;;;;;;
;;PLOTTING PROCEDURES;;
;;;;;;;;;;;;;;;;;;;;;;;
to do-plotting1
  set-current-plot "Average Delay"  
  set-current-plot-pen "default" 
  ifelse total-finished-cars > 0 [ plot total-delay / total-finished-cars]
  [plot 0]  
  set-current-plot "Maximum Delay" 
  plot maximum-delay 
end

to do-plotting 
  set-current-plot "Figure a"  
  set-current-plot-pen "Theoretical" 
  plot expected-average-delay
  set-current-plot-pen "Empirical"
  plot total-delay / total-finished-cars

  set-current-plot "Figure b" 
  set-current-plot-pen "Theoretical" 
  plot expected-maximum-delay
  set-current-plot-pen "Empirical"
  plot maximum-delay 
end


to do-plotting2a
  set-current-plot "Figure c"  
  set-current-plot-pen "Granu. 1" 
  plot total-delay / total-finished-cars
end

to do-plotting2b
  set-current-plot "Figure c"  
  set-current-plot-pen "Granu. 2" 
  plot total-delay / total-finished-cars
end

to do-plotting3
  set-current-plot "Figure d"  
  foreach list-period10 [
    set-current-plot-pen "Period 10" 
    plot ?  
  
    set-current-plot-pen "Period 30" 
    plot first list-period30
    set list-period30 but-first list-period30
  
    set-current-plot-pen "Period 50" 
    plot first list-period50
    set list-period50 but-first list-period50
  
    set-current-plot-pen "Reservation" 
    plot first list-reservation  
    set list-reservation but-first list-reservation
  ]
end
@#$#@#$#@
GRAPHICS-WINDOW
176
87
587
519
200
200
1.0
0
10
1
1
1
0

CC-WINDOW
5
541
1050
636
Command Center

BUTTON
8
10
107
43
Setup
setup
NIL
1
T
OBSERVER
T
NIL

BUTTON
9
46
92
79
Go
go
T
1
T
OBSERVER
T
NIL

MONITOR
602
10
659
59
NIL
clock
3
1

PLOT
592
162
815
313
Figure a
Period or Alpha
Average Delay
0.0
0.0
0.0
10.0
true
true
PENS
"Theoretical" 1.0 0 -16776961 true
"Empirical" 1.0 0 -65536 true

BUTTON
110
11
176
44
go once
go
NIL
1
T
OBSERVER
T
NIL

CHOICE
10
97
148
142
model
model
"Overpass" "Trafic Light" "Reservation"
2

SLIDER
10
143
173
176
lanes
lanes
1
6
3
1
1
NIL

SLIDER
8
176
173
209
spawn-probability-N
spawn-probability-N
0.0
1.0
0.015
0.0010
1
NIL

MONITOR
603
111
670
160
Total Delay
total-delay\n
3
1

SLIDER
12
316
170
349
period
period
10
200
30
1
1
NIL

SLIDER
12
350
171
383
alpha
alpha
0
1
0.45
0.05
1
NIL

SLIDER
12
384
171
417
beta
beta
0
1
0.1
0.1
1
NIL

MONITOR
795
10
890
59
Finished Cars
total-finished-cars
3
1

MONITOR
671
111
768
160
Average Delay
total-delay / total-finished-cars 
3
1

BUTTON
13
461
90
494
Experiment 1
experiment1
NIL
1
T
OBSERVER
T
NIL

PLOT
817
162
1041
312
Figure b
Period or Alpha
Maximum Delay
0.0
0.0
0.0
10.0
true
true
PENS
"Theoretical" 1.0 0 -16776961 true
"Empirical" 1.0 0 -65536 true

BUTTON
96
462
167
495
Experiment 2
experiment2
NIL
1
T
OBSERVER
T
NIL

SLIDER
12
419
171
452
granularity
granularity
1
50
2
1
1
NIL

MONITOR
768
60
870
109
Expec. Max. Delay
expected-maximum-delay
3
1

MONITOR
671
61
768
110
Expec. Avg. Delay
expected-average-delay
3
1

MONITOR
770
110
844
159
Max. Delay
maximum-delay
3
1

MONITOR
671
10
725
59
NIL
total-cars
3
1

MONITOR
727
10
794
59
NIL
active-cars
3
1

MONITOR
603
61
666
110
Seconds
clock * .02
3
1

SLIDER
9
210
173
243
spawn-probability-S
spawn-probability-S
0
1
0.015
0.0010
1
NIL

SLIDER
9
244
172
277
spawn-probability-E
spawn-probability-E
0
1
0.015
0.0010
1
NIL

SLIDER
10
279
171
312
spawn-probability-W
spawn-probability-W
0
1
0.015
0.0010
1
NIL

BUTTON
96
494
167
527
Experiment 4
experiment4
NIL
1
T
OBSERVER
T
NIL

PLOT
592
316
814
466
Figure c
% Chance to Spawn
AverageDelay
0.0
0.0010
0.0
0.1
true
false
PENS
"Granu. 1" 1.0 0 -16776961 true
"Granu. 2" 1.0 0 -65536 true

BUTTON
13
494
90
527
NIL
Experiment3
NIL
1
T
OBSERVER
T
NIL

PLOT
816
316
1040
466
Figure d
% Spawn 
Average Delay
0.0
10.0
0.0
10.0
true
true
PENS
"Period 10" 1.0 0 -11352576 true
"Period 30" 1.0 0 -16776961 true
"Period 50" 1.0 0 -65413 true
"Reservation" 1.0 0 -16711738 true

PLOT
184
114
344
234
Average Delay
NIL
NIL
0.0
1.0
0.0
1.0
true
false
PENS
"default" 1.0 0 -16776961 true

PLOT
422
115
582
235
Maximum Delay
NIL
NIL
0.0
1.0
0.0
1.0
true
false
PENS
"default" 1.0 0 -16745473 true

SWITCH
95
47
185
80
plotting
plotting
0
1
-1000

SLIDER
209
8
381
41
ticks-by-second
ticks-by-second
1
50
10
1
1
NIL

@#$#@#$#@
Title: Traffic Management
Author: Benito Mendoza Garcia and Kurt Dresner
Description:

This program is a translation into NetLogo of an original Java simulator written by
Kurt Dresner and described in
<ul><li>Kurt Dresner and Peter Stone. <a href="http://jmvidal.cse.sc.edu/library/dresner04a.pdf">Multiagent Traffic Management: A Reservation-Based Intersection Control Mechanism</a>. In Proceedings of the Third International Joint Conference on Autonomous Agents and MultiAgent Systems, p. 530--537, ACM. 2004. (see also <a href="http://www.cs.utexas.edu/users/kdresner/2004iav/">Dresner's original applet</a>)</li></ul>
The simulator models traffic at intersections and has three different intersection control policies: overpass, traffic light, and reservation system. The program reproduces the results presented in the paper. It has four buttons, labeled Experiment1 through Experiment4, to perform the simulations that produce the results in Figures 2 - 5 of the paper respectively.  Results from this experiments are shown in the plots and monitors placed on the right side of the screen. Because experiments take a very long time to run, it will take much time to reproduce the graphs in the figures of the paper. 
The experiments for figures 6 to 8 can be carried out by changing the parameters accordingly and taking the results from the monitors on the right side of the screen.
--------

Name: Benito Mendoza Garcia
email: mendoza2@engr.sc.edu

SUMMARY
--------
CSCE 782: Problem Set 1
Tuesday, 7 September 2004
Multiagent Traffic Management Simulation

This program is based on the paper [1] and consists of a simulator to model traffic at intersections and three different intersection control policies: overpass, traffic light, and reservation system. The program reproduces the results presented in the paper. It has four buttons, labeled Experiment1 through Experiment4, to perform the simulations that produce the results in Figures 2 - 5 of the paper respectively.  Results from this experiments are shown in the plots and monitors placed on the right side of the screen. Because experiments take a very long time to run, it will take much time to reproduce the graphs in the figures of the paper. 
The experiments for figures 6 to 8 can be carried out by changing the parameters accordingly and taking the results from the monitors on the right side of the screen.

DIFFERENCES WITH THE PAPER
---------------------------
A difference with the simulator created by the authors of the paper and this one is that in the first one the cars have a rectangle shape (2 wide and 4 long) while in the second one they have a squared shape. The reason is that in NetLogo, although turtles onscreen can be represented with certain size and shape, they have a squared internal representation.
The area of this simulator is (200 m X 200 m) as opposed to (400 m X 400 m) for the one from the paper. The reason is that an area of (400 X 400) was to big to fit on screen.


HOW TO USE IT
---------------
In this program, the user has the ability to control the following parameters (through sliders placed on the left side of the screen):

ticks-by-second: this is the number of ticks (timesteps) that form a second. In the paper this is equal to 50.
model: the intersection control policy (Overpass, Traffic Light, or Reservation System)
lanes: the number of lanes that will be created for each direction (north, south, east, and west)
spawn-probability-N: the probability for spawning a car in the north direction
spawn-probability-S: the probability for spawning a car in the north direction
spawn-probability-E: the probability for spawning a car in the east direction
spawn-probability-W: the probability for spawning a car in the west direction
period: the period of the traffic light
alpha: the fraction of the light�s period that the light spends on green in one direction
beta: the fraction of the light�s period that the light spends on red in al the four directions
granularity: the number of rows and columns that the intersection is divided into when using the Reservation System model (the intersection is divided into a [granularity x granularity] grid of reservation tiles)

The size of the cars and the lane width can be changed by modifying the cars-size and lane-width global variables respectively.

Some plots and monitors are placed on the right side of the screen. The monitors show information such as average delay, expected average delay, maximum delay, expected maximum delay, time step (clock), number of finished cars. The plots show the results obtained from the experiments (buttons Experiment1 through Experiment4).

REFERENCES
[1] Kurt Dresner and Peter Stone. �Multiagent Traffic Management: A Reservation-Based Intersection Control Mechanism�. In Proceedings of the Third International Joint Conference on Autonomous Agents and MultiAgent Systems, p. 530--537, ACM. 2004
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
Polygon -256 true false 152 149 77 163 67 195 67 211 74 234 85 252 100 264 116 276 134 286 151 300 167 285 182 278 206 260 220 242 226 218 226 195 222 166
Polygon -16777216 true false 150 149 128 151 114 151 98 145 80 122 80 103 81 83 95 67 117 58 141 54 151 53 177 55 195 66 207 82 211 94 211 116 204 139 189 149 171 152
Polygon -7566196 true true 151 54 119 59 96 60 81 50 78 39 87 25 103 18 115 23 121 13 150 1 180 14 189 23 197 17 210 19 222 30 222 44 212 57 192 58
Polygon -16777216 true false 70 185 74 171 223 172 224 186
Polygon -16777216 true false 67 211 71 226 224 226 225 211 67 211
Polygon -16777216 true false 91 257 106 269 195 269 211 255
Line -1 false 144 100 70 87
Line -1 false 70 87 45 87
Line -1 false 45 86 26 97
Line -1 false 26 96 22 115
Line -1 false 22 115 25 130
Line -1 false 26 131 37 141
Line -1 false 37 141 55 144
Line -1 false 55 143 143 101
Line -1 false 141 100 227 138
Line -1 false 227 138 241 137
Line -1 false 241 137 249 129
Line -1 false 249 129 254 110
Line -1 false 253 108 248 97
Line -1 false 249 95 235 82
Line -1 false 235 82 144 100

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

car
true
0
Rectangle -7566196 true true 134 30 195 241
Polygon -7566196 true true 150 67 78 108 78 183 148 223
Circle -1 true false 176 49 37
Circle -1 true false 176 190 38
Polygon -16711681 true false 133 93 87 115 87 136 134 136 133 93
Polygon -16711681 true false 88 147 88 174 134 197 134 147

circle
false
0
Circle -7566196 true true 34 34 230

person
false
0
Circle -7566196 true true 155 20 63
Rectangle -7566196 true true 158 79 217 164
Polygon -7566196 true true 158 81 110 129 131 143 158 109 165 110
Polygon -7566196 true true 216 83 267 123 248 143 215 107
Polygon -7566196 true true 167 163 145 234 183 234 183 163
Polygon -7566196 true true 195 163 195 233 227 233 206 159

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
NetLogo 2.1beta2
@#$#@#$#@
@#$#@#$#@
@#$#@#$#@
