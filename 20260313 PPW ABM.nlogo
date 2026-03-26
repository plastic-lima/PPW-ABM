globals [
  cluster-centers
  enrolled-households

  daily-ppw-total
  daily-formal
  daily-formal-7day
  daily-formal-stock
  daily-formal-station
  daily-collected-formal
  daily-collected-formal-formal
  daily-collected-formal-7day
  daily-collected-formal-stations
  daily-uncollected-formal-formal
  cumulative-formal

  daily-informal
  daily-residual
  daily-collected-informal
  daily-collected-informal-informal
  daily-collected-informal-residual
  cumulative-informal

  daily-uncollected-informal-residual
  daily-uncollected-informal-informal
  daily-landfill
  daily-landfill-direct
  cumulative-landfill

  informal-picker-daily-cap
  formal-picker-daily-cap
  formal-picker-7day-cap
  formal-picker-station-cap
  formal-picker-detection-radius
  informal-picker-detection-radius
  informal-available-log
  formal-available-log

  recycling-station-log
  formal-7day-tick-counter

  always-formal-group
  formal-group-initialized?
  intervention-duration
  dropout-rate
  support-multiplier-rp

  motivation-pass-count
  knowledge-pass-count
  opportunity-pass-count
]

breed [ citizens citizen ]
breed [ recycling-stations recycling-station ]
breed [formal-pickers formal-picker]
breed [informal-pickers informal-picker]
undirected-link-breed [social-links social-link]

citizens-own [
  income-level
  household-id
  ppw-generated
  separated?
  previous-separated?
  formal?
  previous-formal?
  disposal-route
  chose-formal?
  residual-stock
  informal-stock
  formal-7day-stock
  formal-stock
  formal-station
  formal-7day-bin
  reachable-by-formal?
  reachable-by-informal?

  motivation-boost
  knowledge-boost
  opportunity-boost

  leaflet-targeted?
  workshop-targeted?
  soc-media-targeted?
  door-targeted?
  parks-targeted?
  incentives-targeted?
  eco-trade-targeted?
  recycling-program-targeted?
  recycling-station-targeted?

  alpha-motivation
  alpha-knowledge
  alpha-opportunity

  picked-residual?
  picked-informal?
  picked-formal?

  enrollment-tick
]

recycling-stations-own [
  received-waste
]

informal-pickers-own [
  daily-collected
  collected-log
  home-x
  home-y
]

formal-pickers-own [
  daily-collected
  collected-log
  assigned-station
  enrolled-household
  household-groups
  home-x
  home-y
]

to initialize-model-state;; resets the world and validates income distribution consistency; called at the start of setup
  clear-all
  ask patches [ set pcolor 8 ]

  set always-formal-group []
  set formal-group-initialized? false

  let total-pct high-income-% + medium-high-income-% + medium-income-% + medium-low-income-% + low-income-%
  if total-pct != 100 [
    user-message "Income-level percentages must sum to 100%"
    stop
  ]
end

to setup-households;; creates household clusters and positions citizens in space; called once during setup before income assignment
  set cluster-centers []
  let remaining num-citizens
  let min-spacing 2.0
  let min-spacing-sq min-spacing * min-spacing

  while [ remaining > 0 ] [
    let cluster-size min list remaining (3 + random 3)

    ;; pick a non-overlapping center
    let cx random-xcor
    let cy random-ycor

    ;; repeat until no existing center is too close
    while [
      (length filter [ center ->
        ((cx - item 0 center) * (cx - item 0 center)
          + (cy - item 1 center) * (cy - item 1 center))
        <= min-spacing-sq
      ] cluster-centers) > 0
    ] [
      set cx random-xcor
      set cy random-ycor
    ]

    ;; record this new center
    let cluster-id length cluster-centers + 1
    set cluster-centers lput (list cx cy) cluster-centers

    create-citizens cluster-size [
      initialize-citizen cluster-id cx cy cluster-size
    ]

    ask patches with  [ distancexy cx cy <= 0.7 ] [
      if cluster-size = 3 [ set pcolor 48 ]
      if cluster-size = 4 [ set pcolor 47 ]
      if cluster-size = 5 [ set pcolor 46 ]
    ]

    set remaining remaining - cluster-size
  ]
end

to initialize-citizen ;; initializes citizen variables and spatial location within the household cluster; called from setup-households during citizen creation
  [cluster-id cx cy cluster-size]
  set household-id cluster-id
  set residual-stock 0
  set informal-stock 0
  set formal-stock 0
  set formal-7day-stock 0
  set formal-station 0
  set formal-7day-bin 0
  set cumulative-landfill 0
  set cumulative-informal 0
  set cumulative-formal 0
  set separated? false
  set previous-separated? false
  set formal? false
  set previous-formal? false
  set chose-formal? false
  set leaflet-targeted? false
  set workshop-targeted? false
  set soc-media-targeted? false
  set door-targeted? false
  set parks-targeted? false
  set incentives-targeted? false
  set eco-trade-targeted? false
  set recycling-program-targeted? false
  set recycling-station-targeted? false
  set alpha-motivation 0
  set alpha-knowledge 0
  set alpha-opportunity 0
  set motivation-boost 0
  set knowledge-boost 0
  set opportunity-boost 0
  hide-turtle
  let angle 360 * (who mod cluster-size) / cluster-size
  let delta-x 0.5 * sin angle
  let delta-y 0.5 * cos angle
  setxy cx + delta-x cy + delta-y
  set enrollment-tick 0
end

to setup-income ;; assigns income levels across households; called once in setup after household creation
  let total num-citizens
  let c1 floor (high-income-%       * total / 100)
  let c2 floor (medium-high-income-% * total / 100)
  let c3 floor (medium-income-%      * total / 100)
  let c4 floor (medium-low-income-%  * total / 100)
  let c5 floor (low-income-%         * total / 100)

  ;; fix rounding gap
  let assigned c1 + c2 + c3 + c4 + c5
  if assigned < total [
    set c5 c5 + (total - assigned)
  ]

  ;; reset and assign all five levels in one pass
  ask citizens [ set income-level 0 ]
  let ordered-citizens sort citizens
  let index 0
  let lvl 1
  foreach (list c1 c2 c3 c4 c5) [ cnt ->
    repeat cnt [
      ask item index ordered-citizens [
        set income-level lvl
      ]
      set index index + 1
    ]
    set lvl lvl + 1
  ]
  let hh-ids remove-duplicates [household-id] of citizens
  foreach hh-ids [ hh ->
    let hh-level first [ income-level ] of citizens with [ household-id = hh ]
    ask citizens with [ household-id = hh ] [
      set income-level hh-level]
  ]
end

to build-social-network;; creates social links between citizens for peer influence; used in check-separation-context
  let sorted-citizens sort-by [ [a b] -> [who] of a < [who] of b ] citizens
  let total count citizens

  ask citizens [
    let i position self sorted-citizens
    let ring-indexes (range (i - 3) (i + 4))


    foreach ring-indexes [ j ->
      if j != i and j >= 0 and j < total [
        let neighbor item j sorted-citizens
        if not link-neighbor? neighbor [
          create-social-link-with neighbor
        ]
      ]
    ]
  ]
  ask links [
    set color gray
    set thickness 0.01
    set hidden? true
  ]
end

to setup-pickers ;; creates and places waste pickers; used in setup for all distance-based detection and collection logic
  let min-distance 1.5

  ;; Create formal pickers
  repeat num-formal-pickers [
    let x random-xcor
    let y random-ycor
    while [any? citizens with [distancexy x y < min-distance] or
      any? formal-pickers with [distancexy x y < min-distance] or
      any? informal-pickers with [distancexy x y < min-distance]
    ] [
      set x random-xcor
      set y random-ycor
    ]
    create-formal-pickers 1 [
      set shape "person"
      set color blue
      set size 1
      set home-x x
      set home-y y
      setxy home-x home-y
      set collected-log []
      set assigned-station []
      set enrolled-household []
      set household-groups []
    ]
  ]

  ;; Create informal pickers
  repeat num-informal-pickers [
    let x random-xcor
    let y random-ycor
    while [any? citizens with [distancexy x y < min-distance] or
      any? formal-pickers with [distancexy x y < min-distance] or
      any? informal-pickers with [distancexy x y < min-distance]
    ] [
      set x random-xcor
      set y random-ycor
    ]
    create-informal-pickers 1 [
      set shape "person"
      set color red
      set size 1
      set home-x x
      set home-y y
      setxy home-x home-y
      set collected-log []
    ]
  ]
end

to setup-recycling-stations;; creates and  places recycling stations; used in disposal decisions and station collection procedures
  let activate-recycling true
  if activate-recycling [
    let placed 0
    while [placed < num-recycling-stations] [
      let x random-xcor
      let y random-ycor
      ;; Make sure no citizen is too close (within radius 1)
      if (not any? citizens with [distancexy x y < 1]) and (not any? recycling-stations with [distancexy x y < 5]) [
        create-recycling-stations 1 [
          set shape "pentagon"
          set color 125
          set size 1
          setxy x y
        ]
        set placed placed + 1
      ]
    ]
  ]
end

to initialize-comb-scores
  ask citizens [
    let mean-score (6 - income-level)

    set alpha-motivation max list 1 min list 5
      (random-normal (3.4 + 0.3 * (mean-score - 3) + (mean-motivation - 3.4)) 0.3)

    set alpha-knowledge max list 1 min list 5
      (random-normal (3.4 + 0.3 * (mean-score - 3) + (mean-knowledge - 3.4)) 0.3)

    set alpha-opportunity max list 1 min list 5
      (random-normal (3.4 + 0.3 * (mean-score - 3) + (mean-opportunity - 3.4)) 0.3)
  ]
end


to setup-interventions;; initializes intervention targeting and recycling program enrollment; used in behavior, collection, and dropout logic
  let hh-ids remove-duplicates [household-id] of citizens
  set enrolled-households
  (ifelse-value (enrolled-recycling-program > 0)
    [n-of enrolled-recycling-program hh-ids]
    [ [] ])
  ask citizens with [member? household-id enrolled-households] [
  set enrollment-tick 0
]
  mark-local-interventions
  if (not formal-group-initialized? and length enrolled-households > 0) [
  ;  set always-formal-group n-of (length enrolled-households * 0.5 * support-multiplier-rp) enrolled-households
        set always-formal-group n-of (length enrolled-households * 1 * support-multiplier-rp) enrolled-households
    set formal-group-initialized? true
  ]
  set intervention-duration 1
end

to setup-logs ;; initializes picker capacities, detection radii, and logging variables; used in picker collection, appearance, and disappearance procedures
  set formal-picker-detection-radius 10
  set informal-picker-detection-radius 23

  set informal-available-log []
  set formal-available-log []
  set recycling-station-log []
  set formal-7day-tick-counter 0
end

to setup ;; main setup sequence calling all initialization procedures in order; executed once when the model starts
       ;  random-seed 0
  initialize-model-state
  setup-households
  setup-income
  build-social-network
  setup-pickers
  setup-recycling-stations
  initialize-comb-scores
  set-municipal-support
  setup-interventions
  assign-formal-pickers-to-stations
  assign-formal-pickers-to-enrolled-households
  setup-logs
  reset-ticks
end

to reset-daily-state;; resets daily waste stocks and collection flags for citizens; called at the start of each tick in go
  set daily-ppw-total 0
  set daily-formal 0
  set daily-informal 0
  set daily-residual 0
  set daily-landfill 0
  set daily-landfill-direct 0
  set daily-formal-station 0
  set daily-formal-7day 0
  set daily-formal-stock 0
  set daily-collected-formal 0
  set daily-collected-formal-formal 0
  set daily-collected-formal-7day 0
  set daily-collected-formal-stations 0
  set daily-uncollected-formal-formal 0
  set daily-collected-informal 0
  set daily-collected-informal-informal 0
  set daily-collected-informal-residual 0
  set daily-uncollected-informal-residual 0
  set daily-uncollected-informal-informal 0

  ask citizens [
    set residual-stock 0
    set informal-stock 0
    set formal-stock 0
    set formal-station 0
    set picked-residual?   false
    set picked-informal?   false
    set picked-formal?     false
  ]

  ask informal-pickers [ set daily-collected 0 ]
  ask formal-pickers [ set daily-collected 0 ]
end

to generate-waste;; generates daily PPW per citizen based on income level; called at each tick at the start of go
  let means [7.032 6.4688 5.6256 5.0624 4.5008]
  ask citizens [
    let i income-level - 1
    let mu item i means
    let sd mu * 0.048 / 1.96
    set ppw-generated random-normal mu sd
  ]
end

to end-interventions;; disables all intervention effects after the intervention period; called in go before behavior decisions
  if ticks >= intervention-duration [
    ask citizens [
      set leaflet-targeted? false
      set workshop-targeted? false
      set soc-media-targeted? false
      set door-targeted? false
      set parks-targeted? false
      set incentives-targeted? false
      set eco-trade-targeted? false
      set recycling-program-targeted? false
      set recycling-station-targeted? false
      set color 126
    ]
    ask patches [ set pcolor 8 ]

    foreach cluster-centers [ center ->
      let cx item 0 center
      let cy item 1 center
      let hh-size count citizens with [ household-id = position center cluster-centers + 1 ]

      ask patches with [ distancexy cx cy <= 0.7 ] [
        if hh-size = 3 [ set pcolor 48 ]
        if hh-size = 4 [ set pcolor 47 ]
        if hh-size = 5 [ set pcolor 46 ]
      ]
    ]
  ]
end

to check-separation-context;; evaluates whether each citizen separates waste based on persistence, interventions, and timing; called in go before disposal decisions
  ask citizens [

    if member? household-id always-formal-group [
      set separated? true
      set chose-formal? true
      stop
    ]

    if ticks = 0 [
      compute-motivation
      set previous-separated? separated?
      stop
    ]

    if random-float 1 > 1 [
      set separated? previous-separated?
      stop
    ]
    compute-motivation
    set previous-separated? separated?
  ]
end

to compute-motivation;; computes motivation to separate waste including intervention effects and social influence; called from check-separation-context
  let z alpha-motivation
  ;; sigmoid probability convert z to probability)
  let k 1.8
  let p 1 / (1 + exp (- k * (z - 1.6)))

  let b motivation-boost
  if incentives-targeted? [ set b 1 - (1 - b) * (1 - 0.64) ]
  if eco-trade-targeted?  [ set b 1 - (1 - b) * (1 - 0.40) ]
  if door-targeted?       [ set b 1 - (1 - b) * (1 - 0.15) ]
  if workshop-targeted?   [ set b 1 - (1 - b) * (1 - 0.15) ]

  set b min list 1 max list 0 b

  set p p + (b * (1 - p))

  set motivation-boost b * 0.9

  set p min list 1 max list 0 p

  if random-float 1 < p [
    set motivation-pass-count motivation-pass-count + 1
    compute-knowledge
    stop
  ]
  ;; otherwise they fail
  set separated? false
end

to compute-knowledge;; computes knowledge threshold for correct separation after motivation is met; called from compute-motivation
  let z alpha-knowledge
  ;; sigmoid: convert to probability
  let k 1.2
  let p 1 / (1 + exp (- k * (z - 1.6)))

  let b knowledge-boost
  if parks-targeted?              [ set b 1 - (1 - b) * (1 - 0.58) ]
  if soc-media-targeted?          [ set b 1 - (1 - b) * (1 - 0.51) ]
  if leaflet-targeted?            [ set b 1 - (1 - b) * (1 - 0.50) ]
  if door-targeted?               [ set b 1 - (1 - b) * (1 - 0.46) ]
  if workshop-targeted?           [ set b 1 - (1 - b) * (1 - 0.46) ]
  if recycling-program-targeted?  [ set b 1 - (1 - b) * (1 - 0.15) ]

  set b min list 1 max list 0 b

  set p p + (b * (1 - p))

  set knowledge-boost b * 0.9

  set p min list 1 max list 0 p

  if random-float 1 < p [
    set knowledge-pass-count knowledge-pass-count + 1
    compute-opportunity
    stop
  ]

  set separated? false
end

to compute-opportunity;; evaluates availability (pickers and stations) to finalize separation decision; called from compute-knowledge
  let z alpha-opportunity

  let n-separating-friends count link-neighbors with [previous-separated? = true]
  if n-separating-friends >= 2 [
;    set z z + 0.5
        set z z + 2
  ]
  set z min list 5 max list 1 z

  let k 1.2
  let p 1 / (1 + exp ( - k * (z - 1.6)))

  let b opportunity-boost
  if eco-trade-targeted?         [ set b 1 - (1 - b) * (1 - 0.19) ]
  if recycling-station-targeted? [ set b 1 - (1 - b) * (1 - 0.54) ]
  if recycling-program-targeted? [ set b 1 - (1 - b) * (1 - 0.44) ]

  set b min list 1 max list 0 b

  set p p + (b * (1 - p))

  set opportunity-boost b * 0.9

  set p min list 1 max list 0 p

  if random-float 1 < p [
    set opportunity-pass-count opportunity-pass-count + 1
    set separated? true
    stop
  ]

  set separated? false
end

to citizen-count
  show (list
    ticks
    motivation-pass-count
    knowledge-pass-count
    opportunity-pass-count
    count citizens with [member? household-id always-formal-group]
  )
end


to check-disposal-context ;; determines whether separating citizens use formal or informal disposal routes; called in go after check-separation-context
  ask citizens [
    if separated?[
      if ticks = 0 [
        decide-disposal-route
        set previous-formal? formal?
      ]
      if ticks > 0 and random-float 1 > 0.1 [
        set formal? previous-formal?
        stop
      ]
      decide-disposal-route
      set previous-formal? formal?
    ]
  ]
end

to decide-disposal-route;; decides disposal route for separating citizens; called in check-disposal-context
  let near-rs? any? recycling-stations in-radius 4
   let n-formal count formal-pickers in-radius 1.4
  let n-informal count informal-pickers in-radius 1.4

  set disposal-route "none"
  set chose-formal? false
  set formal? false

  if not separated? [ stop ]

ifelse n-formal > n-informal [
    ifelse (
      (income-level = 1 and (who mod 100) < 67) or
      (income-level = 2 and (who mod 100) < 42) or
      (income-level = 3 and (who mod 100) < 27) or
      (income-level = 4 and (who mod 100) < 22) or
      (income-level = 5 and (who mod 100) < 29)
      ) [
      set chose-formal? true
      set formal? true
    ] [
      set disposal-route "informal"
      set chose-formal? false
      set formal? false
    ]
  ] [
    ifelse n-informal > n-formal [
      ifelse (
        (income-level = 1 and (who mod 100) >= 33) or
        (income-level = 2 and (who mod 100) >= 58) or
        (income-level = 3 and (who mod 100) >= 73) or
        (income-level = 4 and (who mod 100) >= 78) or
        (income-level = 5 and (who mod 100) >= 71)
        ) [
        set disposal-route "informal"
        set chose-formal? false
        set formal? false
      ] [
        set chose-formal? true
        set formal? true
      ]
    ] [

      ifelse (
        (income-level = 1 and (who mod 100) < 67) or
        (income-level = 2 and (who mod 100) < 42) or
        (income-level = 3 and (who mod 100) < 27) or
        (income-level = 4 and (who mod 100) < 22) or
        (income-level = 5 and (who mod 100) < 29)
        ) [
        set chose-formal? true
        set formal? true
      ] [
        set disposal-route "informal"
        set chose-formal? false
        set formal? false
      ]
    ]
  ]
  if formal? and near-rs? [
    if ((who mod 100 ) < 40 ) [
      set disposal-route "station"
      set chose-formal? true
      set formal? true
    ]
  ]
  if formal? and disposal-route = "none" [
    set disposal-route "formal"
  ]
end

to citizen-waste-allocation;; allocates PPW into residual, informal, formal, station, and program-specific stocks; called in go after disposal decisions
  foreach sort citizens [ c ->
    ask c [
      let is-separating? (member? household-id always-formal-group) or separated?

      let mu0 item (income-level - 1)
      [0.70 0.65 0.55 0.50 0.45]

      let mu mu0
      set mu min list 1 mu

      let sd0  mu0 * 0.048 / 1.96      ;; baseline variability (survey)
      let sd sqrt (sd0 ^ 2)

      let sep-frac ifelse-value is-separating?
      [ max list 0 min list 1 (random-normal  mu sd)]
      [ 0 ]
      let sep-waste ppw-generated * sep-frac

      let res-waste ifelse-value separated?
      [(ppw-generated - sep-waste) * 0.20]
      [ppw-generated]

      if separated? [
        set daily-landfill-direct daily-landfill-direct + (ppw-generated - sep-waste) * 0.80
      ]
      set residual-stock residual-stock + res-waste
      set daily-residual daily-residual + res-waste
      set daily-residual daily-residual + (ppw-generated - sep-waste) * 0.80

      if member? household-id always-formal-group [
        set daily-formal daily-formal + sep-waste
        set formal-7day-stock formal-7day-stock + sep-waste
        set daily-formal-7day daily-formal-7day + sep-waste
        set formal-7day-bin formal-7day-bin + sep-waste
      ]

      if separated? and not member? household-id always-formal-group[
        if disposal-route = "station" [
          set daily-formal daily-formal + sep-waste
          set formal-station formal-station + sep-waste
          set daily-formal-station daily-formal-station + sep-waste
          let my-waste sep-waste
          ask min-one-of recycling-stations [distance myself] [
            set received-waste received-waste + my-waste
          ]
        ]
        if disposal-route = "formal" [
          set daily-formal daily-formal + sep-waste
          set formal-stock formal-stock + sep-waste
          set daily-formal-stock daily-formal-stock + sep-waste
        ]
        if disposal-route = "informal" [
          set informal-stock informal-stock + sep-waste
          set daily-informal daily-informal + sep-waste
        ]
      ]
    ]
  ]
end

to detect-eligible-households;; updates which households are reachable by formal and informal pickers; called in go before picker collection
  ask citizens [
    set reachable-by-formal? false
    set reachable-by-informal? false
  ]

  ask formal-pickers [
    let hhs citizens in-radius formal-picker-detection-radius
    ask hhs [ set reachable-by-formal? true ]
  ]

  ask informal-pickers [
    let hhs citizens in-radius informal-picker-detection-radius
    ask hhs [ set reachable-by-informal? true ]
  ]

  ;; build sets of eligible household-IDs for each picker type
  let eligible-household-ids-formal remove-duplicates [household-id] of citizens with [reachable-by-formal?]
  let eligible-household-ids-informal remove-duplicates [household-id] of citizens with [reachable-by-informal?]
end

to informal-picker-collect;; simulates informal picker collection from residual and informal stock; called in go after formal collection
  ask informal-pickers [

    ;; Collect from residual stock
    let eligible-hhs remove-duplicates [household-id] of citizens with
      [residual-stock > 0 and not picked-residual? and reachable-by-informal?]

    let capped-hhs sublist eligible-hhs 0 (min (list 200 length eligible-hhs))

    if not empty? capped-hhs [
      let hh-id first capped-hhs

      ask citizens with
      [household-id = hh-id and not picked-residual? and reachable-by-informal?] [

        set picked-residual? true

        let recovery-rate item (income-level - 1)
          [0.65 0.60 0.50 0.45 0.40]

        let collected recovery-rate * residual-stock

        set residual-stock residual-stock - collected
        set daily-collected-informal-residual
        daily-collected-informal-residual + collected


        ask myself [
          set daily-collected daily-collected + collected
          set daily-collected-informal daily-collected-informal + collected
        ]
      ]
    ]

    ;; Collect from informal stock

    let eligible-hhs-2 remove-duplicates [household-id] of citizens with
      [informal-stock > 0 and not picked-informal? and reachable-by-informal?]
    let capped-hhs-2 sublist eligible-hhs-2 0 (min (list 200 length eligible-hhs-2))

    foreach capped-hhs-2 [ hh-id ->
      ask citizens with [household-id = hh-id and not picked-informal? and reachable-by-informal?] [

        set picked-informal? true
        let collected min (list informal-stock)
        set informal-stock informal-stock - collected
        ask myself [
          set daily-collected daily-collected + collected
          set daily-collected-informal daily-collected-informal + collected
          set daily-collected-informal-informal daily-collected-informal-informal + collected
        ]
      ]
    ]
  ]
end

to formal-picker-collect-daily;; collects daily formal waste from household; called in go after detect-eligible-households
  ask formal-pickers [
    let eligible-hhs remove-duplicates [household-id] of citizens with
    [formal-stock > 0 and not picked-formal? and reachable-by-formal?]
    let capped-hhs sublist eligible-hhs 0 (min (list 200 length eligible-hhs))

    foreach capped-hhs [ hh-id ->
      ask citizens with [household-id = hh-id and formal-stock > 0 and not picked-formal? and reachable-by-formal?] [
        set picked-formal? true
        let collected min (list formal-stock)
        set formal-stock formal-stock - collected
        ask myself [
          set daily-collected daily-collected + collected
          set daily-collected-formal-formal daily-collected-formal-formal + collected
          set daily-collected-formal daily-collected-formal + collected
        ]
      ]
    ]
  ]
end

to advance-7day-tick-counter;; advances the counter for enrolled household collection; called in go before 7-day collection
  set formal-7day-tick-counter formal-7day-tick-counter + 1
  if formal-7day-tick-counter > 7 [
    set formal-7day-tick-counter 1
  ]
end

to formal-picker-collect-HH;; collects waste from enrolled households on the scheduled 7-day cycle; called in go after daily formal collection
  if formal-7day-tick-counter = 7 [
    ask formal-pickers [
      ;; group households into chunks of 10
      let hhs shuffle enrolled-household
      set household-groups []
      while [length hhs > 0] [
        let chunk sublist hhs 0 (min list 200 length hhs)
        set household-groups lput chunk household-groups
        set hhs filter [x -> not member? x chunk] hhs
      ]

      if length household-groups > 0 [
        let group-index (formal-7day-tick-counter - 1) mod length household-groups
        let current-group item group-index household-groups

        ask citizens with [member? household-id current-group and formal-7day-bin > 0 and not picked-formal?] [
          set picked-formal? true
          let collected min (list formal-7day-bin)
          set formal-7day-bin formal-7day-bin - collected
          ask myself [
            set daily-collected daily-collected + collected
            set daily-collected-formal-7day daily-collected-formal-7day + collected
            set daily-collected-formal daily-collected-formal + collected
          ]
        ]
      ]
    ]
    set formal-7day-tick-counter 0
  ]
end

to formal-picker-collect-stations;; collects waste accumulated at recycling stations and distributes it among assigned formal pickers; called in go every 10 ticks
  if (ticks > 0) and (ticks mod 10 = 0) [
    ask recycling-stations [
      let assigned-pickers formal-pickers with [member? [who] of myself assigned-station]
      let n-pickers count assigned-pickers
      let station-waste received-waste
      if n-pickers > 0 and station-waste > 0 [
        let share station-waste / n-pickers
        foreach sort assigned-pickers [
          picker ->
          ask picker [
            let collected share

            set daily-collected daily-collected + collected
            set daily-collected-formal daily-collected-formal + collected
            set daily-collected-formal-stations daily-collected-formal-stations + collected
            ask myself [
              set received-waste received-waste - collected
            ]
          ]
        ]
      ]
    ]
  ]
end

to log-recycling-station-individuals;; logs recycling station waste levels per tick; called for output tracking
  let row (list ticks)
  ask recycling-stations [
    set row lput received-waste row
  ]
  set recycling-station-log lput row recycling-station-log
end

to assign-formal-pickers-to-stations;; assigns formal pickers to recycling stations up to capacity; called once during setup
  ask recycling-stations [
    let station self
    let station-who [who] of self

    let pickers-in-range formal-pickers
    let assigned-pickers pickers-in-range with [
      member? station-who assigned-station
    ]
    let n-assigned count assigned-pickers

    if n-assigned < 10 [
      let needed 10 - n-assigned
      let available pickers-in-range with [length assigned-station < 6]

      let eligible-pickers available with [
        not member? station-who assigned-station
      ]

      let how-many min list needed count eligible-pickers

      if how-many > 0 [
        let selected-pickers n-of how-many eligible-pickers
        ask selected-pickers [
          set assigned-station lput station-who assigned-station
        ]
      ]
    ]
  ]

  ask recycling-stations [
    let station self
    let station-who [who] of self

    let pickers-in-range formal-pickers with [distance station < formal-picker-detection-radius]
    let available pickers-in-range with [length assigned-station < 1]
    let eligible-pickers available with [not member? station-who assigned-station]
  ]
end

to maintain-formal-pickers-to-stations;; maintains minimum picker coverage per recycling station; called repeatedly in go before station collection
  ask recycling-stations [
    let station-who [who] of self
    let assigned-pickers formal-pickers with [member? station-who assigned-station]
    let n-assigned count assigned-pickers

    if n-assigned < 10 [
      let needed 10 - n-assigned
      let available formal-pickers with [length assigned-station < 6]
      let eligible-pickers available with [not member? station-who assigned-station]

      let how-many min list needed count eligible-pickers
      if how-many > 0 [
        let selected-pickers n-of how-many eligible-pickers
        ask selected-pickers [
          set assigned-station lput station-who assigned-station
        ]
      ]
    ]
  ]
end

to assign-formal-pickers-to-enrolled-households;; assigns enrolled households to formal pickers; called during setup
  ask formal-pickers [
    set enrolled-household []
  ]

  let available formal-pickers with [length enrolled-household < 6]
  let available-households enrolled-households
  while [any? available and length available-households > 0] [
    let picker one-of available
    let hh one-of available-households

    if not member? hh [enrolled-household] of picker [
      ask picker [
        set enrolled-household lput hh enrolled-household
      ]
      set available-households remove hh available-households
    ]
    set available formal-pickers with [length enrolled-household < 6]
  ]
end

to-report ;; reports whether a given agent is present in a list, used during household–picker assignment and maintenance procedures
  list-contains-agent? [a-list an-agent]
  report length filter [x -> x = an-agent] a-list > 0
end

to maintain-formal-pickers-to-enrolled-households;; ensures each enrolled household has one assigned formal picker; called repeatedly in go
  foreach enrolled-households [ hh ->
    let assigned-pickers formal-pickers with [list-contains-agent? enrolled-household hh]
    let n-assigned count assigned-pickers

    ;; assign only if no picker assigned yet (n-assigned < 1)
    if n-assigned < 1 [
      let available formal-pickers with [length enrolled-household < 6]
      let eligible-pickers available with [not list-contains-agent? enrolled-household hh]

      ;; assign only 1 picker per household
      if any? eligible-pickers [
        let picker one-of eligible-pickers
        ask picker [
          set enrolled-household lput hh enrolled-household
        ]
      ]
    ]
  ]
end

to log-daily-collection;; records recent daily collection amounts for picker entry and exit dynamics; called each tick in go
  ask formal-pickers [
    let normalized (daily-collected
      - daily-collected-formal-7day
      - daily-collected-formal-stations
      + (daily-collected-formal-7day / 7)
      + (daily-collected-formal-stations / 10))
    set collected-log lput daily-collected collected-log
    if length collected-log > 15 [
      set collected-log but-first collected-log
    ]
  ]

  ask informal-pickers [
    set collected-log lput daily-collected collected-log
    if length collected-log > 15 [
      set collected-log but-first collected-log
    ]
  ]
end

to picker-disappear ;; removes pickers with persistently low collection; called periodically in go
  if (ticks >= 15) and (ticks mod 14 = 0) [
    ask formal-pickers [
      let avg-collected mean collected-log
      if avg-collected < 25 [ die ]
    ]
    ask informal-pickers [
      let avg-collected mean collected-log
      if avg-collected < 35 [ die ]
    ]
  ]
end

to informal-picker-appear;; adds informal pickers when available waste per picker is high; called periodically in go
  if (ticks >= 15) and (ticks mod 14 = 0) [
    let denom count informal-pickers
    if denom = 0 [ set denom 1 ]
    let today (daily-informal + daily-residual * 0.5) / denom
    set informal-available-log lput today informal-available-log
    if length informal-available-log > 10 [
      set informal-available-log but-first informal-available-log    ]
    let avg-available mean informal-available-log
    if avg-available > 35 [
      ask patch random-xcor random-ycor [
        sprout-informal-pickers 1 [
          set shape "person"
          set color red
          set size 1
          set home-x pxcor
          set home-y pycor
          setxy home-x home-y
          set collected-log []
          set daily-collected 0
        ]
      ]
    ]
  ]
end

to formal-picker-appear;; adds formal pickers when formal waste per picker is high; called periodically in go
  if (ticks >= 15) and (ticks mod 14 = 0) [
    let denom count formal-pickers
    if denom = 0 [ set denom 1 ]
    let today daily-formal / denom
    set formal-available-log lput today formal-available-log
    if length formal-available-log > 10 [
      set formal-available-log but-first formal-available-log    ]
    let avg-available mean formal-available-log
    if avg-available >= 45 [
      ask patch random-xcor random-ycor [
        sprout-formal-pickers 1 [
          set shape "person"
          set color blue
          set size 1
          set home-x pxcor
          set home-y pycor
          setxy home-x home-y
          set collected-log []
          set daily-collected 0
          set assigned-station []
          set enrolled-household []
          set household-groups []
        ]
      ]
    ]
  ]
end

to set-municipal-support;; sets intervention strength and dropout parameters based on municipal support; called once during setup
  if municipal-support = "High" [
    set dropout-rate 0.20
    set support-multiplier-rp 1.2
  ]

  if municipal-support = "Medium" [
    set dropout-rate 0.25
       set support-multiplier-rp 1
  ]

  if municipal-support = "Low" [
    set dropout-rate 0.30
    set support-multiplier-rp 0.8
  ]
end

to mark-local-interventions;; assigns intervention exposure flags to citizens and enrolls households into programs; called during setup-interventions
  if leaflets [
    let exposed n-of (citizen-leaf * 0.9) citizens
    ask exposed [
      set leaflet-targeted? true
      ask patch-here [set pcolor 67
      ]
    ]
  ]
  if workshops [
    let exposed n-of (citizen-wor * 0.9) citizens
    ask exposed [
      set workshop-targeted? true
      ask patch-here [set pcolor 67
      ]
    ]
  ]
  if soc-media [
    let exposed n-of (citizen-soc * 0.9) citizens
    ask exposed [
      set soc-media-targeted? true
      ask patch-here [set pcolor 67
      ]
    ]
  ]
  if door-to-door [
    let exposed n-of (citizen-door * 0.9) citizens
    ask exposed [
      set door-targeted? true
      ask patch-here [set pcolor 67
      ]
      if (not formal-group-initialized?) and random-float 1 < 0.3 [
        if not member? household-id enrolled-households [
          set enrolled-households lput household-id enrolled-households
                    ask citizens with [household-id = household-id] [
            set enrollment-tick 0
          ]
        ]
      ]
    ]
  ]
  if parks-activities [
    let exposed n-of (citizen-par * 0.9) citizens
    ask exposed [
      set parks-targeted? true
      ask patch-here [set pcolor 67
      ]
      if (not formal-group-initialized?) and random-float 1 < 0.2 [
        if not member? household-id enrolled-households [
          set enrolled-households lput household-id enrolled-households
                    ask citizens with [household-id = household-id] [
            set enrollment-tick 0
          ]
        ]
      ]
    ]
  ]
  if incentives [
    let exposed n-of (citizen-incent * 0.9) citizens
    ask exposed [
      set incentives-targeted? true
      ask patch-here [set pcolor 67
      ]
    ]
  ]
  if eco-trade [
    let exposed n-of (citizen-eco * 0.9) citizens
    ask exposed [
      set eco-trade-targeted? true
      ask patch-here [set pcolor 67
      ]
    ]
  ]
  if recycling-program-int [
    let exposed n-of (citizen-rec-progr * 0.9) citizens
    ask exposed [
      set recycling-program-targeted? true
      ask patch-here [set pcolor 67
      ]
      if (not formal-group-initialized?) and random-float 1 < 0.5 [
        if not member? household-id enrolled-households [
          set enrolled-households lput household-id enrolled-households
                    ask citizens with [household-id = household-id] [
            set enrollment-tick 0
          ]
        ]
      ]
    ]
  ]
  if recycling-station-int [
    let exposed n-of (citizen-rec-stat * 0.9) citizens
    ask exposed [
      if any? recycling-stations in-radius 3 [
        set recycling-station-targeted? true
        ask patch-here [set pcolor 67
        ]
      ]
    ]
  ]
end

;to program-dropout;; removes households from the recycling program at fixed intervals; called in go
 ; if ticks > 0 and (ticks mod dropout-interval = 0) [
  ;  let n-drop floor (dropout-rate * length enrolled-households)
   ; if n-drop > 0 [
    ;  let to-drop n-of n-drop enrolled-households
     ; foreach to-drop [ hh ->
      ;  set enrolled-households remove hh enrolled-households
       ; set always-formal-group remove hh always-formal-group
      ;]
    ;]
  ;]
;end

to program-dropout ;; individual, asynchronous, time-growing dropout
  if ticks = 0 [ stop ]

  ;; daily hazard calibrated to 25% per 100 days
  let lambda (- ln (1 - dropout-rate)) / 100

  let to-drop []

  foreach enrolled-households [ hh ->
    let t ticks - [enrollment-tick] of one-of citizens with [household-id = hh]
    let p 1 - exp (- lambda * t)

    if random-float 1 < p [
      set to-drop lput hh to-drop
    ]
  ]

  foreach to-drop [ hh ->
    set enrolled-households remove hh enrolled-households
    set always-formal-group remove hh always-formal-group
  ]
end


to update-daily-summaries;; aggregates daily waste flows and updates cumulative totals; called in go just before tick
  ask citizens [ set daily-ppw-total daily-ppw-total + ppw-generated ]
  set daily-collected-formal (daily-collected-formal-formal + daily-collected-formal-7day + daily-collected-formal-stations)
  set daily-collected-informal sum [daily-collected] of informal-pickers
  set daily-uncollected-informal-residual sum [residual-stock] of citizens
  set daily-uncollected-informal-informal sum [informal-stock] of citizens
  set daily-uncollected-formal-formal sum [formal-stock] of citizens
  set daily-landfill (daily-landfill-direct + daily-uncollected-informal-residual + daily-uncollected-informal-informal + daily-uncollected-formal-formal)
  set cumulative-formal cumulative-formal + daily-collected-formal
  set cumulative-informal cumulative-informal + daily-collected-informal
  set cumulative-landfill cumulative-landfill + daily-landfill
end

to go
  if ticks >= num-ticks [ stop ]

  set motivation-pass-count 0
  set knowledge-pass-count 0
  set opportunity-pass-count 0


  reset-daily-state

  ;; Citizens actions
  generate-waste
  end-interventions
  check-separation-context
  check-disposal-context
  citizen-waste-allocation

  ;; Operational updates
  advance-7day-tick-counter
  detect-eligible-households
  maintain-formal-pickers-to-stations
  maintain-formal-pickers-to-enrolled-households
  formal-picker-collect-daily
  formal-picker-collect-HH
  formal-picker-collect-stations
  informal-picker-collect

  ;; Waste pickers dynamics
  log-daily-collection
  picker-disappear
  informal-picker-appear
  formal-picker-appear
  program-dropout
  update-daily-summaries
  citizen-count
  tick
end
@#$#@#$#@
GRAPHICS-WINDOW
536
24
1148
637
-1
-1
18.30303030303031
1
10
1
1
1
0
1
1
1
-16
16
-16
16
0
0
1
ticks
30.0

INPUTBOX
20
39
110
99
num-citizens
100.0
1
0
Number

BUTTON
1029
642
1084
676
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

INPUTBOX
22
128
109
188
high-income-%
16.0
1
0
Number

INPUTBOX
117
129
211
190
medium-high-income-%
14.0
1
0
Number

INPUTBOX
217
129
296
189
medium-income-%
38.0
1
0
Number

INPUTBOX
305
130
388
190
medium-low-income-%
21.0
1
0
Number

INPUTBOX
400
129
483
190
low-income-%
11.0
1
0
Number

BUTTON
1096
643
1151
678
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

INPUTBOX
952
642
1025
702
num-ticks
350.0
1
0
Number

TEXTBOX
23
16
318
50
NEIGHBORHOOD CONFIGURATION
14
0.0
1

INPUTBOX
140
203
280
264
num-recycling-stations
2.0
1
0
Number

INPUTBOX
15
268
139
328
num-formal-pickers
5.0
1
0
Number

INPUTBOX
145
269
271
329
num-informal-pickers
10.0
1
0
Number

PLOT
1168
20
1478
235
Daily disposal
days
kg/day
0.0
10.0
0.0
600.0
true
true
"" ""
PENS
"Formal disp." 1.0 0 -13345367 true "" "plot daily-formal"
"Informal disp." 1.0 0 -2674135 true "" "plot daily-informal"
"Res. waste" 1.0 0 -16777216 true "" "plot daily-residual"
"Total gen." 1.0 0 -7500403 true "" "plot daily-ppw-total"

PLOT
1165
241
1475
434
Daily collection
days
kg/day
0.0
10.0
0.0
2.0
true
true
"" ""
PENS
"Formal coll." 1.0 0 -13345367 true "" "ifelse count formal-pickers > 0\n[\n  let normalized-formal\n    ( (daily-collected-formal\n        - daily-collected-formal-7day\n        - daily-collected-formal-stations)\n      + (daily-collected-formal-7day / 7)\n      + (daily-collected-formal-stations / 10) )\n\n  plot normalized-formal\n]\n[\n  plot 0\n]\n"
"Informal coll." 1.0 0 -2674135 true "" "plot daily-collected-informal"
"Final disp." 1.0 0 -16777216 true "" "plot daily-landfill"

MONITOR
277
270
402
315
Formal pickers 
count formal-pickers
17
1
11

MONITOR
406
270
508
315
Informal pickers
count informal-pickers
17
1
11

PLOT
1166
438
1475
641
Cumulative collection
days
kg
0.0
10.0
0.0
10.0
true
true
"" ""
PENS
"Formal coll." 1.0 0 -13345367 true "" "plot cumulative-formal"
"Informal coll." 1.0 0 -2674135 true "" "plot cumulative-informal"
"Final disp." 1.0 0 -16777216 true "" "plot cumulative-landfill"

MONITOR
122
46
224
91
num-households
length remove-duplicates [household-id] of citizens
0
1
11

TEXTBOX
543
645
945
679
Households =Yellow patches // Formal pickers = Blue person \nInformal pickers = Red person // Recycl. stat. = Violet pentagons
14
0.0
1

TEXTBOX
16
342
251
368
LOCAL INTERVENTIONS\n
14
0.0
1

SWITCH
15
460
166
493
door-to-door
door-to-door
1
1
-1000

INPUTBOX
168
451
273
512
citizen-door
100.0
1
0
Number

SWITCH
190
398
280
431
workshops
workshops
1
1
-1000

INPUTBOX
284
380
357
441
citizen-wor
100.0
1
0
Number

SWITCH
276
462
417
495
parks-activities
parks-activities
1
1
-1000

INPUTBOX
421
451
526
512
citizen-par
100.0
1
0
Number

SWITCH
14
399
104
432
leaflets
leaflets
1
1
-1000

INPUTBOX
108
381
184
442
citizen-leaf
100.0
1
0
Number

SWITCH
359
401
453
434
soc-media
soc-media
1
1
-1000

INPUTBOX
455
381
527
442
citizen-soc
100.0
1
0
Number

TEXTBOX
17
364
167
382
Education and awareness 
11
0.0
1

TEXTBOX
20
521
170
539
Incentives\n
11
0.0
1

TEXTBOX
20
604
170
622
Infrastructure\n
11
0.0
1

CHOOSER
19
205
125
250
municipal-support
municipal-support
"High" "Medium" "Low"
1

TEXTBOX
21
106
464
124
Income level (% distribution in the population)
11
0.0
1

INPUTBOX
290
206
455
267
enrolled-recycling-program
3.0
1
0
Number

SWITCH
12
545
170
578
incentives
incentives
1
1
-1000

INPUTBOX
173
534
272
595
citizen-incent
100.0
1
0
Number

SWITCH
276
550
418
583
eco-trade
eco-trade
1
1
-1000

INPUTBOX
422
539
525
600
citizen-eco
100.0
1
0
Number

SWITCH
11
627
181
660
recycling-program-int
recycling-program-int
1
1
-1000

INPUTBOX
172
616
271
677
citizen-rec-progr
100.0
1
0
Number

SWITCH
273
633
419
666
recycling-station-int
recycling-station-int
1
1
-1000

INPUTBOX
422
620
522
681
citizen-rec-stat
100.0
1
0
Number

MONITOR
238
48
434
93
num-recycling-progr-households
length enrolled-households
17
1
11

PLOT
1490
20
1802
234
Waste pickers numbers
days
number of pickers
0.0
10.0
0.0
10.0
true
true
"" ""
PENS
"Formal" 1.0 0 -13345367 true "" "plot count formal-pickers\n"
"Informal" 1.0 0 -2674135 true "" "plot count informal-pickers\n"

PLOT
1490
244
1803
444
Waste pickers collection
days
kg/day
0.0
10.0
0.0
10.0
true
true
"" ""
PENS
"Formal" 1.0 0 -13345367 true "" "ifelse count formal-pickers > 0\n[\n  let normalized-formal\n    ( (daily-collected-formal\n        - daily-collected-formal-7day\n        - daily-collected-formal-stations)\n      + (daily-collected-formal-7day / 7)\n      + (daily-collected-formal-stations / 10) )\n\n  plot (normalized-formal / count formal-pickers)\n]\n[ plot 0 ]\n\n"
"Informal" 1.0 0 -2674135 true "" "ifelse count informal-pickers > 0\n  [ plot (daily-collected-informal / count informal-pickers) ]\n  [ plot 0 ]\n"

SLIDER
332
709
485
742
mean-opportunity
mean-opportunity
1
5
3.5
0.1
1
NIL
HORIZONTAL

SLIDER
175
709
321
742
mean-knowledge
mean-knowledge
1
5
3.5
0.1
1
NIL
HORIZONTAL

SLIDER
19
709
161
742
mean-motivation
mean-motivation
1
5
3.5
0.1
1
NIL
HORIZONTAL

TEXTBOX
21
689
342
707
COM-B Factors (baseline conditions are a mean of 3.5)
11
0.0
1

@#$#@#$#@
## WHAT IS IT?

(a general understanding of what the model is trying to show or explain)

## HOW IT WORKS

(what rules the agents use to create the overall behavior of the model)

## HOW TO USE IT

(how to use the model, including a description of each of the items in the Interface tab)

## THINGS TO NOTICE

(suggested things for the user to notice while running the model)

## THINGS TO TRY

(suggested things for the user to try to do (move sliders, switches, etc.) with the model)

## EXTENDING THE MODEL

(suggested things to add or change in the Code tab to make the model more complicated, detailed, accurate, etc.)

## NETLOGO FEATURES

(interesting or unusual features of NetLogo that the model uses, particularly in the Code tab; or where workarounds were needed for missing features)

## RELATED MODELS

(models in the NetLogo Models Library and elsewhere which are of related interest)

## CREDITS AND REFERENCES

(a reference to the model's URL on the web if it has one, as well as any other necessary credits, citations, and links)
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
15
Circle -1 true true 203 65 88
Circle -1 true true 70 65 162
Circle -1 true true 150 105 120
Polygon -7500403 true false 218 120 240 165 255 165 278 120
Circle -7500403 true false 214 72 67
Rectangle -1 true true 164 223 179 298
Polygon -1 true true 45 285 30 285 30 240 15 195 45 210
Circle -1 true true 3 83 150
Rectangle -1 true true 65 221 80 296
Polygon -1 true true 195 285 210 285 210 240 240 210 195 210
Polygon -7500403 true false 276 85 285 105 302 99 294 83
Polygon -7500403 true false 219 85 210 105 193 99 201 83

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

wolf
false
0
Polygon -16777216 true false 253 133 245 131 245 133
Polygon -7500403 true true 2 194 13 197 30 191 38 193 38 205 20 226 20 257 27 265 38 266 40 260 31 253 31 230 60 206 68 198 75 209 66 228 65 243 82 261 84 268 100 267 103 261 77 239 79 231 100 207 98 196 119 201 143 202 160 195 166 210 172 213 173 238 167 251 160 248 154 265 169 264 178 247 186 240 198 260 200 271 217 271 219 262 207 258 195 230 192 198 210 184 227 164 242 144 259 145 284 151 277 141 293 140 299 134 297 127 273 119 270 105
Polygon -7500403 true true -1 195 14 180 36 166 40 153 53 140 82 131 134 133 159 126 188 115 227 108 236 102 238 98 268 86 269 92 281 87 269 103 269 113

x
false
0
Polygon -7500403 true true 270 75 225 30 30 225 75 270
Polygon -7500403 true true 30 75 75 30 270 225 225 270
@#$#@#$#@
NetLogo 6.4.0
@#$#@#$#@
@#$#@#$#@
@#$#@#$#@
<experiments>
  <experiment name="disposal" repetitions="30" runMetricsEveryStep="true">
    <setup>setup</setup>
    <go>go</go>
    <metric>daily-formal</metric>
    <metric>daily-informal</metric>
    <metric>daily-residual</metric>
  </experiment>
  <experiment name="collection rates" repetitions="30" runMetricsEveryStep="true">
    <setup>setup</setup>
    <go>go</go>
    <metric>ifelse-value ((cumulative-formal + cumulative-informal + cumulative-landfill) &gt; 0) [ cumulative-formal / (cumulative-formal + cumulative-informal + cumulative-landfill) ] [ 0 ]</metric>
    <metric>ifelse-value ((cumulative-formal + cumulative-informal + cumulative-landfill) &gt; 0) [ cumulative-informal / (cumulative-formal + cumulative-informal + cumulative-landfill) ] [ 0 ]</metric>
    <metric>ifelse-value ((cumulative-formal + cumulative-informal + cumulative-landfill) &gt; 0) [ cumulative-landfill / (cumulative-formal + cumulative-informal + cumulative-landfill) ] [ 0 ]</metric>
  </experiment>
  <experiment name="analysis" repetitions="30" runMetricsEveryStep="true">
    <setup>setup</setup>
    <go>go</go>
    <metric>ifelse-value (daily-ppw-total &gt; 0) [ daily-formal / daily-ppw-total ] [ 0 ]</metric>
    <metric>ifelse-value (daily-ppw-total &gt; 0) [ daily-informal / daily-ppw-total ] [ 0 ]</metric>
    <metric>ifelse-value (daily-ppw-total &gt; 0) [ daily-residual / daily-ppw-total ] [ 0 ]</metric>
    <metric>ifelse-value ((cumulative-formal + cumulative-informal + cumulative-landfill) &gt; 0) [ cumulative-formal / (cumulative-formal + cumulative-informal + cumulative-landfill) ] [ 0 ]</metric>
    <metric>ifelse-value ((cumulative-formal + cumulative-informal + cumulative-landfill) &gt; 0) [ cumulative-informal / (cumulative-formal + cumulative-informal + cumulative-landfill) ] [ 0 ]</metric>
    <metric>ifelse-value ((cumulative-formal + cumulative-informal + cumulative-landfill) &gt; 0) [ cumulative-landfill / (cumulative-formal + cumulative-informal + cumulative-landfill) ] [ 0 ]</metric>
    <metric>count formal-pickers</metric>
    <metric>count informal-pickers</metric>
    <metric>ifelse-value (count formal-pickers &gt; 0) [ (daily-collected-formal-formal + daily-collected-formal-7day / 7 + daily-collected-formal-stations / 10) / count formal-pickers ] [ 0 ]</metric>
    <metric>ifelse-value (count informal-pickers &gt; 0) [ daily-collected-informal / count informal-pickers ] [ 0 ]</metric>
  </experiment>
  <experiment name="com-b" repetitions="30" runMetricsEveryStep="true">
    <setup>setup</setup>
    <go>go</go>
    <metric>daily-formal</metric>
    <metric>daily-informal</metric>
    <metric>daily-residual</metric>
    <steppedValueSet variable="mean-motivation" first="1" step="2" last="5"/>
    <steppedValueSet variable="mean-knowledge" first="1" step="2" last="5"/>
    <steppedValueSet variable="mean-opportunity" first="1" step="2" last="5"/>
  </experiment>
  <experiment name="collection per picker" repetitions="30" runMetricsEveryStep="true">
    <setup>setup</setup>
    <go>go</go>
    <metric>count formal-pickers</metric>
    <metric>count informal-pickers</metric>
    <metric>ifelse-value (count formal-pickers &gt; 0) [ (daily-collected-formal-formal + daily-collected-formal-7day / 7 + daily-collected-formal-stations / 10) / count formal-pickers ] [ 0 ]</metric>
    <metric>ifelse-value (count informal-pickers &gt; 0) [ daily-collected-informal / count informal-pickers ] [ 0 ]</metric>
  </experiment>
  <experiment name="sensitivity" repetitions="30" runMetricsEveryStep="true">
    <setup>setup</setup>
    <go>go</go>
    <metric>daily-formal</metric>
    <metric>daily-informal</metric>
    <metric>daily-residual</metric>
    <metric>ifelse-value ((cumulative-formal + cumulative-informal + cumulative-landfill) &gt; 0) [ cumulative-formal / (cumulative-formal + cumulative-informal + cumulative-landfill) ] [ 0 ]</metric>
    <metric>ifelse-value ((cumulative-formal + cumulative-informal + cumulative-landfill) &gt; 0) [ cumulative-informal / (cumulative-formal + cumulative-informal + cumulative-landfill) ] [ 0 ]</metric>
    <metric>ifelse-value ((cumulative-formal + cumulative-informal + cumulative-landfill) &gt; 0) [ cumulative-landfill / (cumulative-formal + cumulative-informal + cumulative-landfill) ] [ 0 ]</metric>
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
0
@#$#@#$#@
