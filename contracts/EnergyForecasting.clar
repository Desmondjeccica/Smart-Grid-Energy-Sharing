;; EnergyForecasting Contract
;; Demand prediction and consumption forecasting for smart grid optimization
;; Tracks usage patterns and provides efficiency insights

(define-constant CONTRACT_OWNER tx-sender)
(define-constant ERR_UNAUTHORIZED (err u500))
(define-constant ERR_INVALID_DATA (err u501))
(define-constant ERR_NO_HISTORY (err u502))
(define-constant ERR_INVALID_PERIOD (err u503))

(define-data-var forecast-counter uint u0)
(define-data-var grid-efficiency-score uint u50)

;; Historical energy usage patterns
(define-map user-energy-history
  { user: principal, time-period: uint }
  {
    energy-consumed: uint,
    energy-produced: uint,
    peak-usage-hour: uint,
    efficiency-rating: uint,
    recorded-at: uint
  }
)

;; Grid demand forecasts
(define-map demand-forecasts
  { forecast-id: uint }
  {
    period-start: uint,
    period-end: uint,
    predicted-total-demand: uint,
    predicted-peak-demand: uint,
    predicted-supply: uint,
    confidence-level: uint,
    created-at: uint,
    forecast-type: (string-ascii 20)
  }
)

;; User consumption predictions
(define-map user-consumption-forecasts
  { user: principal, period: uint }
  {
    predicted-consumption: uint,
    predicted-production: uint,
    optimal-trading-time: uint,
    efficiency-recommendation: (string-ascii 100),
    last-updated: uint
  }
)

;; Grid performance metrics
(define-map grid-performance-stats
  { period: uint }
  {
    total-energy-shared: uint,
    peak-demand: uint,
    supply-utilization: uint,
    grid-balance: uint,
    efficiency-score: uint
  }
)

;; Record user energy usage for forecasting
(define-public (record-energy-usage (energy-consumed uint) (energy-produced uint) (peak-hour uint))
  (let (
    (user tx-sender)
    (current-period (/ stacks-block-height u144)) ;; Daily periods (144 blocks ~= 1 day)
    (efficiency-rating (calculate-user-efficiency energy-consumed energy-produced))
  )
    ;; Validate inputs
    (asserts! (<= peak-hour u24) ERR_INVALID_DATA)
    
    (map-set user-energy-history { user: user, time-period: current-period } {
      energy-consumed: energy-consumed,
      energy-produced: energy-produced,
      peak-usage-hour: peak-hour,
      efficiency-rating: efficiency-rating,
      recorded-at: stacks-block-height
    })
    
    ;; Update grid performance stats
    (update-grid-performance-stats current-period energy-consumed energy-produced)
    
    (ok true)
  )
)

;; Generate demand forecast for a period
(define-public (create-demand-forecast (period-duration uint) (forecast-type (string-ascii 20)))
  (let (
    (forecast-id (+ (var-get forecast-counter) u1))
    (current-block stacks-block-height)
    (period-end (+ current-block period-duration))
  )
    ;; Validate inputs
    (asserts! (> period-duration u0) ERR_INVALID_PERIOD)
    (asserts! (or (is-eq forecast-type "daily") (is-eq forecast-type "weekly") (is-eq forecast-type "monthly")) ERR_INVALID_DATA)
    
    ;; Calculate basic forecast (simplified model)
    (let (
      (predicted-demand (calculate-predicted-demand))
      (predicted-supply (calculate-predicted-supply))
      (confidence (calculate-forecast-confidence))
    )
      (map-set demand-forecasts { forecast-id: forecast-id } {
        period-start: current-block,
        period-end: period-end,
        predicted-total-demand: predicted-demand,
        predicted-peak-demand: (+ predicted-demand (/ predicted-demand u4)),
        predicted-supply: predicted-supply,
        confidence-level: confidence,
        created-at: current-block,
        forecast-type: forecast-type
      })
    )
    
    (var-set forecast-counter forecast-id)
    (ok forecast-id)
  )
)

;; Generate user-specific consumption forecast
(define-public (create-user-forecast (user principal))
  (let (
    (current-period (/ stacks-block-height u144))
    (user-history (get-user-average-usage user))
    (predicted-consumption (get consumption user-history))
    (predicted-production (get production user-history))
    (optimal-time (calculate-optimal-trading-time user))
  )
    ;; Generate recommendations
    (let (
      (efficiency-rec (generate-efficiency-recommendation predicted-consumption predicted-production))
    )
      (map-set user-consumption-forecasts { user: user, period: current-period } {
        predicted-consumption: predicted-consumption,
        predicted-production: predicted-production,
        optimal-trading-time: optimal-time,
        efficiency-recommendation: efficiency-rec,
        last-updated: stacks-block-height
      })
    )
    
    (ok true)
  )
)

;; Update grid efficiency score
(define-public (update-grid-efficiency)
  (begin
    (asserts! (is-eq tx-sender CONTRACT_OWNER) ERR_UNAUTHORIZED)
    (let (
      (new-efficiency (calculate-current-grid-efficiency))
    )
      (var-set grid-efficiency-score new-efficiency)
      (ok new-efficiency)
    )
  )
)

;; Read-only functions

(define-read-only (get-demand-forecast (forecast-id uint))
  (map-get? demand-forecasts { forecast-id: forecast-id })
)

(define-read-only (get-user-forecast (user principal) (period uint))
  (map-get? user-consumption-forecasts { user: user, period: period })
)

(define-read-only (get-user-energy-history (user principal) (period uint))
  (map-get? user-energy-history { user: user, time-period: period })
)

(define-read-only (get-grid-performance (period uint))
  (map-get? grid-performance-stats { period: period })
)

(define-read-only (get-current-grid-efficiency)
  (var-get grid-efficiency-score)
)

(define-read-only (get-optimal-trading-time (user principal))
  (calculate-optimal-trading-time user)
)

;; Private helper functions

(define-private (calculate-user-efficiency (consumed uint) (produced uint))
  (if (> consumed u0)
    (if (> produced consumed)
      u5 ;; High efficiency - producing more than consuming
      (if (> produced (/ consumed u2))
        u3 ;; Medium efficiency
        u1 ;; Low efficiency
      )
    )
    u3 ;; Neutral if no consumption
  )
)

(define-private (get-user-average-usage (user principal))
  ;; Simplified average calculation - in reality would analyze multiple periods
  (let (
    (current-period (/ stacks-block-height u144))
    (recent-data (default-to 
      { energy-consumed: u20, energy-produced: u10, peak-usage-hour: u12, efficiency-rating: u3, recorded-at: u0 }
      (map-get? user-energy-history { user: user, time-period: current-period })))
  )
    { consumption: (get energy-consumed recent-data), production: (get energy-produced recent-data) }
  )
)

(define-private (calculate-predicted-demand)
  ;; Simplified demand prediction - in reality would use historical data analysis
  u100
)

(define-private (calculate-predicted-supply)
  ;; Simplified supply prediction
  u80
)

(define-private (calculate-forecast-confidence)
  ;; Simplified confidence calculation
  u75
)

(define-private (calculate-optimal-trading-time (user principal))
  ;; Calculate optimal time based on grid demand patterns
  ;; Returns block number representing optimal trading time
  (let (
    (current-hour (mod (/ stacks-block-height u6) u24)) ;; Approximate hour
  )
    ;; Recommend off-peak hours (simplified)
    (if (or (< current-hour u6) (> current-hour u22))
      (+ stacks-block-height u36) ;; Next off-peak period
      (+ stacks-block-height u144) ;; Next day
    )
  )
)

(define-private (generate-efficiency-recommendation (consumption uint) (production uint))
  (if (> production consumption)
    "Sell excess energy during peak hours"
    (if (> consumption (* production u2))
      "Reduce consumption or increase production"
      "Well balanced - maintain current levels"
    )
  )
)

(define-private (update-grid-performance-stats (period uint) (consumed uint) (produced uint))
  (let (
    (current-stats (default-to 
      { total-energy-shared: u0, peak-demand: u0, supply-utilization: u50, grid-balance: u50, efficiency-score: u50 }
      (map-get? grid-performance-stats { period: period })))
    (new-shared (+ (get total-energy-shared current-stats) consumed))
    (new-peak (if (> consumed (get peak-demand current-stats)) consumed (get peak-demand current-stats)))
    (new-balance (calculate-grid-balance consumed produced))
  )
    (map-set grid-performance-stats { period: period } (merge current-stats {
      total-energy-shared: new-shared,
      peak-demand: new-peak,
      grid-balance: new-balance
    }))
    true
  )
)

(define-private (calculate-grid-balance (demand uint) (supply uint))
  (if (> supply u0)
    (if (> demand supply)
      (/ (* supply u100) demand) ;; Supply ratio
      u100 ;; Oversupply
    )
    u0 ;; No supply
  )
)

(define-private (calculate-current-grid-efficiency)
  ;; Simplified efficiency calculation based on supply-demand balance
  (let (
    (current-period (/ stacks-block-height u144))
    (grid-stats (get-grid-performance current-period))
  )
    (match grid-stats
      stats (get grid-balance stats)
      u50 ;; Default efficiency
    )
  )
)
