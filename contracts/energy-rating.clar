(define-map user-ratings principal
  {
    total-rating: uint,
    rating-count: uint,
    completed-transactions: uint
  }
)

(define-map escrow-ratings uint
  {
    producer-rated: bool,
    consumer-rated: bool,
    producer-rating: uint,
    consumer-rating: uint
  }
)

(define-constant min-rating u1)
(define-constant max-rating u5)
(define-constant rating-multiplier u100)

(define-public (rate-transaction (escrow-id uint) (rating uint) (rate-producer bool))
  (let 
    (
      (agreement (unwrap! (contract-call? .energy-escrow get-escrow-details escrow-id) (err u1)))
      (existing-rating (default-to 
        { producer-rated: false, consumer-rated: false, producer-rating: u0, consumer-rating: u0 }
        (map-get? escrow-ratings escrow-id)))
      (rater tx-sender)
      (ratee (if rate-producer (get producer agreement) (get consumer agreement)))
    )
    (begin
      (asserts! (is-eq (get status agreement) "completed") (err u2))
      (asserts! (and (>= rating min-rating) (<= rating max-rating)) (err u3))
      (asserts! (or (is-eq rater (get producer agreement)) (is-eq rater (get consumer agreement))) (err u4))
      (if rate-producer
        (begin
          (asserts! (is-eq rater (get consumer agreement)) (err u5))
          (asserts! (not (get producer-rated existing-rating)) (err u6))
          (map-set escrow-ratings escrow-id
            (merge existing-rating { producer-rated: true, producer-rating: rating }))
        )
        (begin
          (asserts! (is-eq rater (get producer agreement)) (err u5))
          (asserts! (not (get consumer-rated existing-rating)) (err u6))
          (map-set escrow-ratings escrow-id
            (merge existing-rating { consumer-rated: true, consumer-rating: rating }))
        )
      )
      (update-user-rating ratee rating)
      (ok true)
    )
  )
)

(define-private (update-user-rating (user principal) (new-rating uint))
  (let 
    (
      (current-data (default-to 
        { total-rating: u0, rating-count: u0, completed-transactions: u0 }
        (map-get? user-ratings user)))
      (new-total (+ (get total-rating current-data) new-rating))
      (new-count (+ (get rating-count current-data) u1))
      (new-transactions (+ (get completed-transactions current-data) u1))
    )
    (map-set user-ratings user
      {
        total-rating: new-total,
        rating-count: new-count,
        completed-transactions: new-transactions
      }
    )
  )
)

(define-read-only (get-user-rating (user principal))
  (match (map-get? user-ratings user)
    user-data (ok {
      average-rating: (if (> (get rating-count user-data) u0)
        (/ (* (get total-rating user-data) rating-multiplier) (get rating-count user-data))
        u0),
      total-ratings: (get rating-count user-data),
      completed-transactions: (get completed-transactions user-data)
    })
    (err u1)
  )
)

(define-read-only (get-escrow-rating (escrow-id uint))
  (map-get? escrow-ratings escrow-id)
)

(define-read-only (get-user-reputation-score (user principal))
  (match (map-get? user-ratings user)
    user-data (ok 
      (if (> (get rating-count user-data) u0)
        (+ (* (/ (* (get total-rating user-data) rating-multiplier) (get rating-count user-data)) u2)
           (get completed-transactions user-data))
        u0))
    (err u1)
  )
)

(define-read-only (is-user-eligible-for-rating (user principal) (escrow-id uint))
  (match (contract-call? .energy-escrow get-escrow-details escrow-id)
    agreement (ok (and 
      (is-eq (get status agreement) "completed")
      (or (is-eq user (get producer agreement)) (is-eq user (get consumer agreement)))))
    (err u1)
  )
)
