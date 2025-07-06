(define-map escrow-agreements uint
  {
    producer: principal,
    consumer: principal,
    amount: uint,
    price-per-unit: uint,
    total-cost: uint,
    status: (string-ascii 20),
    created-at: uint,
    expires-at: uint
  }
)

(define-map escrow-balances uint uint)

(define-data-var escrow-counter uint u0)
(define-constant escrow-duration u144)
(define-constant contract-owner tx-sender)

(define-public (create-escrow (producer principal) (amount uint) (price-per-unit uint))
  (let 
    (
      (escrow-id (+ (var-get escrow-counter) u1))
      (total-cost (* amount price-per-unit))
    )
    (begin
      (try! (stx-transfer? total-cost tx-sender (as-contract tx-sender)))
      (var-set escrow-counter escrow-id)
      (map-set escrow-agreements escrow-id
        {
          producer: producer,
          consumer: tx-sender,
          amount: amount,
          price-per-unit: price-per-unit,
          total-cost: total-cost,
          status: "pending",
          created-at: stacks-block-height,
          expires-at: (+ stacks-block-height escrow-duration)
        }
      )
      (map-set escrow-balances escrow-id total-cost)
      (ok escrow-id)
    )
  )
)

(define-public (confirm-delivery (escrow-id uint))
  (let 
    (
      (agreement (unwrap! (map-get? escrow-agreements escrow-id) (err u1)))
      (balance (unwrap! (map-get? escrow-balances escrow-id) (err u2)))
    )
    (begin
      (asserts! (is-eq tx-sender (get consumer agreement)) (err u3))
      (asserts! (is-eq (get status agreement) "pending") (err u4))
      (try! (as-contract (stx-transfer? balance tx-sender (get producer agreement))))
      (map-set escrow-agreements escrow-id
        (merge agreement { status: "completed" })
      )
      (map-delete escrow-balances escrow-id)
      (ok true)
    )
  )
)

(define-public (dispute-delivery (escrow-id uint))
  (let 
    (
      (agreement (unwrap! (map-get? escrow-agreements escrow-id) (err u1)))
    )
    (begin
      (asserts! (is-eq tx-sender (get consumer agreement)) (err u3))
      (asserts! (is-eq (get status agreement) "pending") (err u4))
      (map-set escrow-agreements escrow-id
        (merge agreement { status: "disputed" })
      )
      (ok true)
    )
  )
)

(define-public (claim-expired-escrow (escrow-id uint))
  (let 
    (
      (agreement (unwrap! (map-get? escrow-agreements escrow-id) (err u1)))
      (balance (unwrap! (map-get? escrow-balances escrow-id) (err u2)))
    )
    (begin
      (asserts! (is-eq tx-sender (get consumer agreement)) (err u3))
      (asserts! (is-eq (get status agreement) "pending") (err u4))
      (asserts! (> stacks-block-height (get expires-at agreement)) (err u5))
      (try! (as-contract (stx-transfer? balance tx-sender (get consumer agreement))))
      (map-set escrow-agreements escrow-id
        (merge agreement { status: "expired" })
      )
      (map-delete escrow-balances escrow-id)
      (ok true)
    )
  )
)

(define-public (resolve-dispute (escrow-id uint) (release-to-producer bool))
  (let 
    (
      (agreement (unwrap! (map-get? escrow-agreements escrow-id) (err u1)))
      (balance (unwrap! (map-get? escrow-balances escrow-id) (err u2)))
      (recipient (if release-to-producer (get producer agreement) (get consumer agreement)))
    )
    (begin
      (asserts! (is-eq tx-sender contract-owner) (err u6))
      (asserts! (is-eq (get status agreement) "disputed") (err u4))
      (try! (as-contract (stx-transfer? balance tx-sender recipient)))
      (map-set escrow-agreements escrow-id
        (merge agreement { status: "resolved" })
      )
      (map-delete escrow-balances escrow-id)
      (ok true)
    )
  )
)

(define-read-only (get-escrow-details (escrow-id uint))
  (map-get? escrow-agreements escrow-id)
)

(define-read-only (get-escrow-balance (escrow-id uint))
  (map-get? escrow-balances escrow-id)
)

(define-read-only (get-total-escrows)
  (ok (var-get escrow-counter))
)

(define-read-only (is-escrow-expired (escrow-id uint))
  (match (map-get? escrow-agreements escrow-id)
    agreement (ok (> stacks-block-height (get expires-at agreement)))
    (err u1)
  )
)