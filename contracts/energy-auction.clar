(define-map energy-auctions uint
  {
    producer: principal,
    energy-amount: uint,
    minimum-bid: uint,
    current-highest-bid: uint,
    highest-bidder: (optional principal),
    auction-start: uint,
    auction-end: uint,
    status: (string-ascii 20),
    bid-count: uint,
    reserve-met: bool
  }
)

(define-map auction-bids {auction-id: uint, bidder: principal}
  {
    bid-amount: uint,
    bid-time: uint,
    is-active: bool
  }
)

(define-map user-locked-funds principal uint)

(define-map auction-bidders {auction-id: uint, bidder-index: uint} principal)
(define-map auction-bidder-count uint uint)

(define-data-var auction-counter uint u0)
(define-data-var contract-fee-rate uint u50)
(define-data-var minimum-auction-duration uint u72)
(define-data-var maximum-auction-duration uint u1008)

(define-constant contract-owner tx-sender)
(define-constant err-unauthorized (err u100))
(define-constant err-auction-not-found (err u101))
(define-constant err-auction-ended (err u102))
(define-constant err-auction-active (err u103))
(define-constant err-insufficient-bid (err u104))
(define-constant err-invalid-duration (err u105))
(define-constant err-self-bid (err u106))
(define-constant err-insufficient-funds (err u107))
(define-constant err-no-bids (err u108))
(define-constant err-already-finalized (err u109))

(define-public (create-auction (energy-amount uint) (minimum-bid uint) (duration uint))
  (let 
    (
      (auction-id (+ (var-get auction-counter) u1))
      (auction-start stacks-block-height)
      (auction-end (+ stacks-block-height duration))
    )
    (begin
      (asserts! (> energy-amount u0) (err u110))
      (asserts! (> minimum-bid u0) (err u111))
      (asserts! (and 
        (>= duration (var-get minimum-auction-duration))
        (<= duration (var-get maximum-auction-duration))) err-invalid-duration)
      
      (var-set auction-counter auction-id)
      
      (map-set energy-auctions auction-id
        {
          producer: tx-sender,
          energy-amount: energy-amount,
          minimum-bid: minimum-bid,
          current-highest-bid: u0,
          highest-bidder: none,
          auction-start: auction-start,
          auction-end: auction-end,
          status: "active",
          bid-count: u0,
          reserve-met: false
        }
      )
      
      (map-set auction-bidder-count auction-id u0)
      
      (ok auction-id)
    )
  )
)

(define-public (place-bid (auction-id uint) (bid-amount uint))
  (let 
    (
      (auction (unwrap! (map-get? energy-auctions auction-id) err-auction-not-found))
      (current-locked (default-to u0 (map-get? user-locked-funds tx-sender)))
      (existing-bid (map-get? auction-bids {auction-id: auction-id, bidder: tx-sender}))
      (bidder-count (unwrap! (map-get? auction-bidder-count auction-id) (err u112)))
      (previous-bid-amount (match existing-bid bid (get bid-amount bid) u0))
      (additional-funds-needed (if (> bid-amount previous-bid-amount) 
                                (- bid-amount previous-bid-amount) 
                                u0))
    )
    (begin
      (asserts! (not (is-eq tx-sender (get producer auction))) err-self-bid)
      (asserts! (is-eq (get status auction) "active") err-auction-ended)
      (asserts! (<= stacks-block-height (get auction-end auction)) err-auction-ended)
      (asserts! (>= bid-amount (get minimum-bid auction)) err-insufficient-bid)
      (asserts! (> bid-amount (get current-highest-bid auction)) err-insufficient-bid)
      
      (if (> additional-funds-needed u0)
        (try! (stx-transfer? additional-funds-needed tx-sender (as-contract tx-sender)))
        true
      )
      
      (map-set user-locked-funds tx-sender (+ current-locked additional-funds-needed))
      
      (map-set auction-bids {auction-id: auction-id, bidder: tx-sender}
        {
          bid-amount: bid-amount,
          bid-time: stacks-block-height,
          is-active: true
        }
      )
      
      (if (is-none existing-bid)
        (begin
          (map-set auction-bidders {auction-id: auction-id, bidder-index: bidder-count} tx-sender)
          (map-set auction-bidder-count auction-id (+ bidder-count u1))
        )
        true
      )
      
      (map-set energy-auctions auction-id
        (merge auction {
          current-highest-bid: bid-amount,
          highest-bidder: (some tx-sender),
          bid-count: (+ (get bid-count auction) u1),
          reserve-met: (>= bid-amount (get minimum-bid auction))
        })
      )
      
      (ok true)
    )
  )
)

(define-public (finalize-auction (auction-id uint))
  (let 
    (
      (auction (unwrap! (map-get? energy-auctions auction-id) err-auction-not-found))
      (winner (get highest-bidder auction))
      (winning-bid (get current-highest-bid auction))
      (contract-fee (/ (* winning-bid (var-get contract-fee-rate)) u10000))
      (producer-payout (- winning-bid contract-fee))
    )
    (begin
      (asserts! (> stacks-block-height (get auction-end auction)) err-auction-active)
      (asserts! (is-eq (get status auction) "active") err-already-finalized)
      (asserts! (get reserve-met auction) err-no-bids)
      
      ;; Pay producer
      (try! (match winner
        winning-bidder (begin
          (try! (as-contract (stx-transfer? producer-payout tx-sender (get producer auction))))
          (let ((winner-locked (default-to u0 (map-get? user-locked-funds winning-bidder))))
            (map-set user-locked-funds winning-bidder (- winner-locked winning-bid))
          )
          (ok true)
        )
        (err u113)
      ))
      
      ;; Mark auction as completed - refunds will be processed separately
      (map-set energy-auctions auction-id
        (merge auction { status: "completed" })
      )
      
      (ok true)
    )
  )
)

;; Separate function for individual bidder refunds
(define-public (claim-refund (auction-id uint))
  (let 
    (
      (auction (unwrap! (map-get? energy-auctions auction-id) err-auction-not-found))
      (bid-data (unwrap! (map-get? auction-bids {auction-id: auction-id, bidder: tx-sender}) (err u116)))
      (winner (get highest-bidder auction))
      (is-winner (match winner w (is-eq tx-sender w) false))
    )
    (begin
      (asserts! (is-eq (get status auction) "completed") (err u121))
      (asserts! (not is-winner) (err u122))
      (asserts! (get is-active bid-data) (err u123))
      
      (let 
        (
          (refund-amount (get bid-amount bid-data))
          (bidder-locked (default-to u0 (map-get? user-locked-funds tx-sender)))
        )
        (begin
          (try! (as-contract (stx-transfer? refund-amount tx-sender tx-sender)))
          (map-set user-locked-funds tx-sender (- bidder-locked refund-amount))
          (map-set auction-bids {auction-id: auction-id, bidder: tx-sender}
            (merge bid-data { is-active: false })
          )
          (ok true)
        )
      )
    )
  )
)

(define-public (cancel-auction (auction-id uint))
  (let 
    (
      (auction (unwrap! (map-get? energy-auctions auction-id) err-auction-not-found))
    )
    (begin
      (asserts! (is-eq tx-sender (get producer auction)) err-unauthorized)
      (asserts! (is-eq (get status auction) "active") err-already-finalized)
      (asserts! (is-eq (get bid-count auction) u0) (err u117))
      
      (map-set energy-auctions auction-id
        (merge auction { status: "cancelled" })
      )
      
      (ok true)
    )
  )
)

(define-read-only (get-auction-details (auction-id uint))
  (map-get? energy-auctions auction-id)
)

(define-read-only (get-user-bid (auction-id uint) (bidder principal))
  (map-get? auction-bids {auction-id: auction-id, bidder: bidder})
)

(define-read-only (get-user-locked-funds (user principal))
  (default-to u0 (map-get? user-locked-funds user))
)

(define-read-only (get-total-auctions)
  (ok (var-get auction-counter))
)

(define-read-only (is-auction-active (auction-id uint))
  (match (map-get? energy-auctions auction-id)
    auction (ok (and 
      (is-eq (get status auction) "active")
      (<= stacks-block-height (get auction-end auction))))
    (err u118)
  )
)

(define-read-only (get-auction-time-remaining (auction-id uint))
  (match (map-get? energy-auctions auction-id)
    auction (ok (if (> (get auction-end auction) stacks-block-height)
                  (- (get auction-end auction) stacks-block-height)
                  u0))
    err-auction-not-found
  )
)

(define-read-only (get-contract-fee-rate)
  (ok (var-get contract-fee-rate))
)

(define-public (update-fee-rate (new-rate uint))
  (begin
    (asserts! (is-eq tx-sender contract-owner) err-unauthorized)
    (asserts! (<= new-rate u1000) (err u119))
    (var-set contract-fee-rate new-rate)
    (ok true)
  )
)

(define-public (update-auction-duration-limits (min-duration uint) (max-duration uint))
  (begin
    (asserts! (is-eq tx-sender contract-owner) err-unauthorized)
    (asserts! (< min-duration max-duration) (err u120))
    (var-set minimum-auction-duration min-duration)
    (var-set maximum-auction-duration max-duration)
    (ok true)
  )
)



