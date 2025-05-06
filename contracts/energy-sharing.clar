(define-data-var total-energy-shared uint u0)

(define-map energy-producers principal 
  {
    available-energy: uint,
    energy-price: uint,
    total-earned: uint,
    active: bool
  }
)

(define-map energy-consumers principal 
  {
    energy-needed: uint,
    total-spent: uint,
    last-purchase: uint
  }
)

(define-constant contract-owner tx-sender)
(define-constant min-energy  u1)
(define-constant max-energy  u1000)
(define-constant min-price  u1)
(define-constant max-price  u100)

(define-public (register-as-producer (energy uint) (price uint))
  (begin
    (asserts! (is-ok (check-price price)) (err u1))
    (asserts! (is-ok (check-energy energy)) (err u2))
    (ok (map-set energy-producers tx-sender 
      {
        available-energy: energy,
        energy-price: price,
        total-earned: u0,
        active: true
      }
    ))
  )
)

(define-public (register-as-consumer (energy uint))
  (begin 
    (asserts! (is-ok (check-energy energy)) (err u2))
    (ok (map-set energy-consumers tx-sender
      {
        energy-needed: energy,
        total-spent: u0,
        last-purchase: u0
      }
    ))
  )
)

(define-public (update-available-energy (new-energy uint))
  (let ((producer (unwrap! (map-get? energy-producers tx-sender) (err u3))))
    (begin
      (asserts! (is-ok (check-energy new-energy)) (err u2))
      (ok (map-set energy-producers tx-sender 
        (merge producer { available-energy: new-energy })))
    )
  )
)

(define-public (update-energy-price (new-price uint))
  (let ((producer (unwrap! (map-get? energy-producers tx-sender) (err u3))))
    (begin
      (asserts! (is-ok (check-price new-price)) (err u1))
      (ok (map-set energy-producers tx-sender 
        (merge producer { energy-price: new-price })))
    )
  )
)

(define-public (buy-energy (producer principal) (amount uint))
  (let 
    (
      (seller (unwrap! (map-get? energy-producers producer) (err u3)))
      (buyer (unwrap! (map-get? energy-consumers tx-sender) (err u4)))
    )
    (begin
      (asserts! (>= (get available-energy seller) amount) (err u5))
      (asserts! (get active seller) (err u6))
      (try! (stx-transfer? (* amount (get energy-price seller)) tx-sender producer))
      (var-set total-energy-shared (+ (var-get total-energy-shared) amount))
      (map-set energy-producers producer
        (merge seller 
          { 
            available-energy: (- (get available-energy seller) amount),
            total-earned: (+ (get total-earned seller) (* amount (get energy-price seller)))
          }
        ))
      (map-set energy-consumers tx-sender
        (merge buyer
          {
            total-spent: (+ (get total-spent buyer) (* amount (get energy-price seller))),
            last-purchase: amount
          }
        ))
      (ok true)
    )
  )
)

(define-read-only (get-producer-details (who principal))
  (map-get? energy-producers who)
)

(define-read-only (get-consumer-details (who principal))
  (map-get? energy-consumers who)
)

(define-read-only (get-total-energy-shared)
  (ok (var-get total-energy-shared))
)

(define-private (check-energy (energy uint))
  (if (and (>= energy min-energy) (<= energy max-energy))
    (ok true)
    (err u2)
  )
)

(define-private (check-price (price uint))
  (if (and (>= price min-price) (<= price max-price))
    (ok true)
    (err u1)
  )
)

(define-public (toggle-producer-status)
  (let ((producer (unwrap! (map-get? energy-producers tx-sender) (err u3))))
    (ok (map-set energy-producers tx-sender 
      (merge producer { active: (not (get active producer)) })))
  )
)


(define-map energy-transactions uint 
  {
    producer: principal,
    consumer: principal,
    amount: uint,
    price: uint,
    timestamp: uint
  }
)

(define-data-var transaction-counter uint u0)

(define-public (record-transaction (producer principal) (amount uint) (price uint))
  (begin
    (var-set transaction-counter (+ (var-get transaction-counter) u1))
    (ok (map-set energy-transactions (var-get transaction-counter)
      {
        producer: producer,
        consumer: tx-sender,
        amount: amount,
        price: price,
        timestamp: stacks-block-height
      }
    ))
  )
)

(define-read-only (get-transaction (tx-id uint))
  (map-get? energy-transactions tx-id)
)

(define-read-only (get-user-transactions (user principal))
  (filter filter-user-transactions (map unwrap-transaction (get-transaction-ids)))
)

(define-private (get-transaction-ids)
  (list u1 u2 u3 u4 u5)
)

(define-private (unwrap-transaction (id uint))
  (default-to 
    {
      producer: contract-owner,
      consumer: contract-owner,
      amount: u0,
      price: u0,
      timestamp: u0
    }
    (map-get? energy-transactions id)
  )
)

(define-private (filter-user-transactions (tx {producer: principal, consumer: principal, amount: uint, price: uint, timestamp: uint}))
  (or
    (is-eq (get producer tx) tx-sender)
    (is-eq (get consumer tx) tx-sender)
  )
)

