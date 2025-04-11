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
