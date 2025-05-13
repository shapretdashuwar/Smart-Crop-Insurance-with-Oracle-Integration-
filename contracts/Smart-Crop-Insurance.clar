(define-constant contract-owner tx-sender)
(define-constant min-insurance-amount u1000)
(define-constant max-insurance-amount u100000)
(define-constant oracle-address 'SP2J6ZY48GV1EZ5V2V5RB9MP66SW86PYKKNRV9EJ7)
(define-constant compensation-multiplier u100)

(define-data-var oracle-status bool false)
(define-data-var threshold-temperature uint u35)
(define-data-var min-rainfall uint u100)

(define-map policies
    { farmer: principal }
    {
        amount: uint,
        active: bool,
        start-block: uint,
        end-block: uint,
    }
)

(define-map claims
    { claim-id: uint }
    {
        farmer: principal,
        amount: uint,
        processed: bool,
    }
)

(define-data-var claim-nonce uint u0)

(define-read-only (get-policy (farmer principal))
    (map-get? policies { farmer: farmer })
)

(define-read-only (get-claim (claim-id uint))
    (map-get? claims { claim-id: claim-id })
)

(define-public (purchase-insurance
        (amount uint)
        (duration uint)
    )
    (let ((policy-cost (/ (* amount u5) u100)))
        (asserts! (>= amount min-insurance-amount) (err u1))
        (asserts! (<= amount max-insurance-amount) (err u2))
        (asserts! (is-eq (get-policy tx-sender) none) (err u3))
        (try! (stx-transfer? policy-cost tx-sender contract-owner))
        (ok (map-set policies { farmer: tx-sender } {
            amount: amount,
            active: true,
            start-block: stacks-block-height,
            end-block: (+ stacks-block-height duration),
        }))
    )
)

(define-public (submit-weather-data
        (temperature uint)
        (rainfall uint)
    )
    (begin
        (asserts! (is-eq tx-sender oracle-address) (err u4))
        (var-set oracle-status true)
        (if (or
                (> temperature (var-get threshold-temperature))
                (< rainfall (var-get min-rainfall))
            )
            (process-claims)
            (ok true)
        )
    )
)

(define-public (file-claim)
    (let (
            (policy (unwrap! (get-policy tx-sender) (err u5)))
            (current-nonce (var-get claim-nonce))
        )
        (asserts! (> (get amount policy) u0) (err u6))
        (asserts! (get active policy) (err u7))
        (asserts! (<= stacks-block-height (get end-block policy)) (err u8))
        (var-set claim-nonce (+ current-nonce u1))
        (ok (map-set claims { claim-id: current-nonce } {
            farmer: tx-sender,
            amount: (get amount policy),
            processed: false,
        }))
    )
)

(define-private (process-claims)
    (let ((current-nonce (var-get claim-nonce)))
        (map process-single-claim (list u0 current-nonce))
        (ok true)
    )
)

(define-private (process-single-claim (claim-id uint))
    (let ((claim (unwrap! (get-claim claim-id) (ok false))))
        (if (and
                (not (get processed claim))
                (is-some (get-policy (get farmer claim)))
            )
            (begin
                (try! (stx-transfer? (get amount claim) contract-owner
                    (get farmer claim)
                ))
                (map-set claims { claim-id: claim-id }
                    (merge claim { processed: true })
                )
                (ok true)
            )
            (ok false)
        )
    )
)

(define-public (cancel-policy)
    (let ((policy (unwrap! (get-policy tx-sender) (err u9))))
        (asserts! (get active policy) (err u10))
        (ok (map-set policies { farmer: tx-sender } (merge policy { active: false })))
    )
)
