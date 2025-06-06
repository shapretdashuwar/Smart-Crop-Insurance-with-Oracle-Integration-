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
(define-data-var base-premium-rate uint u5)
(define-data-var total-policies-issued uint u0)
(define-data-var total-claims-paid uint u0)
(define-data-var risk-adjustment-factor uint u100)

(define-map regional-risk
    { region: uint }
    {
        risk-multiplier: uint,
        total-policies: uint,
        total-claims: uint,
        last-updated: uint,
    }
)

(define-map farmer-regions
    { farmer: principal }
    { region: uint }
)

(define-constant region-low-risk u1)
(define-constant region-medium-risk u2)
(define-constant region-high-risk u3)

(define-private (initialize-regions)
    (begin
        (map-set regional-risk { region: region-low-risk } {
            risk-multiplier: u80,
            total-policies: u0,
            total-claims: u0,
            last-updated: stacks-block-height,
        })
        (map-set regional-risk { region: region-medium-risk } {
            risk-multiplier: u100,
            total-policies: u0,
            total-claims: u0,
            last-updated: stacks-block-height,
        })
        (map-set regional-risk { region: region-high-risk } {
            risk-multiplier: u130,
            total-policies: u0,
            total-claims: u0,
            last-updated: stacks-block-height,
        })
    )
)

(define-read-only (get-regional-risk (region uint))
    (map-get? regional-risk { region: region })
)

(define-read-only (get-farmer-region (farmer principal))
    (map-get? farmer-regions { farmer: farmer })
)

(define-private (calculate-claim-ratio)
    (let (
            (total-policies (var-get total-policies-issued))
            (total-claims (var-get total-claims-paid))
        )
        (if (> total-policies u0)
            (/ (* total-claims u100) total-policies)
            u0
        )
    )
)

(define-private (update-risk-adjustment)
    (let ((claim-ratio (calculate-claim-ratio)))
        (if (> claim-ratio u30)
            (var-set risk-adjustment-factor u120)
            (if (> claim-ratio u15)
                (var-set risk-adjustment-factor u110)
                (var-set risk-adjustment-factor u95)
            )
        )
    )
)

(define-private (calculate-adjusted-premium
        (base-amount uint)
        (region uint)
    )
    (let (
            (regional-data (unwrap! (get-regional-risk region) base-amount))
            (base-cost (/ (* base-amount (var-get base-premium-rate)) u100))
            (regional-adjustment (/ (* base-cost (get risk-multiplier regional-data)) u100))
            (risk-adjustment (/ (* regional-adjustment (var-get risk-adjustment-factor)) u100))
        )
        risk-adjustment
    )
)

(define-public (purchase-insurance-with-adjustment
        (amount uint)
        (duration uint)
        (region uint)
    )
    (let (
            (adjusted-premium (calculate-adjusted-premium amount region))
            (regional-data (unwrap! (get-regional-risk region) (err u13)))
        )
        (asserts! (>= amount min-insurance-amount) (err u1))
        (asserts! (<= amount max-insurance-amount) (err u2))
        (asserts! (is-eq (get-policy tx-sender) none) (err u3))
        (asserts! (<= region u3) (err u14))
        (try! (stx-transfer? adjusted-premium tx-sender contract-owner))
        (map-set farmer-regions { farmer: tx-sender } { region: region })
        (map-set regional-risk { region: region }
            (merge regional-data { total-policies: (+ (get total-policies regional-data) u1) })
        )
        (var-set total-policies-issued (+ (var-get total-policies-issued) u1))
        (update-risk-adjustment)
        (ok (map-set policies { farmer: tx-sender } {
            amount: amount,
            active: true,
            start-block: stacks-block-height,
            end-block: (+ stacks-block-height duration),
        }))
    )
)

(define-private (update-claim-statistics (farmer principal))
    (let (
            (farmer-region-data (get-farmer-region farmer))
            (region (get region (unwrap! farmer-region-data (ok false))))
            (regional-data (unwrap! (get-regional-risk region) (ok false)))
        )
        (map-set regional-risk { region: region }
            (merge regional-data { total-claims: (+ (get total-claims regional-data) u1) })
        )
        (var-set total-claims-paid (+ (var-get total-claims-paid) u1))
        (update-risk-adjustment)
        (ok true)
    )
)

(define-read-only (get-premium-quote
        (amount uint)
        (region uint)
    )
    (ok {
        base-premium: (/ (* amount (var-get base-premium-rate)) u100),
        adjusted-premium: (calculate-adjusted-premium amount region),
        risk-factor: (var-get risk-adjustment-factor),
        claim-ratio: (calculate-claim-ratio),
    })
)

(define-public (admin-update-base-rate (new-rate uint))
    (begin
        (asserts! (is-eq tx-sender contract-owner) (err u15))
        (asserts! (and (>= new-rate u1) (<= new-rate u20)) (err u16))
        (ok (var-set base-premium-rate new-rate))
    )
)
(define-constant large-claim-threshold u50000)
(define-constant required-signatures u2)
(define-constant max-validators u5)

(define-data-var validator-count uint u0)

(define-map authorized-validators
    { validator: principal }
    {
        active: bool,
        added-at: uint,
    }
)

(define-map claim-approvals
    { claim-id: uint }
    {
        signatures-required: uint,
        signatures-received: uint,
        approved: bool,
        rejected: bool,
        created-at: uint,
    }
)

(define-map validator-signatures
    {
        claim-id: uint,
        validator: principal,
    }
    {
        approved: bool,
        signed-at: uint,
    }
)

(define-map pending-large-claims
    { claim-id: uint }
    {
        farmer: principal,
        amount: uint,
        submitted-at: uint,
        requires-multisig: bool,
    }
)

(define-read-only (is-authorized-validator (validator principal))
    (match (map-get? authorized-validators { validator: validator })
        some-validator (get active some-validator)
        false
    )
)

(define-read-only (get-claim-approval-status (claim-id uint))
    (map-get? claim-approvals { claim-id: claim-id })
)

(define-read-only (get-validator-signature
        (claim-id uint)
        (validator principal)
    )
    (map-get? validator-signatures {
        claim-id: claim-id,
        validator: validator,
    })
)

(define-read-only (get-pending-large-claim (claim-id uint))
    (map-get? pending-large-claims { claim-id: claim-id })
)

(define-public (add-validator (validator principal))
    (begin
        (asserts! (is-eq tx-sender contract-owner) (err u17))
        (asserts! (< (var-get validator-count) max-validators) (err u18))
        (asserts! (not (is-authorized-validator validator)) (err u19))
        (map-set authorized-validators { validator: validator } {
            active: true,
            added-at: stacks-block-height,
        })
        (var-set validator-count (+ (var-get validator-count) u1))
        (ok true)
    )
)

(define-public (remove-validator (validator principal))
    (begin
        (asserts! (is-eq tx-sender contract-owner) (err u17))
        (asserts! (is-authorized-validator validator) (err u20))
        (map-set authorized-validators { validator: validator } {
            active: false,
            added-at: stacks-block-height,
        })
        (var-set validator-count (- (var-get validator-count) u1))
        (ok true)
    )
)

(define-public (file-claim-with-multisig)
    (let (
            (policy (unwrap! (get-policy tx-sender) (err u5)))
            (current-nonce (var-get claim-nonce))
            (claim-amount (get amount policy))
            (requires-multisig (>= claim-amount large-claim-threshold))
        )
        (asserts! (> claim-amount u0) (err u6))
        (asserts! (get active policy) (err u7))
        (asserts! (<= stacks-block-height (get end-block policy)) (err u8))
        (var-set claim-nonce (+ current-nonce u1))
        (map-set claims { claim-id: current-nonce } {
            farmer: tx-sender,
            amount: claim-amount,
            processed: false,
        })
        (if requires-multisig
            (begin
                (map-set pending-large-claims { claim-id: current-nonce } {
                    farmer: tx-sender,
                    amount: claim-amount,
                    submitted-at: stacks-block-height,
                    requires-multisig: true,
                })
                (map-set claim-approvals { claim-id: current-nonce } {
                    signatures-required: required-signatures,
                    signatures-received: u0,
                    approved: false,
                    rejected: false,
                    created-at: stacks-block-height,
                })
                (ok current-nonce)
            )
            (ok current-nonce)
        )
    )
)
(define-public (approve-large-claim
        (claim-id uint)
        (approve bool)
    )
    (let (
            (approval-status (unwrap! (get-claim-approval-status claim-id) (err u21)))
            (existing-signature (get-validator-signature claim-id tx-sender))
        )
        (asserts! (is-authorized-validator tx-sender) (err u22))
        (asserts! (not (get approved approval-status)) (err u23))
        (asserts! (not (get rejected approval-status)) (err u24))
        (asserts! (is-none existing-signature) (err u25))
        (map-set validator-signatures {
            claim-id: claim-id,
            validator: tx-sender,
        } {
            approved: approve,
            signed-at: stacks-block-height,
        })
        (if approve
            (let ((new-signature-count (+ (get signatures-received approval-status) u1)))
                (map-set claim-approvals { claim-id: claim-id }
                    (merge approval-status { signatures-received: new-signature-count })
                )
                (if (>= new-signature-count
                        (get signatures-required approval-status)
                    )
                    (begin
                        (map-set claim-approvals { claim-id: claim-id }
                            (merge approval-status { approved: true })
                        )
                        (process-approved-large-claim claim-id)
                    )
                    (ok true)
                )
            )
            (begin
                (map-set claim-approvals { claim-id: claim-id }
                    (merge approval-status { rejected: true })
                )
                (ok true)
            )
        )
    )
)

(define-private (process-approved-large-claim (claim-id uint))
    (let (
            (claim (unwrap! (get-claim claim-id) (err u26)))
            (approval-status (unwrap! (get-claim-approval-status claim-id) (err u21)))
        )
        (if (and
                (get approved approval-status)
                (not (get processed claim))
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
            (err u27)
        )
    )
)

(define-read-only (get-claim-details (claim-id uint))
    (let (
            (claim (get-claim claim-id))
            (approval-status (get-claim-approval-status claim-id))
            (pending-claim (get-pending-large-claim claim-id))
        )
        (ok {
            claim: claim,
            approval-status: approval-status,
            pending-claim: pending-claim,
        })
    )
)

(define-public (emergency-process-claim (claim-id uint))
    (let ((claim (unwrap! (get-claim claim-id) (err u26))))
        (asserts! (is-eq tx-sender contract-owner) (err u17))
        (asserts! (not (get processed claim)) (err u28))
        (try! (stx-transfer? (get amount claim) contract-owner (get farmer claim)))
        (map-set claims { claim-id: claim-id } (merge claim { processed: true }))
        (ok true)
    )
)
