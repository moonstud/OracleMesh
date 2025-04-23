;; Decentralized Prediction Market Protocol - Version 2
;; Enhanced with prediction power, liquidity pool, and token transfers

;; Constants
(define-constant ERR-NOT-ORACLE (err u1))
(define-constant ERR-MARKET-CLOSED (err u2))
(define-constant ERR-INVALID-EVENT (err u3))
(define-constant ERR-EVENT-RESOLVED (err u4))
(define-constant ERR-INVALID-PARAMETER (err u5))
(define-constant ERR-INSUFFICIENT-TOKENS (err u6))
(define-constant ERR-EVENT-EXISTS (err u7))
(define-constant ERR-ALREADY-PREDICTED (err u8))
(define-constant ERR-NOT-AUTHORIZED (err u9))
(define-constant MAX-EVENT-ID u1000) ;; Maximum allowed event ID

;; Data Variables
(define-data-var market-oracle principal tx-sender)
(define-data-var market-active bool false)
(define-data-var prediction-round uint u0)
(define-data-var entry-fee uint u1000000) ;; 1 prediction token minimum
(define-data-var liquidity-pool uint u0)

;; Event Structure
(define-map events
    uint
    {
        title: (string-utf8 128),
        description: (string-utf8 512),
        event-hash: (buff 32),     ;; SHA256 hash of the detailed event description
        reward-pool: uint,         ;; Amount of tokens allocated for winners
        predictions-true: uint,
        predictions-false: uint,
        resolved: bool,
        outcome: bool
    }
)

;; Predictor Profiles
(define-map predictor-profiles
    principal
    {
        token-balance: uint,
        events-created: (list 30 uint),
        prediction-power: uint     ;; Can be different from token balance (reputation)
    }
)

;; Prediction Records
(define-map prediction-records
    {event-id: uint, predictor: principal}
    {
        prediction: bool,         ;; true = will happen, false = won't happen
        stake-amount: uint
    }
)

;; Authorization
(define-private (is-oracle)
    (is-eq tx-sender (var-get market-oracle)))

;; Market Management Functions
(define-public (activate-market)
    (begin
        (asserts! (is-oracle) ERR-NOT-ORACLE)
        (var-set market-active true)
        (var-set prediction-round u0)
        (var-set liquidity-pool u0)
        (ok true)))

(define-public (submit-event
    (event-id uint)
    (title (string-utf8 128))
    (description (string-utf8 512))
    (event-hash (buff 32))
    (reward-pool uint))
    (let (
        (predictor-profile (unwrap! (map-get? predictor-profiles tx-sender) ERR-INSUFFICIENT-TOKENS))
        )
        
        ;; Check market status
        (asserts! (var-get market-active) ERR-MARKET-CLOSED)
        
        ;; Validate event-id is within acceptable range
        (asserts! (<= event-id MAX-EVENT-ID) ERR-INVALID-PARAMETER)
        
        ;; Check if event already exists
        (asserts! (is-none (map-get? events event-id)) ERR-EVENT-EXISTS)
        
        ;; Validate title and description are not empty
        (asserts! (> (len title) u0) ERR-INVALID-PARAMETER)
        (asserts! (> (len description) u0) ERR-INVALID-PARAMETER)
        
        ;; Check predictor has enough tokens to submit event
        (asserts! (>= (get token-balance predictor-profile) (var-get entry-fee)) ERR-INSUFFICIENT-TOKENS)
        
        ;; Set the event data
        (map-set events event-id
            {
                title: title,
                description: description,
                event-hash: event-hash,
                reward-pool: reward-pool,
                predictions-true: u0,
                predictions-false: u0,
                resolved: false,
                outcome: false
            })
        
        ;; Update predictor profile
        (map-set predictor-profiles tx-sender
            (merge predictor-profile {
                events-created: (unwrap! (as-max-len? 
                    (append (get events-created predictor-profile) event-id) u30)
                    ERR-INVALID-PARAMETER)
            }))
        
        (ok true)))

;; Membership Functions
(define-public (register-predictor (token-amount uint))
    (begin
        (asserts! (var-get market-active) ERR-MARKET-CLOSED)
        ;; Require minimum token amount
        (asserts! (>= token-amount (var-get entry-fee)) ERR-INSUFFICIENT-TOKENS)
        
        ;; Transfer tokens to market liquidity pool
        (try! (stx-transfer? token-amount tx-sender (var-get market-oracle)))
        
        ;; Initialize predictor profile
        (map-set predictor-profiles tx-sender
            {
                token-balance: token-amount,
                events-created: (list),
                prediction-power: token-amount
            })
            
        ;; Update liquidity pool
        (var-set liquidity-pool (+ (var-get liquidity-pool) token-amount))
        
        (ok true)))

;; Prediction Functions
(define-public (make-prediction
    (event-id uint)
    (predict-true bool))
    (let (
        (event (unwrap! (map-get? events event-id) ERR-INVALID-EVENT))
        (predictor (unwrap! (map-get? predictor-profiles tx-sender) ERR-INSUFFICIENT-TOKENS))
        (prediction-power (get prediction-power predictor))
        )
        
        ;; Check market status
        (asserts! (var-get market-active) ERR-MARKET-CLOSED)
        
        ;; Check event hasn't been resolved
        (asserts! (not (get resolved event)) ERR-EVENT-RESOLVED)
        
        ;; Check predictor hasn't already predicted
        (asserts! (is-none (map-get? prediction-records {event-id: event-id, predictor: tx-sender})) ERR-ALREADY-PREDICTED)
        
        ;; Record prediction
        (map-set prediction-records 
            {event-id: event-id, predictor: tx-sender}
            {
                prediction: predict-true,
                stake-amount: prediction-power
            })
        
        ;; Update prediction counts
        (if predict-true
            (map-set events event-id
                (merge event {predictions-true: (+ (get predictions-true event) prediction-power)}))
            (map-set events event-id
                (merge event {predictions-false: (+ (get predictions-false event) prediction-power)}))
        )
        
        (ok true)))

;; Event Resolution
(define-public (resolve-event (event-id uint) (outcome bool))
    (let (
        (event (unwrap! (map-get? events event-id) ERR-INVALID-EVENT))
        )
        
        ;; Check market status
        (asserts! (var-get market-active) ERR-MARKET-CLOSED)
        
        ;; Only oracle can resolve events
        (asserts! (is-oracle) ERR-NOT-AUTHORIZED)
        
        ;; Check event hasn't been resolved
        (asserts! (not (get resolved event)) ERR-EVENT-RESOLVED)
        
        ;; Update event status
        (map-set events event-id
            (merge event {
                resolved: true,
                outcome: outcome
            }))
        
        ;; If event has a reward pool, distribute it
        (if (> (get reward-pool event) u0)
            (begin
                ;; Ensure liquidity pool has enough balance
                (asserts! (>= (var-get liquidity-pool) (get reward-pool event)) ERR-INSUFFICIENT-TOKENS)
                
                ;; Update liquidity pool
                (var-set liquidity-pool (- (var-get liquidity-pool) (get reward-pool event)))
                
                (ok true))
            (ok false))))

;; Read-only functions
(define-read-only (get-event-details (event-id uint))
    (map-get? events event-id))

(define-read-only (get-predictor-profile (predictor principal))
    (map-get? predictor-profiles predictor))

(define-read-only (get-market-metrics)
    {
        active: (var-get market-active),
        prediction-round: (var-get prediction-round),
        liquidity-pool: (var-get liquidity-pool),
        entry-fee: (var-get entry-fee)
    })

(define-public (update-entry-fee (new-fee uint))
    (begin
        (asserts! (is-oracle) ERR-NOT-ORACLE)
        (var-set entry-fee new-fee)
        (ok true)))

(define-public (close-market)
    (begin
        (asserts! (is-oracle) ERR-NOT-ORACLE)
        (var-set market-active false)
        (ok true)))

(define-public (advance-prediction-round)
    (begin
        (asserts! (is-oracle) ERR-NOT-ORACLE)
        (asserts! (var-get market-active) ERR-MARKET-CLOSED)
        (var-set prediction-round (+ (var-get prediction-round) u1))
        (ok true)))

(define-public (transfer-oracle-role (new-oracle principal))
    (begin
        (asserts! (is-oracle) ERR-NOT-ORACLE)
        (var-set market-oracle new-oracle)
        (ok true)))