;; Decentralized Prediction Market Protocol 

;; Constants
(define-constant ERR-NOT-ORACLE (err u1))
(define-constant ERR-MARKET-CLOSED (err u2))
(define-constant ERR-INVALID-EVENT (err u3))
(define-constant ERR-EVENT-RESOLVED (err u4))
(define-constant ERR-INSUFFICIENT-TOKENS (err u5))
(define-constant ERR-EVENT-EXISTS (err u6))
(define-constant ERR-ALREADY-PREDICTED (err u7))

;; Data Variables
(define-data-var market-oracle principal tx-sender)
(define-data-var market-active bool false)
(define-data-var entry-fee uint u1000000) ;; 1 prediction token minimum

;; Event Structure
(define-map events
    uint
    {
        title: (string-utf8 128),
        description: (string-utf8 512),
        reward-pool: uint,
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
        token-balance: uint
    }
)

;; Prediction Records
(define-map prediction-records
    {event-id: uint, predictor: principal}
    {
        prediction: bool,
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
        (ok true)))

(define-public (submit-event
    (event-id uint)
    (title (string-utf8 128))
    (description (string-utf8 512))
    (reward-pool uint))
    (let ()
        ;; Check market status
        (asserts! (var-get market-active) ERR-MARKET-CLOSED)
        
        ;; Check if event already exists
        (asserts! (is-none (map-get? events event-id)) ERR-EVENT-EXISTS)
        
        ;; Set the event data
        (map-set events event-id
            {
                title: title,
                description: description,
                reward-pool: reward-pool,
                predictions-true: u0,
                predictions-false: u0,
                resolved: false,
                outcome: false
            })
        
        (ok true)))

;; Membership Functions
(define-public (register-predictor (token-amount uint))
    (begin
        (asserts! (var-get market-active) ERR-MARKET-CLOSED)
        ;; Require minimum token amount
        (asserts! (>= token-amount (var-get entry-fee)) ERR-INSUFFICIENT-TOKENS)
        
        ;; Transfer tokens to market would be here in future version
        
        ;; Initialize predictor profile
        (map-set predictor-profiles tx-sender
            {
                token-balance: token-amount
            })
            
        (ok true)))

;; Prediction Functions
(define-public (make-prediction
    (event-id uint)
    (predict-true bool))
    (let (
        (event (unwrap! (map-get? events event-id) ERR-INVALID-EVENT))
        (predictor (unwrap! (map-get? predictor-profiles tx-sender) ERR-INSUFFICIENT-TOKENS))
        (token-balance (get token-balance predictor))
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
                stake-amount: token-balance
            })
        
        ;; Update prediction counts
        (if predict-true
            (map-set events event-id
                (merge event {predictions-true: (+ (get predictions-true event) u1)}))
            (map-set events event-id
                (merge event {predictions-false: (+ (get predictions-false event) u1)}))
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
        (asserts! (is-oracle) ERR-NOT-ORACLE)
        
        ;; Check event hasn't been resolved
        (asserts! (not (get resolved event)) ERR-EVENT-RESOLVED)
        
        ;; Update event status
        (map-set events event-id
            (merge event {
                resolved: true,
                outcome: outcome
            }))
            
        (ok true)))

;; Read-only functions
(define-read-only (get-event-details (event-id uint))
    (map-get? events event-id))

(define-read-only (get-predictor-profile (predictor principal))
    (map-get? predictor-profiles predictor))

(define-public (close-market)
    (begin
        (asserts! (is-oracle) ERR-NOT-ORACLE)
        (var-set market-active false)
        (ok true)))