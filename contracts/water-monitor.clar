;; Water Conservation Management System
;; A municipal utility resource monitoring system with usage tracking, 
;; leak detection, and conservation incentive programs

;; Constants
(define-constant CONTRACT_OWNER tx-sender)
(define-constant ERR_NOT_AUTHORIZED (err u100))
(define-constant ERR_INVALID_AMOUNT (err u101))
(define-constant ERR_INSUFFICIENT_FUNDS (err u102))
(define-constant ERR_METER_NOT_FOUND (err u103))
(define-constant ERR_ALREADY_REGISTERED (err u104))
(define-constant ERR_LEAK_THRESHOLD_EXCEEDED (err u105))
(define-constant LEAK_THRESHOLD u1000) ;; gallons per hour threshold
(define-constant CONSERVATION_REWARD u50) ;; reward for conservation
(define-constant PENALTY_RATE u25) ;; penalty for excessive usage

;; Data Variables
(define-data-var total-water-consumed uint u0)
(define-data-var total-conservation-rewards uint u0)
(define-data-var next-meter-id uint u1)

;; Data Maps
(define-map water-meters 
  uint 
  {
    owner: principal,
    location: (string-ascii 50),
    current-usage: uint,
    monthly-quota: uint,
    last-reading: uint,
    leak-detected: bool,
    conservation-score: uint
  })

(define-map user-profiles 
  principal 
  {
    meter-id: uint,
    total-consumption: uint,
    conservation-points: uint,
    penalties: uint,
    rewards-earned: uint
  })

(define-map usage-history 
  {meter-id: uint, timestamp: uint} 
  {
    usage-amount: uint,
    rate-per-hour: uint,
    anomaly-detected: bool
  })

;; Private Functions
(define-private (is-owner-or-authorized (meter-id uint) (caller principal))
  (match (map-get? water-meters meter-id)
    meter (or (is-eq caller (get owner meter)) (is-eq caller CONTRACT_OWNER))
    false))

(define-private (calculate-conservation-score (usage uint) (quota uint))
  (if (<= usage (/ (* quota u75) u100))
    u100 ;; Excellent conservation
    (if (<= usage (/ (* quota u90) u100))
      u75  ;; Good conservation
      (if (<= usage quota)
        u50  ;; Average usage
        u25)))) ;; Over quota

(define-private (detect-leak (current-rate uint))
  (> current-rate LEAK_THRESHOLD))

(define-private (calculate-penalty (usage uint) (quota uint))
  (if (> usage quota)
    (* (- usage quota) PENALTY_RATE)
    u0))

;; Public Functions

;; Register a new water meter
(define-public (register-meter (location (string-ascii 50)) (monthly-quota uint))
  (let ((meter-id (var-get next-meter-id)))
    (asserts! (> monthly-quota u0) ERR_INVALID_AMOUNT)
    (map-set water-meters meter-id
      {
        owner: tx-sender,
        location: location,
        current-usage: u0,
        monthly-quota: monthly-quota,
        last-reading: u0,
        leak-detected: false,
        conservation-score: u100
      })
    (map-set user-profiles tx-sender
      {
        meter-id: meter-id,
        total-consumption: u0,
        conservation-points: u0,
        penalties: u0,
        rewards-earned: u0
      })
    (var-set next-meter-id (+ meter-id u1))
    (ok meter-id)))

;; Record water usage reading
(define-public (record-usage (meter-id uint) (usage-amount uint) (timestamp uint))
  (let ((meter (unwrap! (map-get? water-meters meter-id) ERR_METER_NOT_FOUND))
        (hourly-rate (if (> timestamp (get last-reading meter)) 
                      (/ usage-amount (- timestamp (get last-reading meter)))
                      u0))
        (leak-status (detect-leak hourly-rate))
        (new-total-usage (+ (get current-usage meter) usage-amount))
        (conservation-score (calculate-conservation-score new-total-usage (get monthly-quota meter))))
    
    (asserts! (is-owner-or-authorized meter-id tx-sender) ERR_NOT_AUTHORIZED)
    (asserts! (> usage-amount u0) ERR_INVALID_AMOUNT)
    
    ;; Update meter data
    (map-set water-meters meter-id
      (merge meter {
        current-usage: new-total-usage,
        last-reading: timestamp,
        leak-detected: leak-status,
        conservation-score: conservation-score
      }))
    
    ;; Record usage history
    (map-set usage-history {meter-id: meter-id, timestamp: timestamp}
      {
        usage-amount: usage-amount,
        rate-per-hour: hourly-rate,
        anomaly-detected: leak-status
      })
    
    ;; Update global consumption
    (var-set total-water-consumed (+ (var-get total-water-consumed) usage-amount))
    
    ;; Alert if leak detected
    (if leak-status
      (begin
        (print {event: "leak-detected", meter-id: meter-id, rate: hourly-rate})
        ERR_LEAK_THRESHOLD_EXCEEDED)
      (ok true))))

;; Process monthly billing and rewards
(define-public (process-monthly-billing (meter-id uint))
  (let ((meter (unwrap! (map-get? water-meters meter-id) ERR_METER_NOT_FOUND))
        (user-profile (unwrap! (map-get? user-profiles (get owner meter)) ERR_METER_NOT_FOUND))
        (penalty (calculate-penalty (get current-usage meter) (get monthly-quota meter)))
        (reward (if (>= (get conservation-score meter) u75) CONSERVATION_REWARD u0)))
    
    (asserts! (is-eq tx-sender CONTRACT_OWNER) ERR_NOT_AUTHORIZED)
    
    ;; Update user profile
    (map-set user-profiles (get owner meter)
      (merge user-profile {
        total-consumption: (+ (get total-consumption user-profile) (get current-usage meter)),
        conservation-points: (+ (get conservation-points user-profile) (get conservation-score meter)),
        penalties: (+ (get penalties user-profile) penalty),
        rewards-earned: (+ (get rewards-earned user-profile) reward)
      }))
    
    ;; Reset meter for new month
    (map-set water-meters meter-id
      (merge meter {
        current-usage: u0,
        conservation-score: u100
      }))
    
    ;; Update global rewards
    (var-set total-conservation-rewards (+ (var-get total-conservation-rewards) reward))
    
    (ok {penalty: penalty, reward: reward})))

;; Emergency leak shutoff
(define-public (emergency-shutoff (meter-id uint))
  (let ((meter (unwrap! (map-get? water-meters meter-id) ERR_METER_NOT_FOUND)))
    (asserts! (is-eq tx-sender CONTRACT_OWNER) ERR_NOT_AUTHORIZED)
    (asserts! (get leak-detected meter) (err u106))
    (print {event: "emergency-shutoff", meter-id: meter-id})
    (ok true)))

;; Update monthly quota
(define-public (update-quota (meter-id uint) (new-quota uint))
  (let ((meter (unwrap! (map-get? water-meters meter-id) ERR_METER_NOT_FOUND)))
    (asserts! (is-owner-or-authorized meter-id tx-sender) ERR_NOT_AUTHORIZED)
    (asserts! (> new-quota u0) ERR_INVALID_AMOUNT)
    (map-set water-meters meter-id
      (merge meter {monthly-quota: new-quota}))
    (ok true)))

;; Read-only Functions

;; Get meter information
(define-read-only (get-meter-info (meter-id uint))
  (map-get? water-meters meter-id))

;; Get user profile
(define-read-only (get-user-profile (user principal))
  (map-get? user-profiles user))

;; Get usage history
(define-read-only (get-usage-history (meter-id uint) (timestamp uint))
  (map-get? usage-history {meter-id: meter-id, timestamp: timestamp}))

;; Get conservation statistics
(define-read-only (get-conservation-stats)
  {
    total-water-consumed: (var-get total-water-consumed),
    total-conservation-rewards: (var-get total-conservation-rewards),
    active-meters: (var-get next-meter-id)
  })

;; Check if meter has leak
(define-read-only (has-leak (meter-id uint))
  (match (map-get? water-meters meter-id)
    meter (get leak-detected meter)
    false))

;; Get conservation ranking
(define-read-only (get-conservation-score (meter-id uint))
  (match (map-get? water-meters meter-id)
    meter (get conservation-score meter)
    u0))
