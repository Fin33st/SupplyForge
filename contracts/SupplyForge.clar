;; SupplyForge - A decentralized supply chain transparency platform
;; Enables end-to-end product tracking from manufacturer to consumer

;; Data storage
(define-map manufacturer-profiles principal {
  active: bool,
  certifications: (list 10 uint),
  trust-score: uint,
  last-activity: uint,
  shipment-count: uint
})

(define-map product-batches uint {
  producer: principal,
  units: uint,
  quality-rating: uint,
  active: bool,
  product-category: uint,
  total-handoffs: uint,
  manufactured-at: uint
})

(define-map tracking-records {tracker: principal, batch-id: uint} {
  timestamp: uint,
  verified: bool
})

(define-map product-categories uint (string-ascii 64))

;; Constants
(define-constant ERR_NOT_AUTHORIZED (err u100))
(define-constant ERR_INVALID_PARAMS (err u101))
(define-constant ERR_MANUFACTURER_NOT_FOUND (err u102))
(define-constant ERR_BATCH_NOT_FOUND (err u103))
(define-constant ERR_INSUFFICIENT_UNITS (err u104))
(define-constant ERR_ALREADY_REGISTERED (err u105))
(define-constant ERR_ALREADY_TRACKED (err u106))
(define-constant ERR_INVALID_PRINCIPAL (err u107))
(define-constant ERR_INVALID_VALUE (err u108))
(define-constant ERR_CATEGORY_NOT_FOUND (err u109))

(define-constant ZERO_ADDRESS 'SP000000000000000000002Q6VF78)
(define-constant MIN_QUALITY_RATING u1)
(define-constant MAX_QUALITY_RATING u1000)
(define-constant MIN_BATCH_UNITS u1000)
(define-constant MAX_CATEGORY_ID u1000)

;; Data variables
(define-data-var contract-owner principal tx-sender)
(define-data-var next-batch-id uint u1)
(define-data-var platform-fee-percent uint u5)
(define-data-var platform-balance uint u0)

;; Admin functions
(define-public (set-contract-owner (new-owner principal))
  (begin
    (asserts! (is-eq tx-sender (var-get contract-owner)) ERR_NOT_AUTHORIZED)
    (asserts! (not (is-eq new-owner ZERO_ADDRESS)) ERR_INVALID_PRINCIPAL)
    (ok (var-set contract-owner new-owner))))

(define-public (set-platform-fee (new-fee uint))
  (begin
    (asserts! (is-eq tx-sender (var-get contract-owner)) ERR_NOT_AUTHORIZED)
    (asserts! (<= new-fee u20) ERR_INVALID_PARAMS)
    (ok (var-set platform-fee-percent new-fee))))

(define-public (add-product-category (category-id uint) (category-name (string-ascii 64)))
  (begin
    (asserts! (is-eq tx-sender (var-get contract-owner)) ERR_NOT_AUTHORIZED)
    (asserts! (> (len category-name) u0) ERR_INVALID_PARAMS)
    (asserts! (< category-id MAX_CATEGORY_ID) ERR_INVALID_PARAMS)
    (asserts! (is-none (map-get? product-categories category-id)) ERR_ALREADY_REGISTERED)
    (ok (map-set product-categories category-id category-name))))

;; Manufacturer functions
(define-public (register-manufacturer (certifications (list 10 uint)))
  (begin
    (asserts! (is-none (map-get? manufacturer-profiles tx-sender)) ERR_ALREADY_REGISTERED)
    (asserts! (validate-certifications certifications) ERR_INVALID_PARAMS)
    (ok (map-set manufacturer-profiles tx-sender {
      active: true,
      certifications: certifications,
      trust-score: u0,
      last-activity: u0,
      shipment-count: u0
    }))))

(define-public (update-certifications (certifications (list 10 uint)))
  (let ((manufacturer-profile (unwrap! (map-get? manufacturer-profiles tx-sender) ERR_MANUFACTURER_NOT_FOUND)))
    (asserts! (validate-certifications certifications) ERR_INVALID_PARAMS)
    (ok (map-set manufacturer-profiles tx-sender (merge manufacturer-profile {certifications: certifications})))))

(define-public (deactivate-manufacturer)
  (let ((manufacturer-profile (unwrap! (map-get? manufacturer-profiles tx-sender) ERR_MANUFACTURER_NOT_FOUND)))
    (ok (map-set manufacturer-profiles tx-sender (merge manufacturer-profile {active: false})))))

(define-public (reactivate-manufacturer)
  (let ((manufacturer-profile (unwrap! (map-get? manufacturer-profiles tx-sender) ERR_MANUFACTURER_NOT_FOUND)))
    (ok (map-set manufacturer-profiles tx-sender (merge manufacturer-profile {active: true})))))

;; Product batch functions
(define-public (create-batch (units uint) (quality-rating uint) (product-category uint) (stx-amount uint))
  (begin
    (asserts! (>= units MIN_BATCH_UNITS) ERR_INVALID_PARAMS)
    (asserts! (and (>= quality-rating MIN_QUALITY_RATING) (<= quality-rating MAX_QUALITY_RATING)) ERR_INVALID_PARAMS)
    (asserts! (is-some (map-get? product-categories product-category)) ERR_CATEGORY_NOT_FOUND)
    (asserts! (>= stx-amount units) ERR_INSUFFICIENT_UNITS)

    (try! (stx-transfer? stx-amount tx-sender (as-contract tx-sender)))

    (let ((batch-id (var-get next-batch-id)))
      (map-set product-batches batch-id {
        producer: tx-sender,
        units: units,
        quality-rating: quality-rating,
        active: true,
        product-category: product-category,
        total-handoffs: u0,
        manufactured-at: u0
      })

      (var-set next-batch-id (+ batch-id u1))
      (ok batch-id))))

(define-public (recall-batch (batch-id uint))
  (let ((batch (unwrap! (map-get? product-batches batch-id) ERR_BATCH_NOT_FOUND)))
    (asserts! (is-eq tx-sender (get producer batch)) ERR_NOT_AUTHORIZED)
    (ok (map-set product-batches batch-id (merge batch {active: false})))))

(define-public (reactivate-batch (batch-id uint))
  (let ((batch (unwrap! (map-get? product-batches batch-id) ERR_BATCH_NOT_FOUND)))
    (asserts! (is-eq tx-sender (get producer batch)) ERR_NOT_AUTHORIZED)
    (ok (map-set product-batches batch-id (merge batch {active: true})))))

(define-public (add-batch-units (batch-id uint) (additional-units uint))
  (let ((batch (unwrap! (map-get? product-batches batch-id) ERR_BATCH_NOT_FOUND)))
    (asserts! (is-eq tx-sender (get producer batch)) ERR_NOT_AUTHORIZED)
    (asserts! (> additional-units u0) ERR_INVALID_PARAMS)

    (try! (stx-transfer? additional-units tx-sender (as-contract tx-sender)))

    (ok (map-set product-batches batch-id
      (merge batch {units: (+ (get units batch) additional-units)})))))

;; Helper function to check certification match
(define-private (check-certification-match (product-category uint) (certifications (list 10 uint)))
  (or
    (and (> (len certifications) u0) (is-eq product-category (unwrap-panic (element-at certifications u0))))
    (and (> (len certifications) u1) (is-eq product-category (unwrap-panic (element-at certifications u1))))
    (and (> (len certifications) u2) (is-eq product-category (unwrap-panic (element-at certifications u2))))
    (and (> (len certifications) u3) (is-eq product-category (unwrap-panic (element-at certifications u3))))
    (and (> (len certifications) u4) (is-eq product-category (unwrap-panic (element-at certifications u4))))
    (and (> (len certifications) u5) (is-eq product-category (unwrap-panic (element-at certifications u5))))
    (and (> (len certifications) u6) (is-eq product-category (unwrap-panic (element-at certifications u6))))
    (and (> (len certifications) u7) (is-eq product-category (unwrap-panic (element-at certifications u7))))
    (and (> (len certifications) u8) (is-eq product-category (unwrap-panic (element-at certifications u8))))
    (and (> (len certifications) u9) (is-eq product-category (unwrap-panic (element-at certifications u9))))
  ))

;; Tracking functions
(define-public (track-batch (batch-id uint))
  (let (
    (manufacturer-profile (unwrap! (map-get? manufacturer-profiles tx-sender) ERR_MANUFACTURER_NOT_FOUND))
    (batch (unwrap! (map-get? product-batches batch-id) ERR_BATCH_NOT_FOUND))
    (tracking-key {tracker: tx-sender, batch-id: batch-id})
  )
    (asserts! (get active manufacturer-profile) ERR_MANUFACTURER_NOT_FOUND)
    (asserts! (get active batch) ERR_BATCH_NOT_FOUND)
    (asserts! (is-none (map-get? tracking-records tracking-key)) ERR_ALREADY_TRACKED)
    (asserts! (>= (get units batch) (get quality-rating batch)) ERR_INSUFFICIENT_UNITS)
    (asserts! (check-certification-match (get product-category batch) (get certifications manufacturer-profile)) ERR_INVALID_PARAMS)

    (let (
      (quality-rating (get quality-rating batch))
      (platform-fee (/ (* quality-rating (var-get platform-fee-percent)) u100))
      (manufacturer-trust (- quality-rating platform-fee))
    )
      (map-set tracking-records tracking-key {timestamp: u0, verified: true})

      (map-set product-batches batch-id (merge batch {
        units: (- (get units batch) quality-rating),
        total-handoffs: (+ (get total-handoffs batch) u1)
      }))

      (map-set manufacturer-profiles tx-sender (merge manufacturer-profile {
        trust-score: (+ (get trust-score manufacturer-profile) manufacturer-trust),
        shipment-count: (+ (get shipment-count manufacturer-profile) u1)
      }))

      (var-set platform-balance (+ (var-get platform-balance) platform-fee))

      (ok manufacturer-trust))))

(define-public (claim-trust-score)
  (let ((manufacturer-profile (unwrap! (map-get? manufacturer-profiles tx-sender) ERR_MANUFACTURER_NOT_FOUND)))
    (let ((trust-score (get trust-score manufacturer-profile)))
      (asserts! (> trust-score u0) ERR_INSUFFICIENT_UNITS)

      (try! (as-contract (stx-transfer? trust-score tx-sender tx-sender)))

      (map-set manufacturer-profiles tx-sender (merge manufacturer-profile {
        trust-score: u0,
        last-activity: u0
      }))

      (ok trust-score))))

(define-public (withdraw-platform-fees)
  (begin
    (asserts! (is-eq tx-sender (var-get contract-owner)) ERR_NOT_AUTHORIZED)
    (let ((amount (var-get platform-balance)))
      (asserts! (> amount u0) ERR_INSUFFICIENT_UNITS)

      (try! (as-contract (stx-transfer? amount tx-sender (var-get contract-owner))))

      (var-set platform-balance u0)

      (ok amount))))

;; Helper functions
(define-private (is-valid-product-category (product-category uint))
  (is-some (map-get? product-categories product-category)))

(define-private (count-valid-product-categories (certifications (list 10 uint)))
  (+
    (if (and (> (len certifications) u0) (is-valid-product-category (unwrap-panic (element-at certifications u0)))) u1 u0)
    (if (and (> (len certifications) u1) (is-valid-product-category (unwrap-panic (element-at certifications u1)))) u1 u0)
    (if (and (> (len certifications) u2) (is-valid-product-category (unwrap-panic (element-at certifications u2)))) u1 u0)
    (if (and (> (len certifications) u3) (is-valid-product-category (unwrap-panic (element-at certifications u3)))) u1 u0)
    (if (and (> (len certifications) u4) (is-valid-product-category (unwrap-panic (element-at certifications u4)))) u1 u0)
    (if (and (> (len certifications) u5) (is-valid-product-category (unwrap-panic (element-at certifications u5)))) u1 u0)
    (if (and (> (len certifications) u6) (is-valid-product-category (unwrap-panic (element-at certifications u6)))) u1 u0)
    (if (and (> (len certifications) u7) (is-valid-product-category (unwrap-panic (element-at certifications u7)))) u1 u0)
    (if (and (> (len certifications) u8) (is-valid-product-category (unwrap-panic (element-at certifications u8)))) u1 u0)
    (if (and (> (len certifications) u9) (is-valid-product-category (unwrap-panic (element-at certifications u9)))) u1 u0)
  ))

(define-private (validate-certifications (certifications (list 10 uint)))
  (let ((certs-len (len certifications)))
    (and
      (> certs-len u0)
      (<= certs-len u10)
      (is-eq certs-len (count-valid-product-categories certifications)))))

;; Read-only functions
(define-read-only (get-manufacturer-profile (manufacturer principal))
  (map-get? manufacturer-profiles manufacturer))

(define-read-only (get-batch (batch-id uint))
  (map-get? product-batches batch-id))

(define-read-only (get-product-category (category-id uint))
  (map-get? product-categories category-id))

(define-read-only (get-platform-fee)
  (var-get platform-fee-percent))

(define-read-only (get-platform-balance)
  (var-get platform-balance))

(define-read-only (get-tracking-record (tracker principal) (batch-id uint))
  (map-get? tracking-records {tracker: tracker, batch-id: batch-id}))