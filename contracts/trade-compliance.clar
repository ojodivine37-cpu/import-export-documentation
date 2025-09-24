;; Import/Export Trade Compliance Contract
;; Trade compliance platform with customs documentation and duty calculation

;; Constants
(define-constant CONTRACT-OWNER tx-sender)
(define-constant ERR-UNAUTHORIZED (err u100))
(define-constant ERR-SHIPMENT-NOT-FOUND (err u101))
(define-constant ERR-DOCUMENT-NOT-FOUND (err u102))
(define-constant ERR-INVALID-VALUE (err u103))
(define-constant ERR-FILING-ERROR (err u104))

;; Data structures
(define-map shipments
  { shipment-id: uint }
  {
    origin-country: (string-ascii 32),
    destination-country: (string-ascii 32),
    goods-description: (string-ascii 128),
    declared-value: uint,
    weight: uint,
    customs-status: (string-ascii 16),
    duty-paid: bool,
    shipper: principal
  }
)

(define-map customs-documents
  { document-id: uint }
  {
    shipment-id: uint,
    document-type: (string-ascii 32),
    document-number: (string-ascii 64),
    issue-date: uint,
    expiry-date: uint,
    status: (string-ascii 16),
    issuing-authority: (string-ascii 64)
  }
)

(define-map duty-calculations
  { shipment-id: uint }
  {
    base-value: uint,
    tariff-rate: uint,
    calculated-duty: uint,
    additional-fees: uint,
    total-amount: uint,
    calculation-date: uint
  }
)

(define-map regulatory-filings
  { filing-id: uint }
  {
    shipment-id: uint,
    filing-type: (string-ascii 32),
    submission-date: uint,
    approval-date: uint,
    reference-number: (string-ascii 32),
    status: (string-ascii 16),
    filed-by: principal
  }
)

(define-data-var next-shipment-id uint u1)
(define-data-var next-document-id uint u1)
(define-data-var next-filing-id uint u1)

;; Register new shipment
(define-public (register-shipment (origin (string-ascii 32)) (destination (string-ascii 32)) (goods (string-ascii 128)) (value uint) (weight uint))
  (let ((shipment-id (var-get next-shipment-id)))
    (asserts! (> value u0) ERR-INVALID-VALUE)
    (asserts! (> weight u0) ERR-INVALID-VALUE)
    (map-set shipments
      { shipment-id: shipment-id }
      {
        origin-country: origin,
        destination-country: destination,
        goods-description: goods,
        declared-value: value,
        weight: weight,
        customs-status: "pending",
        duty-paid: false,
        shipper: tx-sender
      }
    )
    (var-set next-shipment-id (+ shipment-id u1))
    (ok shipment-id)
  )
)

;; Create customs document
(define-public (create-document (shipment-id uint) (doc-type (string-ascii 32)) (doc-number (string-ascii 64)) (issue-date uint) (expiry-date uint) (authority (string-ascii 64)))
  (let 
    (
      (shipment (unwrap! (map-get? shipments { shipment-id: shipment-id }) ERR-SHIPMENT-NOT-FOUND))
      (document-id (var-get next-document-id))
    )
    (asserts! (is-eq (get shipper shipment) tx-sender) ERR-UNAUTHORIZED)
    (map-set customs-documents
      { document-id: document-id }
      {
        shipment-id: shipment-id,
        document-type: doc-type,
        document-number: doc-number,
        issue-date: issue-date,
        expiry-date: expiry-date,
        status: "active",
        issuing-authority: authority
      }
    )
    (var-set next-document-id (+ document-id u1))
    (ok document-id)
  )
)

;; Calculate duties and fees
(define-public (calculate-duty (shipment-id uint) (tariff-rate uint) (additional-fees uint))
  (let ((shipment (unwrap! (map-get? shipments { shipment-id: shipment-id }) ERR-SHIPMENT-NOT-FOUND)))
    (asserts! (is-eq (get shipper shipment) tx-sender) ERR-UNAUTHORIZED)
    (let 
      (
        (base-value (get declared-value shipment))
        (calculated-duty (* base-value (/ tariff-rate u100)))
        (total-amount (+ calculated-duty additional-fees))
      )
      (map-set duty-calculations
        { shipment-id: shipment-id }
        {
          base-value: base-value,
          tariff-rate: tariff-rate,
          calculated-duty: calculated-duty,
          additional-fees: additional-fees,
          total-amount: total-amount,
          calculation-date: u0
        }
      )
      (ok total-amount)
    )
  )
)

;; Submit regulatory filing
(define-public (submit-filing (shipment-id uint) (filing-type (string-ascii 32)) (reference (string-ascii 32)))
  (let 
    (
      (shipment (unwrap! (map-get? shipments { shipment-id: shipment-id }) ERR-SHIPMENT-NOT-FOUND))
      (filing-id (var-get next-filing-id))
    )
    (asserts! (is-eq (get shipper shipment) tx-sender) ERR-UNAUTHORIZED)
    (map-set regulatory-filings
      { filing-id: filing-id }
      {
        shipment-id: shipment-id,
        filing-type: filing-type,
        submission-date: u0,
        approval-date: u0,
        reference-number: reference,
        status: "submitted",
        filed-by: tx-sender
      }
    )
    (var-set next-filing-id (+ filing-id u1))
    (ok filing-id)
  )
)

;; Update customs status
(define-public (update-customs-status (shipment-id uint) (new-status (string-ascii 16)))
  (let ((shipment (unwrap! (map-get? shipments { shipment-id: shipment-id }) ERR-SHIPMENT-NOT-FOUND)))
    (asserts! (is-eq (get shipper shipment) tx-sender) ERR-UNAUTHORIZED)
    (map-set shipments
      { shipment-id: shipment-id }
      (merge shipment { customs-status: new-status })
    )
    (ok true)
  )
)

;; Mark duty as paid
(define-public (mark-duty-paid (shipment-id uint))
  (let ((shipment (unwrap! (map-get? shipments { shipment-id: shipment-id }) ERR-SHIPMENT-NOT-FOUND)))
    (asserts! (is-eq (get shipper shipment) tx-sender) ERR-UNAUTHORIZED)
    (map-set shipments
      { shipment-id: shipment-id }
      (merge shipment { duty-paid: true, customs-status: "cleared" })
    )
    (ok true)
  )
)

;; Get shipment information
(define-read-only (get-shipment-info (shipment-id uint))
  (map-get? shipments { shipment-id: shipment-id })
)

;; Get customs document
(define-read-only (get-document (document-id uint))
  (map-get? customs-documents { document-id: document-id })
)

;; Get duty calculation
(define-read-only (get-duty-calculation (shipment-id uint))
  (map-get? duty-calculations { shipment-id: shipment-id })
)

;; Get filing status
(define-read-only (get-filing-status (filing-id uint))
  (map-get? regulatory-filings { filing-id: filing-id })
)
