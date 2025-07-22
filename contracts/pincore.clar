;; =============================================================================
;; CONSTANTS
;; =============================================================================

(define-constant DOMAIN_SUFFIX ".pincore")
(define-constant DOMAIN_REGISTRATION_PERIOD u525600) ;; ~1 year in blocks (assuming 10min blocks)
(define-constant GRACE_PERIOD u10080) ;; ~1 week grace period after expiry
(define-constant BASE_REGISTRATION_FEE u500000) ;; 0.5 STX in micro-STX
(define-constant MIN_DOMAIN_LENGTH u3)
(define-constant MAX_DOMAIN_LENGTH u64)
(define-constant MAX_METADATA_LENGTH u256)

;; New constants for dynamic pricing and marketplace
(define-constant PREMIUM_MULTIPLIER_3_CHAR u10) ;; 10x for 3-char domains
(define-constant PREMIUM_MULTIPLIER_4_CHAR u5)  ;; 5x for 4-char domains
(define-constant DEMAND_PRICE_INCREMENT u50000) ;; +0.05 STX per registration attempt
(define-constant MARKETPLACE_FEE_PERCENT u250) ;; 2.5% marketplace fee
(define-constant AUCTION_EXTENSION_BLOCKS u144) ;; ~1 day extension if bid in last hour
(define-constant MIN_AUCTION_DURATION u1440) ;; ~10 days minimum auction
(define-constant MAX_AUCTION_DURATION u14400) ;; ~100 days maximum auction

;; Error codes
(define-constant ERR_DOMAIN_UNAVAILABLE u100)
(define-constant ERR_DOMAIN_TOO_SHORT u101)
(define-constant ERR_DOMAIN_TOO_LONG u102)
(define-constant ERR_INVALID_METADATA u103)
(define-constant ERR_UNAUTHORIZED u200)
(define-constant ERR_INSUFFICIENT_FUNDS u201)
(define-constant ERR_DOMAIN_NOT_FOUND u404)
(define-constant ERR_SUBDOMAIN_NOT_FOUND u405)
(define-constant ERR_DOMAIN_EXPIRED u406)

;; New error codes for marketplace and pricing
(define-constant ERR_NOT_FOR_SALE u500)
(define-constant ERR_INVALID_PRICE u501)
(define-constant ERR_AUCTION_NOT_FOUND u502)
(define-constant ERR_AUCTION_ENDED u503)
(define-constant ERR_AUCTION_ACTIVE u504)
(define-constant ERR_BID_TOO_LOW u505)
(define-constant ERR_INVALID_AUCTION_DURATION u506)
(define-constant ERR_CANNOT_BID_OWN_AUCTION u507)

;; =============================================================================
;; DATA VARIABLES
;; =============================================================================

(define-data-var domain-counter uint u0)
(define-data-var contract-owner principal tx-sender)
(define-data-var marketplace-enabled bool true)
(define-data-var auction-counter uint u0)

;; =============================================================================
;; DATA MAPS
;; =============================================================================

(define-map domains
  { name: (buff 64) }
  {
    owner: principal,
    expires-at: uint,
    metadata: (buff 256),
    created-at: uint
  }
)

(define-map subdomains
  { parent: (buff 64), label: (buff 64) }
  {
    owner: principal,
    metadata: (buff 256),
    created-at: uint
  }
)

;; Track domain history for analytics
(define-map domain-history
  { name: (buff 64), block: uint }
  {
    action: (string-ascii 20),
    actor: principal
  }
)

;; Dynamic pricing maps
(define-map domain-pricing
  { length: uint }
  { base-price: uint, multiplier: uint }
)

(define-map demand-tracker
  { name: (buff 64) }
  { attempts: uint, last-attempt: uint }
)

;; Marketplace maps
(define-map domain-listings
  { name: (buff 64) }
  {
    seller: principal,
    price: uint,
    listed-at: uint
  }
)

(define-map domain-auctions
  { auction-id: uint }
  {
    domain: (buff 64),
    seller: principal,
    starting-price: uint,
    current-bid: uint,
    current-bidder: (optional principal),
    end-block: uint,
    created-at: uint
  }
)

(define-map auction-bids
  { auction-id: uint, bidder: principal }
  { amount: uint, block: uint }
)

;; =============================================================================
;; PRIVATE FUNCTIONS
;; =============================================================================

(define-private (is-valid-domain-name (name (buff 64)))
  (and
    (>= (len name) MIN_DOMAIN_LENGTH)
    (<= (len name) MAX_DOMAIN_LENGTH)
  )
)

(define-private (is-domain-expired (expires-at uint))
  (> stacks-block-height expires-at)
)

(define-private (is-in-grace-period (expires-at uint))
  (and
    (is-domain-expired expires-at)
    (<= stacks-block-height (+ expires-at GRACE_PERIOD))
  )
)

(define-private (log-domain-action (name (buff 64)) (action (string-ascii 20)))
  (map-set domain-history
    { name: name, block: stacks-block-height }
    { action: action, actor: tx-sender }
  )
)

;; Helper function to validate parent domain
(define-private (validate-parent-domain (parent (buff 64)))
  (match (map-get? domains { name: parent })
    parent-domain
    (if (is-domain-expired (get expires-at parent-domain))
      (err ERR_DOMAIN_EXPIRED)
      (ok parent-domain)
    )
    (err ERR_DOMAIN_NOT_FOUND)
  )
)

;; Dynamic pricing calculation
(define-private (calculate-registration-fee (name (buff 64)))
  (let (
    (domain-length (len name))
    (demand-info (default-to 
      { attempts: u0, last-attempt: u0 }
      (map-get? demand-tracker { name: name })
    ))
    (base-fee BASE_REGISTRATION_FEE)
  )
    ;; Apply premium pricing for short domains
    (let (
      (premium-fee (if (<= domain-length u3)
        (* base-fee PREMIUM_MULTIPLIER_3_CHAR)
        (if (<= domain-length u4)
          (* base-fee PREMIUM_MULTIPLIER_4_CHAR)
          base-fee
        )
      ))
    )
      ;; Add demand-based pricing
      (+ premium-fee (* (get attempts demand-info) DEMAND_PRICE_INCREMENT))
    )
  )
)

(define-private (update-demand-tracker (name (buff 64)))
  (let (
    (current-demand (default-to 
      { attempts: u0, last-attempt: u0 }
      (map-get? demand-tracker { name: name })
    ))
  )
    (map-set demand-tracker { name: name } {
      attempts: (+ (get attempts current-demand) u1),
      last-attempt: stacks-block-height
    })
  )
)

(define-private (calculate-marketplace-fee (price uint))
  (/ (* price MARKETPLACE_FEE_PERCENT) u10000)
)

(define-private (transfer-with-fee (amount uint) (recipient principal))
  (let (
    (fee (calculate-marketplace-fee amount))
    (seller-amount (- amount fee))
  )
    (try! (stx-transfer? seller-amount tx-sender recipient))
    (try! (stx-transfer? fee tx-sender (as-contract tx-sender)))
    (ok seller-amount)
  )
)

;; =============================================================================
;; READ-ONLY FUNCTIONS
;; =============================================================================

(define-read-only (is-available (name (buff 64)))
  (let ((domain-info (map-get? domains { name: name })))
    (match domain-info
      domain-data
      (let ((expires-at (get expires-at domain-data)))
        ;; Domain is available if it has expired and grace period has passed
        (> stacks-block-height (+ expires-at GRACE_PERIOD))
      )
      ;; Domain doesn't exist, so it's available
      true
    )
  )
)

(define-read-only (get-domain (name (buff 64)))
  (map-get? domains { name: name })
)

(define-read-only (get-subdomain (parent (buff 64)) (label (buff 64)))
  (map-get? subdomains { parent: parent, label: label })
)

(define-read-only (resolve (name (buff 64)))
  (let ((domain-info (map-get? domains { name: name })))
    (match domain-info
      domain-data
      (let ((expires-at (get expires-at domain-data)))
        (if (is-domain-expired expires-at)
          (err ERR_DOMAIN_EXPIRED)
          (ok {
            owner: (get owner domain-data),
            metadata: (get metadata domain-data),
            expires-at: expires-at,
            created-at: (get created-at domain-data)
          })
        )
      )
      (err ERR_DOMAIN_NOT_FOUND)
    )
  )
)

(define-read-only (resolve-subdomain (parent (buff 64)) (label (buff 64)))
  (match (validate-parent-domain parent)
    parent-domain
    (let ((subdomain-info (get-subdomain parent label)))
      (match subdomain-info
        subdomain-data
        (ok {
          owner: (get owner subdomain-data),
          metadata: (get metadata subdomain-data),
          created-at: (get created-at subdomain-data),
          parent-owner: (get owner parent-domain)
        })
        (err ERR_SUBDOMAIN_NOT_FOUND)
      )
    )
    error-code (err error-code)
  )
)

(define-read-only (get-domain-count)
  (var-get domain-counter)
)

(define-read-only (get-contract-owner)
  (var-get contract-owner)
)

(define-read-only (whoami)
  tx-sender
)

;; New read-only functions for pricing and marketplace
(define-read-only (get-registration-price (name (buff 64)))
  (calculate-registration-fee name)
)

(define-read-only (get-demand-info (name (buff 64)))
  (map-get? demand-tracker { name: name })
)

(define-read-only (get-domain-listing (name (buff 64)))
  (map-get? domain-listings { name: name })
)

(define-read-only (get-auction (auction-id uint))
  (map-get? domain-auctions { auction-id: auction-id })
)

(define-read-only (get-auction-bid (auction-id uint) (bidder principal))
  (map-get? auction-bids { auction-id: auction-id, bidder: bidder })
)

(define-read-only (is-marketplace-enabled)
  (var-get marketplace-enabled)
)

;; =============================================================================
;; PUBLIC FUNCTIONS
;; =============================================================================

(define-public (register (name (buff 64)) (metadata (buff 256)))
  (begin
    ;; Validate inputs
    (asserts! (is-available name) (err ERR_DOMAIN_UNAVAILABLE))
    (asserts! (is-valid-domain-name name) (err ERR_DOMAIN_TOO_SHORT))
    (asserts! (<= (len metadata) MAX_METADATA_LENGTH) (err ERR_INVALID_METADATA))
    
    ;; Calculate dynamic price and update demand tracker
    (let ((registration-fee (calculate-registration-fee name)))
      (update-demand-tracker name)
      
      ;; Process payment
      (try! (stx-transfer? registration-fee tx-sender (as-contract tx-sender)))
      
      ;; Register domain
      (let ((expiry (+ stacks-block-height DOMAIN_REGISTRATION_PERIOD)))
        (map-set domains { name: name } {
          owner: tx-sender,
          expires-at: expiry,
          metadata: metadata,
          created-at: stacks-block-height
        })
        
        ;; Update counter and log action
        (var-set domain-counter (+ (var-get domain-counter) u1))
        (log-domain-action name "register")
        
        (ok {
          domain: name,
          owner: tx-sender,
          expires-at: expiry,
          price-paid: registration-fee
        })
      )
    )
  )
)

(define-public (renew (name (buff 64)))
  (let ((domain-info (get-domain name)))
    (match domain-info domain-data
      (let (
        (current-owner (get owner domain-data))
        (current-expiry (get expires-at domain-data))
      )
        ;; Only owner can renew, and only during grace period if expired
        (asserts! (is-eq tx-sender current-owner) (err ERR_UNAUTHORIZED))
        (asserts! 
          (or 
            (not (is-domain-expired current-expiry))
            (is-in-grace-period current-expiry)
          ) 
          (err ERR_DOMAIN_EXPIRED)
        )
        
        ;; Process payment (use base fee for renewals)
        (try! (stx-transfer? BASE_REGISTRATION_FEE tx-sender (as-contract tx-sender)))
        
        ;; Extend expiration
        (let (
          (new-expiry (+ current-expiry DOMAIN_REGISTRATION_PERIOD))
          (owner (get owner domain-data))
          (metadata (get metadata domain-data))
          (created-at (get created-at domain-data))
        )
          (map-set domains { name: name } {
            owner: owner,
            expires-at: new-expiry,
            metadata: metadata,
            created-at: created-at
          })
          (log-domain-action name "renew")
          
          (ok {
            domain: name,
            new-expires-at: new-expiry
          })
        )
      )
      (err ERR_DOMAIN_NOT_FOUND)
    )
  )
)

(define-public (transfer (name (buff 64)) (new-owner principal))
  (let ((domain-info (get-domain name)))
    (match domain-info
      domain-data
      (let ((expires-at (get expires-at domain-data)))
        (asserts! (is-eq tx-sender (get owner domain-data)) (err ERR_UNAUTHORIZED))
        (asserts! (not (is-domain-expired expires-at)) (err ERR_DOMAIN_EXPIRED))
        
        ;; Remove from marketplace if listed
        (map-delete domain-listings { name: name })
        
        (map-set domains { name: name } (merge domain-data { owner: new-owner }))
        (log-domain-action name "transfer")
        
        (ok {
          domain: name,
          old-owner: tx-sender,
          new-owner: new-owner
        })
      )
      (err ERR_DOMAIN_NOT_FOUND)
    )
  )
)

(define-public (set-metadata (name (buff 64)) (new-metadata (buff 256)))
  (let ((domain-info (get-domain name)))
    (match domain-info
      domain-data
      (let ((expires-at (get expires-at domain-data)))
        (asserts! (is-eq tx-sender (get owner domain-data)) (err ERR_UNAUTHORIZED))
        (asserts! (not (is-domain-expired expires-at)) (err ERR_DOMAIN_EXPIRED))
        (asserts! (<= (len new-metadata) MAX_METADATA_LENGTH) (err ERR_INVALID_METADATA))
        
        (map-set domains { name: name } (merge domain-data { metadata: new-metadata }))
        (log-domain-action name "update-metadata")
        
        (ok true)
      )
      (err ERR_DOMAIN_NOT_FOUND)
    )
  )
)

(define-public (create-subdomain (parent (buff 64)) (label (buff 64)) (metadata (buff 256)))
  (match (validate-parent-domain parent)
    parent-domain
    (begin
      (asserts! (is-eq tx-sender (get owner parent-domain)) (err ERR_UNAUTHORIZED))
      (asserts! (is-valid-domain-name label) (err ERR_DOMAIN_TOO_SHORT))
      (asserts! (<= (len metadata) MAX_METADATA_LENGTH) (err ERR_INVALID_METADATA))
      
      (map-set subdomains { parent: parent, label: label } {
        owner: tx-sender,
        metadata: metadata,
        created-at: stacks-block-height
      })
      
      (ok {
        parent: parent,
        label: label,
        owner: tx-sender
      })
    )
    error-code (err error-code)
  )
)

(define-public (transfer-subdomain (parent (buff 64)) (label (buff 64)) (new-owner principal))
  (let ((subdomain-info (get-subdomain parent label)))
    (match subdomain-info
      subdomain-data
      (begin
        (asserts! (is-eq tx-sender (get owner subdomain-data)) (err ERR_UNAUTHORIZED))
        
        ;; Verify parent domain is still valid
        (match (validate-parent-domain parent)
          parent-domain
          (begin
            (map-set subdomains { parent: parent, label: label } 
              (merge subdomain-data { owner: new-owner }))
            
            (ok {
              parent: parent,
              label: label,
              old-owner: tx-sender,
              new-owner: new-owner
            })
          )
          error-code (err error-code)
        )
      )
      (err ERR_SUBDOMAIN_NOT_FOUND)
    )
  )
)

(define-public (set-subdomain-metadata (parent (buff 64)) (label (buff 64)) (new-metadata (buff 256)))
  (let ((subdomain-info (get-subdomain parent label)))
    (match subdomain-info
      subdomain-data
      (begin
        (asserts! (is-eq tx-sender (get owner subdomain-data)) (err ERR_UNAUTHORIZED))
        (asserts! (<= (len new-metadata) MAX_METADATA_LENGTH) (err ERR_INVALID_METADATA))
        
        ;; Verify parent domain is still valid
        (match (validate-parent-domain parent)
          parent-domain
          (begin
            (map-set subdomains { parent: parent, label: label } 
              (merge subdomain-data { metadata: new-metadata }))
            (ok true)
          )
          error-code (err error-code)
        )
      )
      (err ERR_SUBDOMAIN_NOT_FOUND)
    )
  )
)

;; =============================================================================
;; MARKETPLACE FUNCTIONS
;; =============================================================================

(define-public (list-domain (name (buff 64)) (price uint))
  (let ((domain-info (get-domain name)))
    (match domain-info
      domain-data
      (begin
        (asserts! (var-get marketplace-enabled) (err ERR_UNAUTHORIZED))
        (asserts! (is-eq tx-sender (get owner domain-data)) (err ERR_UNAUTHORIZED))
        (asserts! (not (is-domain-expired (get expires-at domain-data))) (err ERR_DOMAIN_EXPIRED))
        (asserts! (> price u0) (err ERR_INVALID_PRICE))
        
        (map-set domain-listings { name: name } {
          seller: tx-sender,
          price: price,
          listed-at: stacks-block-height
        })
        
        (log-domain-action name "list")
        (ok true)
      )
      (err ERR_DOMAIN_NOT_FOUND)
    )
  )
)

(define-public (unlist-domain (name (buff 64)))
  (let ((listing-info (get-domain-listing name)))
    (match listing-info
      listing-data
      (begin
        (asserts! (is-eq tx-sender (get seller listing-data)) (err ERR_UNAUTHORIZED))
        (map-delete domain-listings { name: name })
        (log-domain-action name "unlist")
        (ok true)
      )
      (err ERR_NOT_FOR_SALE)
    )
  )
)

(define-public (buy-domain (name (buff 64)))
  (let (
    (domain-info (get-domain name))
    (listing-info (get-domain-listing name))
  )
    (match domain-info
      domain-data
      (match listing-info
        listing-data
        (let (
          (seller (get seller listing-data))
          (price (get price listing-data))
        )
          (asserts! (not (is-eq tx-sender seller)) (err ERR_UNAUTHORIZED))
          (asserts! (not (is-domain-expired (get expires-at domain-data))) (err ERR_DOMAIN_EXPIRED))
          
          ;; Transfer payment with marketplace fee
          (try! (transfer-with-fee price seller))
          
          ;; Transfer domain ownership
          (map-set domains { name: name } (merge domain-data { owner: tx-sender }))
          (map-delete domain-listings { name: name })
          
          (log-domain-action name "purchase")
          
          (ok {
            domain: name,
            buyer: tx-sender,
            seller: seller,
            price: price
          })
        )
        (err ERR_NOT_FOR_SALE)
      )
      (err ERR_DOMAIN_NOT_FOUND)
    )
  )
)

;; =============================================================================
;; AUCTION FUNCTIONS
;; =============================================================================

(define-public (create-auction (name (buff 64)) (starting-price uint) (duration uint))
  (let ((domain-info (get-domain name)))
    (match domain-info
      domain-data
      (begin
        (asserts! (var-get marketplace-enabled) (err ERR_UNAUTHORIZED))
        (asserts! (is-eq tx-sender (get owner domain-data)) (err ERR_UNAUTHORIZED))
        (asserts! (not (is-domain-expired (get expires-at domain-data))) (err ERR_DOMAIN_EXPIRED))
        (asserts! (> starting-price u0) (err ERR_INVALID_PRICE))
        (asserts! (and (>= duration MIN_AUCTION_DURATION) (<= duration MAX_AUCTION_DURATION)) (err ERR_INVALID_AUCTION_DURATION))
        
        ;; Remove from direct sale listing if exists
        (map-delete domain-listings { name: name })
        
        (let ((auction-id (+ (var-get auction-counter) u1)))
          (map-set domain-auctions { auction-id: auction-id } {
            domain: name,
            seller: tx-sender,
            starting-price: starting-price,
            current-bid: u0,
            current-bidder: none,
            end-block: (+ stacks-block-height duration),
            created-at: stacks-block-height
          })
          
          (var-set auction-counter auction-id)
          (log-domain-action name "auction-create")
          
          (ok auction-id)
        )
      )
      (err ERR_DOMAIN_NOT_FOUND)
    )
  )
)

(define-public (place-bid (auction-id uint) (bid-amount uint))
  (let ((auction-info (get-auction auction-id)))
    (match auction-info
      auction-data
      (let (
        (current-bid (get current-bid auction-data))
        (end-block (get end-block auction-data))
        (seller (get seller auction-data))
      )
        (asserts! (not (is-eq tx-sender seller)) (err ERR_CANNOT_BID_OWN_AUCTION))
        (asserts! (<= stacks-block-height end-block) (err ERR_AUCTION_ENDED))
        (asserts! (> bid-amount (if (> current-bid u0) current-bid (get starting-price auction-data))) (err ERR_BID_TOO_LOW))
        
        ;; Refund previous bidder if exists
        (match (get current-bidder auction-data)
          previous-bidder
          (try! (as-contract (stx-transfer? current-bid tx-sender previous-bidder)))
          true ;; No previous bidder
        )
        
        ;; Escrow new bid
        (try! (stx-transfer? bid-amount tx-sender (as-contract tx-sender)))
        
        ;; Extend auction if bid placed in last hour
        (let (
          (new-end-block (if (<= (- end-block stacks-block-height) u6) ;; ~1 hour
            (+ end-block AUCTION_EXTENSION_BLOCKS)
            end-block
          ))
        )
          ;; Update auction
          (map-set domain-auctions { auction-id: auction-id } 
            (merge auction-data {
              current-bid: bid-amount,
              current-bidder: (some tx-sender),
              end-block: new-end-block
            })
          )
          
          ;; Record bid
          (map-set auction-bids { auction-id: auction-id, bidder: tx-sender } {
            amount: bid-amount,
            block: stacks-block-height
          })
          
          (ok {
            auction-id: auction-id,
            bidder: tx-sender,
            amount: bid-amount,
            new-end-block: new-end-block
          })
        )
      )
      (err ERR_AUCTION_NOT_FOUND)
    )
  )
)

(define-public (finalize-auction (auction-id uint))
  (let ((auction-info (get-auction auction-id)))
    (match auction-info
      auction-data
      (let (
        (domain-name (get domain auction-data))
        (seller (get seller auction-data))
        (current-bid (get current-bid auction-data))
        (end-block (get end-block auction-data))
      )
        (asserts! (> stacks-block-height end-block) (err ERR_AUCTION_ACTIVE))
        
        (if (> current-bid u0)
          ;; Auction had bids - transfer domain and payment
          (match (get current-bidder auction-data)
            winner
            (let ((domain-info (unwrap! (get-domain domain-name) (err ERR_DOMAIN_NOT_FOUND))))
              ;; Transfer payment to seller (minus marketplace fee)
              (let ((seller-amount (try! (as-contract (transfer-with-fee current-bid seller)))))
                ;; Transfer domain to winner
                (map-set domains { name: domain-name } (merge domain-info { owner: winner }))
                (map-delete domain-auctions { auction-id: auction-id })
                
                (log-domain-action domain-name "auction-finalize")
                
                (ok {
                  domain: domain-name,
                  winner: winner,
                  final-price: current-bid,
                  seller-received: seller-amount
                })
              )
            )
            (err ERR_AUCTION_NOT_FOUND) ;; Should not happen
          )
          ;; No bids - just remove auction
          (begin
            (map-delete domain-auctions { auction-id: auction-id })
            (log-domain-action domain-name "auction-cancel")
            (ok {
              domain: domain-name,
              winner: seller, ;; Domain stays with seller
              final-price: u0,
              seller-received: u0
            })
          )
        )
      )
      (err ERR_AUCTION_NOT_FOUND)
    )
  )
)

;; =============================================================================
;; ADMIN FUNCTIONS
;; =============================================================================

(define-public (set-contract-owner (new-owner principal))
  (begin
    (asserts! (is-eq tx-sender (var-get contract-owner)) (err ERR_UNAUTHORIZED))
    (var-set contract-owner new-owner)
    (ok true)
  )
)

(define-public (set-marketplace-enabled (enabled bool))
  (begin
    (asserts! (is-eq tx-sender (var-get contract-owner)) (err ERR_UNAUTHORIZED))
    (var-set marketplace-enabled enabled)
    (ok true)
  )
)

(define-public (update-pricing (length uint) (base-price uint) (multiplier uint))
  (begin
    (asserts! (is-eq tx-sender (var-get contract-owner)) (err ERR_UNAUTHORIZED))
    (map-set domain-pricing { length: length } { base-price: base-price, multiplier: multiplier })
    (ok true)
  )
)

(define-public (emergency-withdraw (amount uint))
  (begin
    (asserts! (is-eq tx-sender (var-get contract-owner)) (err ERR_UNAUTHORIZED))
    (try! (as-contract (stx-transfer? amount tx-sender (var-get contract-owner))))
    (ok true)
  )
)