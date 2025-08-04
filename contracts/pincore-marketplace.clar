;; =============================================================================
;; PINCORE MARKETPLACE CONTRACT
;; =============================================================================
;; Domain marketplace, auctions, and trading functionality

;; =============================================================================
;; CONSTANTS
;; =============================================================================

(define-constant MARKETPLACE_FEE_PERCENT u250) ;; 2.5% marketplace fee
(define-constant AUCTION_EXTENSION_BLOCKS u144) ;; ~1 day extension if bid in last hour
(define-constant MIN_AUCTION_DURATION u1440) ;; ~10 days minimum auction
(define-constant MAX_AUCTION_DURATION u14400) ;; ~100 days maximum auction

;; Error codes
(define-constant ERR_UNAUTHORIZED u200)
(define-constant ERR_DOMAIN_NOT_FOUND u404)
(define-constant ERR_DOMAIN_EXPIRED u406)
(define-constant ERR_NOT_FOR_SALE u500)
(define-constant ERR_INVALID_PRICE u501)
(define-constant ERR_AUCTION_NOT_FOUND u502)
(define-constant ERR_AUCTION_ENDED u503)
(define-constant ERR_AUCTION_ACTIVE u504)
(define-constant ERR_BID_TOO_LOW u505)
(define-constant ERR_INVALID_AUCTION_DURATION u506)
(define-constant ERR_CANNOT_BID_OWN_AUCTION u507)
(define-constant ERR_INVALID_DOMAIN_NAME u600)
(define-constant ERR_INVALID_AUCTION_ID u602)
(define-constant ERR_MARKETPLACE_DISABLED u700)
(define-constant ERR_REGISTRY_NOT_SET u701)
(define-constant ERR_INVALID_PRINCIPAL u702)
(define-constant ERR_INVALID_BLOCK_HEIGHT u703)
(define-constant ERR_REGISTRY_CALL_FAILED u704)

;; =============================================================================
;; DATA VARIABLES
;; =============================================================================

(define-data-var contract-owner principal tx-sender)
(define-data-var marketplace-enabled bool true)
(define-data-var auction-counter uint u0)

;; =============================================================================
;; DATA MAPS
;; =============================================================================

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
;; INPUT VALIDATION FUNCTIONS
;; =============================================================================

(define-private (validate-domain-name (name (buff 64)))
  (if (and
    (>= (len name) u3)
    (<= (len name) u64)
    (> (len name) u0)
  )
    (ok name)
    (err ERR_INVALID_DOMAIN_NAME)
  )
)

(define-private (validate-price (price uint))
  (if (> price u0)
    (ok price)
    (err ERR_INVALID_PRICE)
  )
)

(define-private (validate-auction-id (auction-id uint))
  (if (> auction-id u0)
    (ok auction-id)
    (err ERR_INVALID_AUCTION_ID)
  )
)

(define-private (validate-auction-duration (duration uint))
  (if (and (>= duration MIN_AUCTION_DURATION) (<= duration MAX_AUCTION_DURATION))
    (ok duration)
    (err ERR_INVALID_AUCTION_DURATION)
  )
)

(define-private (validate-principal (addr principal))
  (if (not (is-eq addr 'SP000000000000000000002Q6VF78))
    (ok addr)
    (err ERR_INVALID_PRINCIPAL)
  )
)

(define-private (validate-uint (value uint))
  (if (>= value u0)
    (ok value)
    (err ERR_INVALID_PRICE)
  )
)

(define-private (validate-block-height (height uint))
  (if (> height u0)
    (ok height)
    (err ERR_INVALID_BLOCK_HEIGHT)
  )
)

(define-private (validate-bool (value bool))
  (ok value)
)

;; =============================================================================
;; SAFE DATA EXTRACTION FUNCTIONS
;; =============================================================================

(define-private (get-safe-listing-data (listing-data {seller: principal, price: uint, listed-at: uint}))
  (let (
    (safe-seller (try! (validate-principal (get seller listing-data))))
    (safe-price (try! (validate-uint (get price listing-data))))
    (safe-listed-at (try! (validate-uint (get listed-at listing-data))))
  )
    (ok {
      seller: safe-seller,
      price: safe-price,
      listed-at: safe-listed-at
    })
  )
)

(define-private (get-safe-auction-data (auction-data {domain: (buff 64), seller: principal, starting-price: uint, current-bid: uint, current-bidder: (optional principal), end-block: uint, created-at: uint}))
  (let (
    (safe-domain (try! (validate-domain-name (get domain auction-data))))
    (safe-seller (try! (validate-principal (get seller auction-data))))
    (safe-starting-price (try! (validate-uint (get starting-price auction-data))))
    (safe-current-bid (try! (validate-uint (get current-bid auction-data))))
    (safe-end-block (try! (validate-uint (get end-block auction-data))))
    (safe-created-at (try! (validate-uint (get created-at auction-data))))
  )
    (ok {
      domain: safe-domain,
      seller: safe-seller,
      starting-price: safe-starting-price,
      current-bid: safe-current-bid,
      current-bidder: (get current-bidder auction-data),
      end-block: safe-end-block,
      created-at: safe-created-at
    })
  )
)

(define-private (get-safe-bid-data (bid-data {amount: uint, block: uint}))
  (let (
    (safe-amount (try! (validate-uint (get amount bid-data))))
    (safe-block (try! (validate-uint (get block bid-data))))
  )
    (ok {
      amount: safe-amount,
      block: safe-block
    })
  )
)

;; Safe extraction of domain info from registry response
(define-private (get-safe-domain-info (domain-info {owner: principal, expires-at: uint, metadata: (buff 256), created-at: uint}))
  (let (
    (safe-owner (try! (validate-principal (get owner domain-info))))
    (safe-expires-at (try! (validate-uint (get expires-at domain-info))))
    (safe-created-at (try! (validate-uint (get created-at domain-info))))
  )
    (ok {
      owner: safe-owner,
      expires-at: safe-expires-at,
      metadata: (get metadata domain-info),
      created-at: safe-created-at
    })
  )
)

;; =============================================================================
;; PRIVATE FUNCTIONS
;; =============================================================================

(define-private (calculate-marketplace-fee (validated-price uint))
  (/ (* validated-price MARKETPLACE_FEE_PERCENT) u10000)
)

(define-private (transfer-with-fee (validated-amount uint) (recipient principal))
  (let (
    (validated-recipient (try! (validate-principal recipient)))
    (fee (calculate-marketplace-fee validated-amount))
    (seller-amount (- validated-amount fee))
    (validated-sender (try! (validate-principal tx-sender)))
  )
    (try! (stx-transfer? seller-amount validated-sender validated-recipient))
    (try! (stx-transfer? fee validated-sender (as-contract tx-sender)))
    (ok seller-amount)
  )
)

;; Check if domain is expired
(define-private (is-domain-expired-registry (expires-at uint))
  (let (
    (validated-expires-at (try! (validate-uint expires-at)))
    (validated-block-height (try! (validate-block-height stacks-block-height)))
  )
    (ok (> validated-block-height validated-expires-at))
  )
)

;; =============================================================================
;; READ-ONLY FUNCTIONS
;; =============================================================================

(define-read-only (get-domain-listing (name (buff 64)))
  (match (validate-domain-name name)
    validated-name (map-get? domain-listings { name: validated-name })
    error-code none
  )
)

(define-read-only (get-auction (auction-id uint))
  (match (validate-auction-id auction-id)
    validated-id (map-get? domain-auctions { auction-id: validated-id })
    error-code none
  )
)

(define-read-only (get-auction-bid (auction-id uint) (bidder principal))
  (match (validate-auction-id auction-id)
    validated-id 
    (match (validate-principal bidder)
      validated-bidder (map-get? auction-bids { auction-id: validated-id, bidder: validated-bidder })
      error-code none
    )
    error-code none
  )
)

(define-read-only (is-marketplace-enabled)
  (var-get marketplace-enabled)
)

(define-read-only (get-contract-owner)
  (var-get contract-owner)
)

(define-read-only (get-auction-count)
  (var-get auction-counter)
)

;; =============================================================================
;; MARKETPLACE FUNCTIONS (TRAIT-BASED)
;; =============================================================================

;; These functions require the registry contract to be passed as a trait implementation
(define-public (list-domain-with-registry 
  (name (buff 64)) 
  (price uint) 
  (registry <registry-trait>))
  (let (
    (validated-name (try! (validate-domain-name name)))
    (validated-price (try! (validate-price price)))
    (validated-sender (try! (validate-principal tx-sender)))
    (validated-block-height (try! (validate-block-height stacks-block-height)))
    (validated-marketplace-enabled (unwrap-panic (validate-bool (var-get marketplace-enabled))))
  )
    (asserts! validated-marketplace-enabled (err ERR_MARKETPLACE_DISABLED))
    
    ;; Get domain info from registry - handle optional response properly
    (let ((domain-data (unwrap! (contract-call? registry get-domain validated-name) (err ERR_REGISTRY_CALL_FAILED))))
      ;; domain-data is now (optional domain-info), not a response
      (match domain-data
        domain-info  ;; This is the actual domain info struct
        (let ((safe-domain-info (try! (get-safe-domain-info domain-info))))
          (let (
            (safe-owner (get owner safe-domain-info))
            (safe-expires-at (get expires-at safe-domain-info))
          )
            (asserts! (is-eq validated-sender safe-owner) (err ERR_UNAUTHORIZED))
            (let ((is-expired (try! (is-domain-expired-registry safe-expires-at))))
              (asserts! (not is-expired) (err ERR_DOMAIN_EXPIRED))
              
              (map-set domain-listings { name: validated-name } {
                seller: validated-sender,
                price: validated-price,
                listed-at: validated-block-height
              })
              
              (ok true)
            )
          )
        )
        ;; This handles the 'none' case (domain not found)
        (err ERR_DOMAIN_NOT_FOUND)
      )
    )
  )
)

(define-public (buy-domain-with-registry 
  (name (buff 64)) 
  (registry <registry-trait>))
  (let (
    (validated-name (try! (validate-domain-name name)))
    (validated-sender (try! (validate-principal tx-sender)))
  )
    ;; Get domain info from registry - handle optional response properly
    (let ((domain-data (unwrap! (contract-call? registry get-domain validated-name) (err ERR_REGISTRY_CALL_FAILED))))
      ;; domain-data is now (optional domain-info), not a response
      (match domain-data
        domain-info  ;; This is the actual domain info struct
        (let ((safe-domain-info (try! (get-safe-domain-info domain-info))))
          (match (get-domain-listing validated-name)
            listing-data
            (let (
              (safe-listing-data (try! (get-safe-listing-data listing-data)))
              (safe-expires-at (get expires-at safe-domain-info))
            )
              (let (
                (safe-seller (get seller safe-listing-data))
                (safe-price (get price safe-listing-data))
              )
                (asserts! (not (is-eq validated-sender safe-seller)) (err ERR_UNAUTHORIZED))
                (let ((is-expired (try! (is-domain-expired-registry safe-expires-at))))
                  (asserts! (not is-expired) (err ERR_DOMAIN_EXPIRED))
                  
                  ;; Transfer payment with marketplace fee
                  (try! (transfer-with-fee safe-price safe-seller))
                  
                  ;; Transfer domain ownership via registry
                  (unwrap! (contract-call? registry marketplace-transfer validated-name safe-seller validated-sender) (err ERR_REGISTRY_CALL_FAILED))
                  
                  ;; Remove listing
                  (map-delete domain-listings { name: validated-name })
                  
                  (ok {
                    domain: validated-name,
                    buyer: validated-sender,
                    seller: safe-seller,
                    price: safe-price
                  })
                )
              )
            )
            (err ERR_NOT_FOR_SALE)
          )
        )
        ;; This handles the 'none' case (domain not found)
        (err ERR_DOMAIN_NOT_FOUND)
      )
    )
  )
)

(define-public (create-auction-with-registry 
  (name (buff 64)) 
  (starting-price uint) 
  (duration uint) 
  (registry <registry-trait>))
  (let (
    (validated-name (try! (validate-domain-name name)))
    (validated-price (try! (validate-price starting-price)))
    (validated-duration (try! (validate-auction-duration duration)))
    (validated-sender (try! (validate-principal tx-sender)))
    (validated-block-height (try! (validate-block-height stacks-block-height)))
    (validated-marketplace-enabled (unwrap-panic (validate-bool (var-get marketplace-enabled))))
    (validated-auction-counter (try! (validate-uint (var-get auction-counter))))
  )
    (asserts! validated-marketplace-enabled (err ERR_MARKETPLACE_DISABLED))
    
    ;; Get domain info from registry - handle optional response properly
    (let ((domain-data (unwrap! (contract-call? registry get-domain validated-name) (err ERR_REGISTRY_CALL_FAILED))))
      ;; domain-data is now (optional domain-info), not a response
      (match domain-data
        domain-info  ;; This is the actual domain info struct
        (let ((safe-domain-info (try! (get-safe-domain-info domain-info))))
          (let (
            (safe-owner (get owner safe-domain-info))
            (safe-expires-at (get expires-at safe-domain-info))
          )
            (asserts! (is-eq validated-sender safe-owner) (err ERR_UNAUTHORIZED))
            (let ((is-expired (try! (is-domain-expired-registry safe-expires-at))))
              (asserts! (not is-expired) (err ERR_DOMAIN_EXPIRED))
              
              ;; Remove from direct sale listing if exists
              (map-delete domain-listings { name: validated-name })
              
              (let ((auction-id (+ validated-auction-counter u1)))
                (map-set domain-auctions { auction-id: auction-id } {
                  domain: validated-name,
                  seller: validated-sender,
                  starting-price: validated-price,
                  current-bid: u0,
                  current-bidder: none,
                  end-block: (+ validated-block-height validated-duration),
                  created-at: validated-block-height
                })
                
                (var-set auction-counter auction-id)
                
                (ok auction-id)
              )
            )
          )
        )
        ;; This handles the 'none' case (domain not found)
        (err ERR_DOMAIN_NOT_FOUND)
      )
    )
  )
)

(define-public (finalize-auction-with-registry 
  (auction-id uint) 
  (registry <registry-trait>))
  (let (
    (validated-id (try! (validate-auction-id auction-id)))
    (validated-block-height (try! (validate-block-height stacks-block-height)))
  )
    (match (get-auction validated-id)
      auction-data
      (let ((safe-auction-data (try! (get-safe-auction-data auction-data))))
        (let (
          (safe-domain-name (get domain safe-auction-data))
          (safe-seller (get seller safe-auction-data))
          (safe-current-bid (get current-bid safe-auction-data))
          (safe-end-block (get end-block safe-auction-data))
        )
          (asserts! (> validated-block-height safe-end-block) (err ERR_AUCTION_ACTIVE))
          
          (if (> safe-current-bid u0)
            ;; Auction had bids - transfer domain and payment
            (match (get current-bidder safe-auction-data)
              winner
              (let ((validated-winner (try! (validate-principal winner))))
                ;; Transfer payment to seller (minus marketplace fee)
                (let ((seller-amount (try! (as-contract (transfer-with-fee safe-current-bid safe-seller)))))
                  ;; Transfer domain to winner via registry
                  (unwrap! (contract-call? registry marketplace-transfer safe-domain-name safe-seller validated-winner) (err ERR_REGISTRY_CALL_FAILED))
                  
                  (map-delete domain-auctions { auction-id: validated-id })
                  
                  (ok {
                    domain: safe-domain-name,
                    winner: validated-winner,
                    final-price: safe-current-bid,
                    seller-received: seller-amount
                  })
                )
              )
              (err ERR_AUCTION_NOT_FOUND) ;; Should not happen
            )
            ;; No bids - just remove auction
            (begin
              (map-delete domain-auctions { auction-id: validated-id })
              (ok {
                domain: safe-domain-name,
                winner: safe-seller, ;; Domain stays with seller
                final-price: u0,
                seller-received: u0
              })
            )
          )
        )
      )
      (err ERR_AUCTION_NOT_FOUND)
    )
  )
)

;; =============================================================================
;; AUCTION FUNCTIONS
;; =============================================================================

(define-public (unlist-domain (name (buff 64)))
  (let (
    (validated-name (try! (validate-domain-name name)))
    (validated-sender (try! (validate-principal tx-sender)))
  )
    (match (get-domain-listing validated-name)
      listing-data
      (let ((safe-listing-data (try! (get-safe-listing-data listing-data))))
        (asserts! (is-eq validated-sender (get seller safe-listing-data)) (err ERR_UNAUTHORIZED))
        (map-delete domain-listings { name: validated-name })
        (ok true)
      )
      (err ERR_NOT_FOR_SALE)
    )
  )
)

(define-public (place-bid (auction-id uint) (bid-amount uint))
  (let (
    (validated-id (try! (validate-auction-id auction-id)))
    (validated-amount (try! (validate-price bid-amount)))
    (validated-sender (try! (validate-principal tx-sender)))
    (validated-block-height (try! (validate-block-height stacks-block-height)))
  )
    (match (get-auction validated-id)
      auction-data
      (let ((safe-auction-data (try! (get-safe-auction-data auction-data))))
        (let (
          (safe-current-bid (get current-bid safe-auction-data))
          (safe-end-block (get end-block safe-auction-data))
          (safe-seller (get seller safe-auction-data))
          (safe-starting-price (get starting-price safe-auction-data))
        )
          (asserts! (not (is-eq validated-sender safe-seller)) (err ERR_CANNOT_BID_OWN_AUCTION))
          (asserts! (<= validated-block-height safe-end-block) (err ERR_AUCTION_ENDED))
          (asserts! (> validated-amount (if (> safe-current-bid u0) safe-current-bid safe-starting-price)) (err ERR_BID_TOO_LOW))
          
          ;; Refund previous bidder if exists
          (match (get current-bidder safe-auction-data)
            previous-bidder
            (let ((validated-previous-bidder (try! (validate-principal previous-bidder))))
              (try! (as-contract (stx-transfer? safe-current-bid tx-sender validated-previous-bidder)))
            )
            true ;; No previous bidder
          )
          
          ;; Escrow new bid
          (try! (stx-transfer? validated-amount validated-sender (as-contract tx-sender)))
          
          ;; Extend auction if bid placed in last hour
          (let (
            (new-end-block (if (<= (- safe-end-block validated-block-height) u6) ;; ~1 hour
              (+ safe-end-block AUCTION_EXTENSION_BLOCKS)
              safe-end-block
            ))
          )
            ;; Update auction
            (map-set domain-auctions { auction-id: validated-id } 
              (merge safe-auction-data {
                current-bid: validated-amount,
                current-bidder: (some validated-sender),
                end-block: new-end-block
              })
            )
            
            ;; Record bid
            (map-set auction-bids { auction-id: validated-id, bidder: validated-sender } {
              amount: validated-amount,
              block: validated-block-height
            })
            
            (ok {
              auction-id: validated-id,
              bidder: validated-sender,
              amount: validated-amount,
              new-end-block: new-end-block
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
  (let (
    (validated-new-owner (try! (validate-principal new-owner)))
    (validated-current-owner (try! (validate-principal (var-get contract-owner))))
    (validated-sender (try! (validate-principal tx-sender)))
  )
    (asserts! (is-eq validated-sender validated-current-owner) (err ERR_UNAUTHORIZED))
    (var-set contract-owner validated-new-owner)
    (ok true)
  )
)

(define-public (set-marketplace-enabled (enabled bool))
  (let (
    (validated-enabled (unwrap-panic (validate-bool enabled)))
    (validated-current-owner (try! (validate-principal (var-get contract-owner))))
    (validated-sender (try! (validate-principal tx-sender)))
  )
    (asserts! (is-eq validated-sender validated-current-owner) (err ERR_UNAUTHORIZED))
    (var-set marketplace-enabled validated-enabled)
    (ok true)
  )
)

(define-public (emergency-withdraw (amount uint))
  (let (
    (validated-amount (try! (validate-price amount)))
    (validated-current-owner (try! (validate-principal (var-get contract-owner))))
    (validated-sender (try! (validate-principal tx-sender)))
  )
    (asserts! (is-eq validated-sender validated-current-owner) (err ERR_UNAUTHORIZED))
    (try! (as-contract (stx-transfer? validated-amount tx-sender validated-current-owner)))
    (ok true)
  )
)

;; =============================================================================
;; TRAIT DEFINITION (for registry contract)
;; =============================================================================

(define-trait registry-trait
  (
    ;; Get domain information
    (get-domain ((buff 64)) (response (optional {owner: principal, expires-at: uint, metadata: (buff 256), created-at: uint}) uint))
    ;; Transfer domain ownership (marketplace authorized)
    (marketplace-transfer ((buff 64) principal principal) (response bool uint))
  )
)