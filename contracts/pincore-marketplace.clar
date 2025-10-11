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
;; ENHANCED VALIDATION FUNCTIONS (LSP-Compatible)
;; =============================================================================

;; Direct assertion-based validation that LSP can better understand
(define-private (assert-valid-domain-name (name (buff 64)))
  (begin
    (asserts! (>= (len name) u3) (err ERR_INVALID_DOMAIN_NAME))
    (asserts! (<= (len name) u64) (err ERR_INVALID_DOMAIN_NAME))
    (asserts! (> (len name) u0) (err ERR_INVALID_DOMAIN_NAME))
    (ok name)
  )
)

(define-private (assert-valid-price (price uint))
  (begin
    (asserts! (> price u0) (err ERR_INVALID_PRICE))
    (ok price)
  )
)

(define-private (assert-valid-auction-id (auction-id uint))
  (begin
    (asserts! (> auction-id u0) (err ERR_INVALID_AUCTION_ID))
    (ok auction-id)
  )
)

(define-private (assert-valid-auction-duration (duration uint))
  (begin
    (asserts! (>= duration MIN_AUCTION_DURATION) (err ERR_INVALID_AUCTION_DURATION))
    (asserts! (<= duration MAX_AUCTION_DURATION) (err ERR_INVALID_AUCTION_DURATION))
    (ok duration)
  )
)

(define-private (assert-valid-principal (addr principal))
  (begin
    (asserts! (not (is-eq addr 'SP000000000000000000002Q6VF78)) (err ERR_INVALID_PRINCIPAL))
    (ok addr)
  )
)

(define-private (assert-valid-uint (value uint))
  (begin
    (asserts! (>= value u0) (err ERR_INVALID_PRICE))
    (ok value)
  )
)

(define-private (assert-valid-block-height (height uint))
  (begin
    (asserts! (> height u0) (err ERR_INVALID_BLOCK_HEIGHT))
    (ok height)
  )
)

;; =============================================================================
;; SAFE DATA EXTRACTION WITH INLINE VALIDATION
;; =============================================================================

(define-private (extract-safe-listing-data (listing-data {seller: principal, price: uint, listed-at: uint}))
  (let (
    ;; Extract and validate each field inline
    (seller-field (get seller listing-data))
    (price-field (get price listing-data))
    (listed-at-field (get listed-at listing-data))
  )
    ;; Validate extracted fields
    (asserts! (not (is-eq seller-field 'SP000000000000000000002Q6VF78)) (err ERR_INVALID_PRINCIPAL))
    (asserts! (> price-field u0) (err ERR_INVALID_PRICE))
    (asserts! (>= listed-at-field u0) (err ERR_INVALID_BLOCK_HEIGHT))
    
    (ok {
      seller: seller-field,
      price: price-field,
      listed-at: listed-at-field
    })
  )
)

(define-private (extract-safe-auction-data (auction-data {domain: (buff 64), seller: principal, starting-price: uint, current-bid: uint, current-bidder: (optional principal), end-block: uint, created-at: uint}))
  (let (
    ;; Extract fields inline
    (domain-field (get domain auction-data))
    (seller-field (get seller auction-data))
    (starting-price-field (get starting-price auction-data))
    (current-bid-field (get current-bid auction-data))
    (current-bidder-field (get current-bidder auction-data))
    (end-block-field (get end-block auction-data))
    (created-at-field (get created-at auction-data))
  )
    ;; Validate extracted fields inline
    (asserts! (>= (len domain-field) u3) (err ERR_INVALID_DOMAIN_NAME))
    (asserts! (<= (len domain-field) u64) (err ERR_INVALID_DOMAIN_NAME))
    (asserts! (not (is-eq seller-field 'SP000000000000000000002Q6VF78)) (err ERR_INVALID_PRINCIPAL))
    (asserts! (> starting-price-field u0) (err ERR_INVALID_PRICE))
    (asserts! (>= current-bid-field u0) (err ERR_INVALID_PRICE))
    (asserts! (> end-block-field u0) (err ERR_INVALID_BLOCK_HEIGHT))
    (asserts! (>= created-at-field u0) (err ERR_INVALID_BLOCK_HEIGHT))
    
    (ok {
      domain: domain-field,
      seller: seller-field,
      starting-price: starting-price-field,
      current-bid: current-bid-field,
      current-bidder: current-bidder-field,
      end-block: end-block-field,
      created-at: created-at-field
    })
  )
)

(define-private (extract-safe-domain-info (domain-info {owner: principal, expires-at: uint, metadata: (buff 256), created-at: uint}))
  (let (
    ;; Extract fields inline
    (owner-field (get owner domain-info))
    (expires-at-field (get expires-at domain-info))
    (metadata-field (get metadata domain-info))
    (created-at-field (get created-at domain-info))
  )
    ;; Validate extracted fields inline
    (asserts! (not (is-eq owner-field 'SP000000000000000000002Q6VF78)) (err ERR_INVALID_PRINCIPAL))
    (asserts! (> expires-at-field u0) (err ERR_INVALID_BLOCK_HEIGHT))
    (asserts! (>= created-at-field u0) (err ERR_INVALID_BLOCK_HEIGHT))
    
    (ok {
      owner: owner-field,
      expires-at: expires-at-field,
      metadata: metadata-field,
      created-at: created-at-field
    })
  )
)

;; =============================================================================
;; PRIVATE FUNCTIONS WITH INLINE VALIDATION
;; =============================================================================

(define-private (calculate-marketplace-fee (validated-price uint))
  ;; Price is already validated by caller, perform calculation
  (begin
    (asserts! (> validated-price u0) (err ERR_INVALID_PRICE))
    (ok (/ (* validated-price MARKETPLACE_FEE_PERCENT) u10000))
  )
)

(define-private (transfer-with-fee (validated-amount uint) (validated-recipient principal))
  (let (
    ;; Validate inputs inline
    (amount validated-amount)
    (recipient validated-recipient)
  )
    ;; Additional validation
    (asserts! (> amount u0) (err ERR_INVALID_PRICE))
    (asserts! (not (is-eq recipient 'SP000000000000000000002Q6VF78)) (err ERR_INVALID_PRINCIPAL))
    (asserts! (not (is-eq tx-sender 'SP000000000000000000002Q6VF78)) (err ERR_INVALID_PRINCIPAL))
    
    (let (
      (fee (unwrap! (calculate-marketplace-fee amount) (err ERR_INVALID_PRICE)))
      (seller-amount (- amount fee))
    )
      (try! (stx-transfer? seller-amount tx-sender recipient))
      (try! (stx-transfer? fee tx-sender (as-contract tx-sender)))
      (ok seller-amount)
    )
  )
)

(define-private (is-domain-expired-registry (validated-expires-at uint))
  (let (
    (expires-at validated-expires-at)
    (current-height stacks-block-height)
  )
    ;; Validate inputs inline
    (asserts! (> expires-at u0) (err ERR_INVALID_BLOCK_HEIGHT))
    (asserts! (> current-height u0) (err ERR_INVALID_BLOCK_HEIGHT))
    
    (ok (> current-height expires-at))
  )
)

;; =============================================================================
;; READ-ONLY FUNCTIONS WITH INLINE VALIDATION
;; =============================================================================

(define-read-only (get-domain-listing (name (buff 64)))
  (let (
    ;; Validate input inline
    (domain-name name)
  )
    (if (and (>= (len domain-name) u3) (<= (len domain-name) u64) (> (len domain-name) u0))
      (map-get? domain-listings { name: domain-name })
      none
    )
  )
)

(define-read-only (get-auction (auction-id uint))
  (let (
    ;; Validate input inline
    (id auction-id)
  )
    (if (> id u0)
      (map-get? domain-auctions { auction-id: id })
      none
    )
  )
)

(define-read-only (get-auction-bid (auction-id uint) (bidder principal))
  (let (
    ;; Validate inputs inline
    (id auction-id)
    (user bidder)
  )
    (if (and (> id u0) (not (is-eq user 'SP000000000000000000002Q6VF78)))
      (map-get? auction-bids { auction-id: id, bidder: user })
      none
    )
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
;; MARKETPLACE FUNCTIONS WITH ENHANCED VALIDATION
;; =============================================================================

(define-public (list-domain-with-registry 
  (name (buff 64)) 
  (price uint) 
  (registry <registry-trait>))
  (let (
    ;; Validate inputs with inline assertions
    (domain-name name)
    (listing-price price)
    (caller tx-sender)
    (current-height stacks-block-height)
    (marketplace-status (var-get marketplace-enabled))
  )
    ;; Inline validation of all inputs
    (asserts! (>= (len domain-name) u3) (err ERR_INVALID_DOMAIN_NAME))
    (asserts! (<= (len domain-name) u64) (err ERR_INVALID_DOMAIN_NAME))
    (asserts! (> (len domain-name) u0) (err ERR_INVALID_DOMAIN_NAME))
    (asserts! (> listing-price u0) (err ERR_INVALID_PRICE))
    (asserts! (not (is-eq caller 'SP000000000000000000002Q6VF78)) (err ERR_INVALID_PRINCIPAL))
    (asserts! (> current-height u0) (err ERR_INVALID_BLOCK_HEIGHT))
    (asserts! marketplace-status (err ERR_MARKETPLACE_DISABLED))
    
    ;; Get domain info from registry
    (let ((domain-response (unwrap! (contract-call? registry get-domain domain-name) (err ERR_REGISTRY_CALL_FAILED))))
      (match domain-response
        domain-info
        (let ((safe-domain-info (try! (extract-safe-domain-info domain-info))))
          (let (
            (domain-owner (get owner safe-domain-info))
            (domain-expires-at (get expires-at safe-domain-info))
          )
            ;; Verify ownership and expiration
            (asserts! (is-eq caller domain-owner) (err ERR_UNAUTHORIZED))
            
            (let ((is-expired (try! (is-domain-expired-registry domain-expires-at))))
              (asserts! (not is-expired) (err ERR_DOMAIN_EXPIRED))
              
              ;; Create listing
              (map-set domain-listings { name: domain-name } {
                seller: caller,
                price: listing-price,
                listed-at: current-height
              })
              
              (ok true)
            )
          )
        )
        (err ERR_DOMAIN_NOT_FOUND)
      )
    )
  )
)

(define-public (buy-domain-with-registry 
  (name (buff 64)) 
  (registry <registry-trait>))
  (let (
    ;; Validate inputs inline
    (domain-name name)
    (buyer tx-sender)
  )
    ;; Inline validation
    (asserts! (>= (len domain-name) u3) (err ERR_INVALID_DOMAIN_NAME))
    (asserts! (<= (len domain-name) u64) (err ERR_INVALID_DOMAIN_NAME))
    (asserts! (> (len domain-name) u0) (err ERR_INVALID_DOMAIN_NAME))
    (asserts! (not (is-eq buyer 'SP000000000000000000002Q6VF78)) (err ERR_INVALID_PRINCIPAL))
    
    ;; Get domain info from registry
    (let ((domain-response (unwrap! (contract-call? registry get-domain domain-name) (err ERR_REGISTRY_CALL_FAILED))))
      (match domain-response
        domain-info
        (let ((safe-domain-info (try! (extract-safe-domain-info domain-info))))
          ;; Get listing
          (match (get-domain-listing domain-name)
            listing-data
            (let (
              (safe-listing-data (try! (extract-safe-listing-data listing-data)))
              (domain-expires-at (get expires-at safe-domain-info))
            )
              (let (
                (seller (get seller safe-listing-data))
                (listing-price (get price safe-listing-data))
              )
                ;; Verify buyer is not seller
                (asserts! (not (is-eq buyer seller)) (err ERR_UNAUTHORIZED))
                
                ;; Check domain expiration
                (let ((is-expired (try! (is-domain-expired-registry domain-expires-at))))
                  (asserts! (not is-expired) (err ERR_DOMAIN_EXPIRED))
                  
                  ;; Transfer payment
                  (try! (transfer-with-fee listing-price seller))
                  
                  ;; Transfer domain
                  (unwrap! (contract-call? registry marketplace-transfer domain-name seller buyer) (err ERR_REGISTRY_CALL_FAILED))
                  
                  ;; Remove listing
                  (map-delete domain-listings { name: domain-name })
                  
                  (ok {
                    domain: domain-name,
                    buyer: buyer,
                    seller: seller,
                    price: listing-price
                  })
                )
              )
            )
            (err ERR_NOT_FOR_SALE)
          )
        )
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
    ;; Validate inputs inline
    (domain-name name)
    (start-price starting-price)
    (auction-duration duration)
    (seller tx-sender)
    (current-height stacks-block-height)
    (marketplace-status (var-get marketplace-enabled))
    (current-counter (var-get auction-counter))
  )
    ;; Inline validation
    (asserts! (>= (len domain-name) u3) (err ERR_INVALID_DOMAIN_NAME))
    (asserts! (<= (len domain-name) u64) (err ERR_INVALID_DOMAIN_NAME))
    (asserts! (> (len domain-name) u0) (err ERR_INVALID_DOMAIN_NAME))
    (asserts! (> start-price u0) (err ERR_INVALID_PRICE))
    (asserts! (>= auction-duration MIN_AUCTION_DURATION) (err ERR_INVALID_AUCTION_DURATION))
    (asserts! (<= auction-duration MAX_AUCTION_DURATION) (err ERR_INVALID_AUCTION_DURATION))
    (asserts! (not (is-eq seller 'SP000000000000000000002Q6VF78)) (err ERR_INVALID_PRINCIPAL))
    (asserts! (> current-height u0) (err ERR_INVALID_BLOCK_HEIGHT))
    (asserts! marketplace-status (err ERR_MARKETPLACE_DISABLED))
    (asserts! (>= current-counter u0) (err ERR_INVALID_AUCTION_ID))
    
    ;; Get domain info from registry
    (let ((domain-response (unwrap! (contract-call? registry get-domain domain-name) (err ERR_REGISTRY_CALL_FAILED))))
      (match domain-response
        domain-info
        (let ((safe-domain-info (try! (extract-safe-domain-info domain-info))))
          (let (
            (domain-owner (get owner safe-domain-info))
            (domain-expires-at (get expires-at safe-domain-info))
          )
            ;; Verify ownership
            (asserts! (is-eq seller domain-owner) (err ERR_UNAUTHORIZED))
            
            ;; Check expiration
            (let ((is-expired (try! (is-domain-expired-registry domain-expires-at))))
              (asserts! (not is-expired) (err ERR_DOMAIN_EXPIRED))
              
              ;; Remove existing listing
              (map-delete domain-listings { name: domain-name })
              
              ;; Create auction
              (let ((new-auction-id (+ current-counter u1)))
                (map-set domain-auctions { auction-id: new-auction-id } {
                  domain: domain-name,
                  seller: seller,
                  starting-price: start-price,
                  current-bid: u0,
                  current-bidder: none,
                  end-block: (+ current-height auction-duration),
                  created-at: current-height
                })
                
                (var-set auction-counter new-auction-id)
                (ok new-auction-id)
              )
            )
          )
        )
        (err ERR_DOMAIN_NOT_FOUND)
      )
    )
  )
)

(define-public (finalize-auction-with-registry 
  (auction-id uint) 
  (registry <registry-trait>))
  (let (
    ;; Validate inputs inline
    (id auction-id)
    (current-height stacks-block-height)
  )
    ;; Inline validation
    (asserts! (> id u0) (err ERR_INVALID_AUCTION_ID))
    (asserts! (> current-height u0) (err ERR_INVALID_BLOCK_HEIGHT))
    
    ;; Get auction data
    (match (get-auction id)
      auction-data
      (let ((safe-auction-data (try! (extract-safe-auction-data auction-data))))
        (let (
          (domain-name (get domain safe-auction-data))
          (seller (get seller safe-auction-data))
          (current-bid (get current-bid safe-auction-data))
          (end-block (get end-block safe-auction-data))
          (current-bidder-opt (get current-bidder safe-auction-data))
        )
          ;; Ensure auction has ended
          (asserts! (> current-height end-block) (err ERR_AUCTION_ACTIVE))
          
          (if (> current-bid u0)
            ;; Auction had bids
            (match current-bidder-opt
              winner
              (begin
                ;; Validate winner
                (asserts! (not (is-eq winner 'SP000000000000000000002Q6VF78)) (err ERR_INVALID_PRINCIPAL))
                
                ;; Transfer payment
                (let ((seller-amount (try! (as-contract (transfer-with-fee current-bid seller)))))
                  ;; Transfer domain
                  (unwrap! (contract-call? registry marketplace-transfer domain-name seller winner) (err ERR_REGISTRY_CALL_FAILED))
                  
                  ;; Remove auction
                  (map-delete domain-auctions { auction-id: id })
                  
                  (ok {
                    domain: domain-name,
                    winner: winner,
                    final-price: current-bid,
                    seller-received: seller-amount
                  })
                )
              )
              (err ERR_AUCTION_NOT_FOUND)
            )
            ;; No bids
            (begin
              (map-delete domain-auctions { auction-id: id })
              (ok {
                domain: domain-name,
                winner: seller,
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
;; AUCTION FUNCTIONS WITH ENHANCED VALIDATION
;; =============================================================================

(define-public (unlist-domain (name (buff 64)))
  (let (
    ;; Validate inputs inline
    (domain-name name)
    (caller tx-sender)
  )
    ;; Inline validation
    (asserts! (>= (len domain-name) u3) (err ERR_INVALID_DOMAIN_NAME))
    (asserts! (<= (len domain-name) u64) (err ERR_INVALID_DOMAIN_NAME))
    (asserts! (> (len domain-name) u0) (err ERR_INVALID_DOMAIN_NAME))
    (asserts! (not (is-eq caller 'SP000000000000000000002Q6VF78)) (err ERR_INVALID_PRINCIPAL))
    
    ;; Get listing
    (match (get-domain-listing domain-name)
      listing-data
      (let ((safe-listing-data (try! (extract-safe-listing-data listing-data))))
        (let ((seller (get seller safe-listing-data)))
          ;; Verify caller is seller
          (asserts! (is-eq caller seller) (err ERR_UNAUTHORIZED))
          
          ;; Remove listing
          (map-delete domain-listings { name: domain-name })
          (ok true)
        )
      )
      (err ERR_NOT_FOR_SALE)
    )
  )
)

(define-public (place-bid (auction-id uint) (bid-amount uint))
  (let (
    ;; Validate inputs inline
    (id auction-id)
    (amount bid-amount)
    (bidder tx-sender)
    (current-height stacks-block-height)
  )
    ;; Inline validation
    (asserts! (> id u0) (err ERR_INVALID_AUCTION_ID))
    (asserts! (> amount u0) (err ERR_INVALID_PRICE))
    (asserts! (not (is-eq bidder 'SP000000000000000000002Q6VF78)) (err ERR_INVALID_PRINCIPAL))
    (asserts! (> current-height u0) (err ERR_INVALID_BLOCK_HEIGHT))
    
    ;; Get auction data
    (match (get-auction id)
      auction-data
      (let ((safe-auction-data (try! (extract-safe-auction-data auction-data))))
        (let (
          (current-bid (get current-bid safe-auction-data))
          (end-block (get end-block safe-auction-data))
          (seller (get seller safe-auction-data))
          (starting-price (get starting-price safe-auction-data))
          (current-bidder-opt (get current-bidder safe-auction-data))
        )
          ;; Verify bidder is not seller
          (asserts! (not (is-eq bidder seller)) (err ERR_CANNOT_BID_OWN_AUCTION))
          
          ;; Verify auction hasn't ended
          (asserts! (<= current-height end-block) (err ERR_AUCTION_ENDED))
          
          ;; Verify bid is high enough
          (asserts! (> amount (if (> current-bid u0) current-bid starting-price)) (err ERR_BID_TOO_LOW))
          
          ;; Refund previous bidder if exists
          (match current-bidder-opt
            previous-bidder
            (begin
              (asserts! (not (is-eq previous-bidder 'SP000000000000000000002Q6VF78)) (err ERR_INVALID_PRINCIPAL))
              (try! (as-contract (stx-transfer? current-bid tx-sender previous-bidder)))
            )
            true
          )
          
          ;; Escrow new bid
          (try! (stx-transfer? amount bidder (as-contract tx-sender)))
          
          ;; Calculate new end block (extend if needed)
          (let (
            (time-remaining (- end-block current-height))
            (new-end-block (if (<= time-remaining u6)
              (+ end-block AUCTION_EXTENSION_BLOCKS)
              end-block
            ))
          )
            ;; Update auction
            (map-set domain-auctions { auction-id: id } 
              (merge safe-auction-data {
                current-bid: amount,
                current-bidder: (some bidder),
                end-block: new-end-block
              })
            )
            
            ;; Record bid
            (map-set auction-bids { auction-id: id, bidder: bidder } {
              amount: amount,
              block: current-height
            })
            
            (ok {
              auction-id: id,
              bidder: bidder,
              amount: amount,
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
;; ADMIN FUNCTIONS WITH ENHANCED VALIDATION
;; =============================================================================

(define-public (set-contract-owner (new-owner principal))
  (let (
    ;; Validate inputs inline
    (owner new-owner)
    (current-owner (var-get contract-owner))
    (caller tx-sender)
  )
    ;; Inline validation
    (asserts! (not (is-eq owner 'SP000000000000000000002Q6VF78)) (err ERR_INVALID_PRINCIPAL))
    (asserts! (not (is-eq current-owner 'SP000000000000000000002Q6VF78)) (err ERR_INVALID_PRINCIPAL))
    (asserts! (not (is-eq caller 'SP000000000000000000002Q6VF78)) (err ERR_INVALID_PRINCIPAL))
    (asserts! (is-eq caller current-owner) (err ERR_UNAUTHORIZED))
    
    (var-set contract-owner owner)
    (ok true)
  )
)

(define-public (set-marketplace-enabled (enabled bool))
  (let (
    ;; Validate inputs inline
    (status enabled)
    (current-owner (var-get contract-owner))
    (caller tx-sender)
  )
    ;; Inline validation
    (asserts! (not (is-eq current-owner 'SP000000000000000000002Q6VF78)) (err ERR_INVALID_PRINCIPAL))
    (asserts! (not (is-eq caller 'SP000000000000000000002Q6VF78)) (err ERR_INVALID_PRINCIPAL))
    (asserts! (is-eq caller current-owner) (err ERR_UNAUTHORIZED))
    
    (var-set marketplace-enabled status)
    (ok true)
  )
)

(define-public (emergency-withdraw (amount uint))
  (let (
    ;; Validate inputs inline
    (withdrawal-amount amount)
    (current-owner (var-get contract-owner))
    (caller tx-sender)
  )
    ;; Inline validation
    (asserts! (> withdrawal-amount u0) (err ERR_INVALID_PRICE))
    (asserts! (not (is-eq current-owner 'SP000000000000000000002Q6VF78)) (err ERR_INVALID_PRINCIPAL))
    (asserts! (not (is-eq caller 'SP000000000000000000002Q6VF78)) (err ERR_INVALID_PRINCIPAL))
    (asserts! (is-eq caller current-owner) (err ERR_UNAUTHORIZED))
    
    (try! (as-contract (stx-transfer? withdrawal-amount tx-sender current-owner)))
    (ok true)
  )
)

;; =============================================================================
;; TRAIT DEFINITION
;; =============================================================================

(define-trait registry-trait
  (
    (get-domain ((buff 64)) (response (optional {owner: principal, expires-at: uint, metadata: (buff 256), created-at: uint}) uint))
    (marketplace-transfer ((buff 64) principal principal) (response bool uint))
  )
)
