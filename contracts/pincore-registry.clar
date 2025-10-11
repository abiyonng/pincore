;; =============================================================================
;; PINCORE DOMAIN REGISTRY CONTRACT
;; =============================================================================
;; Core domain registration, renewal, transfer, and subdomain management

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

;; Dynamic pricing constants
(define-constant PREMIUM_MULTIPLIER_3_CHAR u10) ;; 10x for 3-char domains
(define-constant PREMIUM_MULTIPLIER_4_CHAR u5)  ;; 5x for 4-char domains
(define-constant DEMAND_PRICE_INCREMENT u50000) ;; +0.05 STX per registration attempt

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
(define-constant ERR_INVALID_DOMAIN_NAME u600)
(define-constant ERR_INVALID_LABEL u601)
(define-constant ERR_INVALID_PRICE u602)
(define-constant ERR_INVALID_PRINCIPAL u603)
(define-constant ERR_INVALID_BLOCK_HEIGHT u604)

;; =============================================================================
;; DATA VARIABLES
;; =============================================================================

(define-data-var domain-counter uint u0)
(define-data-var contract-owner principal tx-sender)
(define-data-var marketplace-contract (optional principal) none)

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
(define-map demand-tracker
  { name: (buff 64) }
  { attempts: uint, last-attempt: uint }
)

;; =============================================================================
;; ENHANCED CHARACTER VALIDATION
;; =============================================================================

;; Enhanced character validation for domain names
(define-private (is-valid-char (char (buff 1)))
  (or 
    (and (>= char 0x61) (<= char 0x7a)) ;; a-z
    (and (>= char 0x41) (<= char 0x5a)) ;; A-Z  
    (and (>= char 0x30) (<= char 0x39)) ;; 0-9
    (is-eq char 0x2d)                   ;; hyphen (-)
  )
)

;; Check if name contains only valid characters
(define-private (is-valid-name-chars (name (buff 64)))
  (let ((name-as-max (as-max-len? name u64)))
    (match name-as-max
      valid-name (is-eq (len (filter is-valid-char valid-name)) (len valid-name))
      false
    )
  )
)

;; =============================================================================
;; SAFE DATA EXTRACTION WITH INLINE VALIDATION
;; =============================================================================

(define-private (extract-safe-domain-data (domain-data {owner: principal, expires-at: uint, metadata: (buff 256), created-at: uint}))
  (let (
    ;; Extract fields inline
    (owner-field (get owner domain-data))
    (expires-at-field (get expires-at domain-data))
    (metadata-field (get metadata domain-data))
    (created-at-field (get created-at domain-data))
  )
    ;; Validate extracted fields inline
    (asserts! (not (is-eq owner-field 'SP000000000000000000002Q6VF78)) (err ERR_INVALID_PRINCIPAL))
    (asserts! (>= expires-at-field u0) (err ERR_INVALID_BLOCK_HEIGHT))
    (asserts! (<= (len metadata-field) MAX_METADATA_LENGTH) (err ERR_INVALID_METADATA))
    (asserts! (>= created-at-field u0) (err ERR_INVALID_BLOCK_HEIGHT))
    
    (ok {
      owner: owner-field,
      expires-at: expires-at-field,
      metadata: metadata-field,
      created-at: created-at-field
    })
  )
)

(define-private (extract-safe-subdomain-data (subdomain-data {owner: principal, metadata: (buff 256), created-at: uint}))
  (let (
    ;; Extract fields inline
    (owner-field (get owner subdomain-data))
    (metadata-field (get metadata subdomain-data))
    (created-at-field (get created-at subdomain-data))
  )
    ;; Validate extracted fields inline
    (asserts! (not (is-eq owner-field 'SP000000000000000000002Q6VF78)) (err ERR_INVALID_PRINCIPAL))
    (asserts! (<= (len metadata-field) MAX_METADATA_LENGTH) (err ERR_INVALID_METADATA))
    (asserts! (>= created-at-field u0) (err ERR_INVALID_BLOCK_HEIGHT))
    
    (ok {
      owner: owner-field,
      metadata: metadata-field,
      created-at: created-at-field
    })
  )
)

(define-private (extract-safe-demand-data (demand-data {attempts: uint, last-attempt: uint}))
  (let (
    ;; Extract fields inline
    (attempts-field (get attempts demand-data))
    (last-attempt-field (get last-attempt demand-data))
  )
    ;; Validate extracted fields inline
    (asserts! (>= attempts-field u0) (err ERR_INVALID_PRICE))
    (asserts! (>= last-attempt-field u0) (err ERR_INVALID_BLOCK_HEIGHT))
    
    (ok {
      attempts: attempts-field,
      last-attempt: last-attempt-field
    })
  )
)

;; =============================================================================
;; PRIVATE FUNCTIONS WITH INLINE VALIDATION
;; =============================================================================

(define-private (is-domain-expired (validated-expires-at uint))
  (let (
    (expires-at validated-expires-at)
    (current-height stacks-block-height)
  )
    ;; Validate inputs inline
    (asserts! (>= expires-at u0) (err ERR_INVALID_BLOCK_HEIGHT))
    (asserts! (> current-height u0) (err ERR_INVALID_BLOCK_HEIGHT))
    
    (ok (> current-height expires-at))
  )
)

(define-private (is-in-grace-period (validated-expires-at uint))
  (let (
    (expires-at validated-expires-at)
    (current-height stacks-block-height)
  )
    ;; Validate inputs inline
    (asserts! (>= expires-at u0) (err ERR_INVALID_BLOCK_HEIGHT))
    (asserts! (> current-height u0) (err ERR_INVALID_BLOCK_HEIGHT))
    
    (let ((is-expired (try! (is-domain-expired expires-at))))
      (ok (and is-expired (<= current-height (+ expires-at GRACE_PERIOD))))
    )
  )
)

(define-private (log-domain-action (validated-name (buff 64)) (action (string-ascii 20)))
  (let (
    (domain-name validated-name)
    (action-string action)
    (current-height stacks-block-height)
    (caller tx-sender)
  )
    ;; Validate inputs inline
    (asserts! (>= (len domain-name) MIN_DOMAIN_LENGTH) (err ERR_INVALID_DOMAIN_NAME))
    (asserts! (<= (len domain-name) MAX_DOMAIN_LENGTH) (err ERR_INVALID_DOMAIN_NAME))
    (asserts! (> (len action-string) u0) (err ERR_INVALID_DOMAIN_NAME))
    (asserts! (> current-height u0) (err ERR_INVALID_BLOCK_HEIGHT))
    (asserts! (not (is-eq caller 'SP000000000000000000002Q6VF78)) (err ERR_INVALID_PRINCIPAL))
    
    (map-set domain-history
      { name: domain-name, block: current-height }
      { action: action-string, actor: caller }
    )
    (ok true)
  )
)

(define-private (validate-parent-domain (validated-parent (buff 64)))
  (let (
    (parent-name validated-parent)
  )
    ;; Validate parent name inline
    (asserts! (>= (len parent-name) MIN_DOMAIN_LENGTH) (err ERR_INVALID_DOMAIN_NAME))
    (asserts! (<= (len parent-name) MAX_DOMAIN_LENGTH) (err ERR_INVALID_DOMAIN_NAME))
    
    (match (map-get? domains { name: parent-name })
      parent-domain
      (let ((safe-domain-data (try! (extract-safe-domain-data parent-domain))))
        (let ((expires-at (get expires-at safe-domain-data)))
          (let ((is-expired (try! (is-domain-expired expires-at))))
            (if is-expired
              (err ERR_DOMAIN_EXPIRED)
              (ok safe-domain-data)
            )
          )
        )
      )
      (err ERR_DOMAIN_NOT_FOUND)
    )
  )
)

(define-private (calculate-registration-fee (validated-name (buff 64)))
  (let (
    (domain-name validated-name)
    (domain-length (len domain-name))
    (base-fee BASE_REGISTRATION_FEE)
  )
    ;; Validate domain name inline - return response type consistently
    (asserts! (>= domain-length MIN_DOMAIN_LENGTH) (err ERR_INVALID_DOMAIN_NAME))
    (asserts! (<= domain-length MAX_DOMAIN_LENGTH) (err ERR_INVALID_DOMAIN_NAME))
    
    (let (
      (demand-info (default-to 
        { attempts: u0, last-attempt: u0 }
        (map-get? demand-tracker { name: domain-name })
      ))
    )
      (let ((safe-demand-data (try! (extract-safe-demand-data demand-info))))
        (let (
          (premium-fee (if (<= domain-length u3)
            (* base-fee PREMIUM_MULTIPLIER_3_CHAR)
            (if (<= domain-length u4)
              (* base-fee PREMIUM_MULTIPLIER_4_CHAR)
              base-fee
            )
          ))
          (attempts (get attempts safe-demand-data))
        )
          ;; Return the calculated fee wrapped in ok
          (ok (+ premium-fee (* attempts DEMAND_PRICE_INCREMENT)))
        )
      )
    )
  )
)

(define-private (update-demand-tracker (validated-name (buff 64)))
  (let (
    (domain-name validated-name)
    (current-height stacks-block-height)
  )
    ;; Validate inputs inline
    (asserts! (>= (len domain-name) MIN_DOMAIN_LENGTH) (err ERR_INVALID_DOMAIN_NAME))
    (asserts! (<= (len domain-name) MAX_DOMAIN_LENGTH) (err ERR_INVALID_DOMAIN_NAME))
    (asserts! (> current-height u0) (err ERR_INVALID_BLOCK_HEIGHT))
    
    (let (
      (current-demand (default-to 
        { attempts: u0, last-attempt: u0 }
        (map-get? demand-tracker { name: domain-name })
      ))
    )
      (let ((safe-demand-data (try! (extract-safe-demand-data current-demand))))
        (map-set demand-tracker { name: domain-name } {
          attempts: (+ (get attempts safe-demand-data) u1),
          last-attempt: current-height
        })
        (ok true)
      )
    )
  )
)

;; Check if caller is authorized marketplace contract
(define-private (is-marketplace-authorized)
  (let (
    (marketplace-opt (var-get marketplace-contract))
    (caller contract-caller)
  )
    (match marketplace-opt
      marketplace-addr 
      (begin
        (asserts! (not (is-eq marketplace-addr 'SP000000000000000000002Q6VF78)) false)
        (asserts! (not (is-eq caller 'SP000000000000000000002Q6VF78)) false)
        (is-eq caller marketplace-addr)
      )
      false
    )
  )
)

;; =============================================================================
;; READ-ONLY FUNCTIONS WITH INLINE VALIDATION
;; =============================================================================

(define-read-only (is-available (name (buff 64)))
  (let (
    (domain-name name)
    (domain-length (len domain-name))
    (current-height stacks-block-height)
  )
    ;; Validate domain name inline
    (if (and 
      (>= domain-length MIN_DOMAIN_LENGTH)
      (<= domain-length MAX_DOMAIN_LENGTH)
      (> domain-length u0)
      (is-valid-name-chars domain-name)
      (> current-height u0)
    )
      (let ((domain-info (map-get? domains { name: domain-name })))
        (match domain-info
          domain-data
          (match (extract-safe-domain-data domain-data)
            safe-domain-data
            (let ((expires-at (get expires-at safe-domain-data)))
              (match (is-domain-expired expires-at)
                is-expired (> current-height (+ expires-at GRACE_PERIOD))
                error-code false
              )
            )
            error-code false
          )
          true
        )
      )
      false
    )
  )
)

(define-read-only (get-domain (name (buff 64)))
  (let (
    (domain-name name)
    (domain-length (len domain-name))
  )
    ;; Validate domain name inline
    (if (and 
      (>= domain-length MIN_DOMAIN_LENGTH)
      (<= domain-length MAX_DOMAIN_LENGTH)
      (> domain-length u0)
      (is-valid-name-chars domain-name)
    )
      (map-get? domains { name: domain-name })
      none
    )
  )
)

(define-read-only (get-subdomain (parent (buff 64)) (label (buff 64)))
  (let (
    (parent-name parent)
    (label-name label)
    (parent-length (len parent-name))
    (label-length (len label-name))
  )
    ;; Validate inputs inline
    (if (and 
      (>= parent-length MIN_DOMAIN_LENGTH)
      (<= parent-length MAX_DOMAIN_LENGTH)
      (> parent-length u0)
      (is-valid-name-chars parent-name)
      (>= label-length MIN_DOMAIN_LENGTH)
      (<= label-length MAX_DOMAIN_LENGTH)
      (> label-length u0)
      (is-valid-name-chars label-name)
    )
      (map-get? subdomains { parent: parent-name, label: label-name })
      none
    )
  )
)

(define-read-only (resolve (name (buff 64)))
  (let (
    (domain-name name)
    (domain-length (len domain-name))
  )
    ;; Validate domain name inline
    (if (and 
      (>= domain-length MIN_DOMAIN_LENGTH)
      (<= domain-length MAX_DOMAIN_LENGTH)
      (> domain-length u0)
      (is-valid-name-chars domain-name)
    )
      (let ((domain-info (map-get? domains { name: domain-name })))
        (match domain-info
          domain-data
          (match (extract-safe-domain-data domain-data)
            safe-domain-data
            (let ((expires-at (get expires-at safe-domain-data)))
              (match (is-domain-expired expires-at)
                is-expired
                (if is-expired
                  (err ERR_DOMAIN_EXPIRED)
                  (ok {
                    owner: (get owner safe-domain-data),
                    metadata: (get metadata safe-domain-data),
                    expires-at: expires-at,
                    created-at: (get created-at safe-domain-data)
                  })
                )
                error-code (err error-code)
              )
            )
            error-code (err error-code)
          )
          (err ERR_DOMAIN_NOT_FOUND)
        )
      )
      (err ERR_INVALID_DOMAIN_NAME)
    )
  )
)

(define-read-only (resolve-subdomain (parent (buff 64)) (label (buff 64)))
  (let (
    (parent-name parent)
    (label-name label)
    (parent-length (len parent-name))
    (label-length (len label-name))
  )
    ;; Validate inputs inline
    (if (and 
      (>= parent-length MIN_DOMAIN_LENGTH)
      (<= parent-length MAX_DOMAIN_LENGTH)
      (> parent-length u0)
      (is-valid-name-chars parent-name)
      (>= label-length MIN_DOMAIN_LENGTH)
      (<= label-length MAX_DOMAIN_LENGTH)
      (> label-length u0)
      (is-valid-name-chars label-name)
    )
      (match (validate-parent-domain parent-name)
        parent-domain
        (let ((subdomain-info (get-subdomain parent-name label-name)))
          (match subdomain-info
            subdomain-data
            (match (extract-safe-subdomain-data subdomain-data)
              safe-subdomain-data
              (ok {
                owner: (get owner safe-subdomain-data),
                metadata: (get metadata safe-subdomain-data),
                created-at: (get created-at safe-subdomain-data),
                parent-owner: (get owner parent-domain)
              })
              error-code (err error-code)
            )
            (err ERR_SUBDOMAIN_NOT_FOUND)
          )
        )
        error-code (err error-code)
      )
      (err ERR_INVALID_DOMAIN_NAME)
    )
  )
)

(define-read-only (get-domain-count)
  (var-get domain-counter)
)

(define-read-only (get-contract-owner)
  (var-get contract-owner)
)

(define-read-only (get-marketplace-contract)
  (var-get marketplace-contract)
)

(define-read-only (get-registration-price (name (buff 64)))
  (let (
    (domain-name name)
    (domain-length (len domain-name))
  )
    (if (and 
      (>= domain-length MIN_DOMAIN_LENGTH)
      (<= domain-length MAX_DOMAIN_LENGTH)
      (> domain-length u0)
      (is-valid-name-chars domain-name)
    )
      ;; Use unwrap! with default value on error
      (unwrap! (calculate-registration-fee domain-name) u0)
      u0
    )
  )
)

(define-read-only (get-demand-info (name (buff 64)))
  (let (
    (domain-name name)
    (domain-length (len domain-name))
  )
    ;; Validate domain name inline
    (if (and 
      (>= domain-length MIN_DOMAIN_LENGTH)
      (<= domain-length MAX_DOMAIN_LENGTH)
      (> domain-length u0)
      (is-valid-name-chars domain-name)
    )
      (map-get? demand-tracker { name: domain-name })
      none
    )
  )
)

(define-read-only (whoami)
  tx-sender
)

;; =============================================================================
;; PUBLIC FUNCTIONS WITH ENHANCED INLINE VALIDATION
;; =============================================================================

(define-public (register (name (buff 64)) (metadata (buff 256)))
  (let (
    ;; Validate inputs inline
    (domain-name name)
    (domain-metadata metadata)
    (domain-length (len domain-name))
    (metadata-length (len domain-metadata))
    (caller tx-sender)
    (current-height stacks-block-height)
  )
    ;; Inline validation of all inputs
    (asserts! (>= domain-length MIN_DOMAIN_LENGTH) (err ERR_INVALID_DOMAIN_NAME))
    (asserts! (<= domain-length MAX_DOMAIN_LENGTH) (err ERR_INVALID_DOMAIN_NAME))
    (asserts! (> domain-length u0) (err ERR_INVALID_DOMAIN_NAME))
    (asserts! (is-valid-name-chars domain-name) (err ERR_INVALID_DOMAIN_NAME))
    (asserts! (<= metadata-length MAX_METADATA_LENGTH) (err ERR_INVALID_METADATA))
    (asserts! (not (is-eq caller 'SP000000000000000000002Q6VF78)) (err ERR_INVALID_PRINCIPAL))
    (asserts! (> current-height u0) (err ERR_INVALID_BLOCK_HEIGHT))
    
    ;; Check availability
    (asserts! (is-available domain-name) (err ERR_DOMAIN_UNAVAILABLE))
    
    ;; Calculate registration fee - properly handle response type with try!
    (let ((registration-fee (try! (calculate-registration-fee domain-name))))
      (try! (update-demand-tracker domain-name))
      
      ;; Transfer payment
      (try! (stx-transfer? registration-fee caller (as-contract tx-sender)))
      
      (let ((expiry (+ current-height DOMAIN_REGISTRATION_PERIOD)))
        ;; Create domain record
        (map-set domains { name: domain-name } {
          owner: caller,
          expires-at: expiry,
          metadata: domain-metadata,
          created-at: current-height
        })
        
        ;; Update counter and log
        (var-set domain-counter (+ (var-get domain-counter) u1))
        (try! (log-domain-action domain-name "register"))
        
        (ok {
          domain: domain-name,
          owner: caller,
          expires-at: expiry,
          price-paid: registration-fee
        })
      )
    )
  )
)

(define-public (renew (name (buff 64)))
  (let (
    ;; Validate inputs inline
    (domain-name name)
    (domain-length (len domain-name))
    (caller tx-sender)
  )
    ;; Inline validation
    (asserts! (>= domain-length MIN_DOMAIN_LENGTH) (err ERR_INVALID_DOMAIN_NAME))
    (asserts! (<= domain-length MAX_DOMAIN_LENGTH) (err ERR_INVALID_DOMAIN_NAME))
    (asserts! (> domain-length u0) (err ERR_INVALID_DOMAIN_NAME))
    (asserts! (is-valid-name-chars domain-name) (err ERR_INVALID_DOMAIN_NAME))
    (asserts! (not (is-eq caller 'SP000000000000000000002Q6VF78)) (err ERR_INVALID_PRINCIPAL))
    
    ;; Get domain
    (match (get-domain domain-name)
      domain-data
      (let ((safe-domain-data (try! (extract-safe-domain-data domain-data))))
        (let (
          (current-owner (get owner safe-domain-data))
          (current-expiry (get expires-at safe-domain-data))
          (domain-metadata (get metadata safe-domain-data))
          (created-at (get created-at safe-domain-data))
        )
          ;; Verify ownership
          (asserts! (is-eq caller current-owner) (err ERR_UNAUTHORIZED))
          
          ;; Check if renewal is allowed
          (let (
            (is-expired (try! (is-domain-expired current-expiry)))
            (in-grace (try! (is-in-grace-period current-expiry)))
          )
            (asserts! (or (not is-expired) in-grace) (err ERR_DOMAIN_EXPIRED))
            
            ;; Transfer renewal fee
            (try! (stx-transfer? BASE_REGISTRATION_FEE caller (as-contract tx-sender)))
            
            ;; Update domain with new expiry
            (let ((new-expiry (+ current-expiry DOMAIN_REGISTRATION_PERIOD)))
              (map-set domains { name: domain-name } {
                owner: current-owner,
                expires-at: new-expiry,
                metadata: domain-metadata,
                created-at: created-at
              })
              (try! (log-domain-action domain-name "renew"))
              
              (ok {
                domain: domain-name,
                new-expires-at: new-expiry
              })
            )
          )
        )
      )
      (err ERR_DOMAIN_NOT_FOUND)
    )
  )
)

(define-public (transfer (name (buff 64)) (new-owner principal))
  (let (
    ;; Validate inputs inline
    (domain-name name)
    (recipient new-owner)
    (domain-length (len domain-name))
    (caller tx-sender)
  )
    ;; Inline validation
    (asserts! (>= domain-length MIN_DOMAIN_LENGTH) (err ERR_INVALID_DOMAIN_NAME))
    (asserts! (<= domain-length MAX_DOMAIN_LENGTH) (err ERR_INVALID_DOMAIN_NAME))
    (asserts! (> domain-length u0) (err ERR_INVALID_DOMAIN_NAME))
    (asserts! (is-valid-name-chars domain-name) (err ERR_INVALID_DOMAIN_NAME))
    (asserts! (not (is-eq recipient 'SP000000000000000000002Q6VF78)) (err ERR_INVALID_PRINCIPAL))
    (asserts! (not (is-eq caller 'SP000000000000000000002Q6VF78)) (err ERR_INVALID_PRINCIPAL))
    
    ;; Get domain
    (match (get-domain domain-name)
      domain-data
      (let ((safe-domain-data (try! (extract-safe-domain-data domain-data))))
        (let ((expires-at (get expires-at safe-domain-data)))
          ;; Verify ownership
          (asserts! (is-eq caller (get owner safe-domain-data)) (err ERR_UNAUTHORIZED))
          
          ;; Check domain not expired
          (let ((is-expired (try! (is-domain-expired expires-at))))
            (asserts! (not is-expired) (err ERR_DOMAIN_EXPIRED))
            
            ;; Update ownership
            (map-set domains { name: domain-name } (merge safe-domain-data { owner: recipient }))
            (try! (log-domain-action domain-name "transfer"))
            
            (ok {
              domain: domain-name,
              old-owner: caller,
              new-owner: recipient
            })
          )
        )
      )
      (err ERR_DOMAIN_NOT_FOUND)
    )
  )
)

;; Marketplace-authorized transfer (called by marketplace contract)
(define-public (marketplace-transfer (name (buff 64)) (from principal) (to principal))
  (let (
    ;; Validate inputs inline
    (domain-name name)
    (seller from)
    (buyer to)
    (domain-length (len domain-name))
  )
    ;; Inline validation
    (asserts! (>= domain-length MIN_DOMAIN_LENGTH) (err ERR_INVALID_DOMAIN_NAME))
    (asserts! (<= domain-length MAX_DOMAIN_LENGTH) (err ERR_INVALID_DOMAIN_NAME))
    (asserts! (> domain-length u0) (err ERR_INVALID_DOMAIN_NAME))
    (asserts! (is-valid-name-chars domain-name) (err ERR_INVALID_DOMAIN_NAME))
    (asserts! (not (is-eq seller 'SP000000000000000000002Q6VF78)) (err ERR_INVALID_PRINCIPAL))
    (asserts! (not (is-eq buyer 'SP000000000000000000002Q6VF78)) (err ERR_INVALID_PRINCIPAL))
    (asserts! (is-marketplace-authorized) (err ERR_UNAUTHORIZED))
    
    ;; Get domain
    (match (get-domain domain-name)
      domain-data
      (let ((safe-domain-data (try! (extract-safe-domain-data domain-data))))
        (let ((expires-at (get expires-at safe-domain-data)))
          ;; Verify seller ownership
          (asserts! (is-eq seller (get owner safe-domain-data)) (err ERR_UNAUTHORIZED))
          
          ;; Check domain not expired
          (let ((is-expired (try! (is-domain-expired expires-at))))
            (asserts! (not is-expired) (err ERR_DOMAIN_EXPIRED))
            
            ;; Update ownership
            (map-set domains { name: domain-name } (merge safe-domain-data { owner: buyer }))
            (try! (log-domain-action domain-name "marketplace-transfer"))
            
            (ok true)
          )
        )
      )
      (err ERR_DOMAIN_NOT_FOUND)
    )
  )
)

(define-public (set-metadata (name (buff 64)) (new-metadata (buff 256)))
  (let (
    ;; Validate inputs inline
    (domain-name name)
    (metadata new-metadata)
    (domain-length (len domain-name))
    (metadata-length (len metadata))
    (caller tx-sender)
  )
    ;; Inline validation
    (asserts! (>= domain-length MIN_DOMAIN_LENGTH) (err ERR_INVALID_DOMAIN_NAME))
    (asserts! (<= domain-length MAX_DOMAIN_LENGTH) (err ERR_INVALID_DOMAIN_NAME))
    (asserts! (> domain-length u0) (err ERR_INVALID_DOMAIN_NAME))
    (asserts! (is-valid-name-chars domain-name) (err ERR_INVALID_DOMAIN_NAME))
    (asserts! (<= metadata-length MAX_METADATA_LENGTH) (err ERR_INVALID_METADATA))
    (asserts! (not (is-eq caller 'SP000000000000000000002Q6VF78)) (err ERR_INVALID_PRINCIPAL))
    
    ;; Get domain
    (match (get-domain domain-name)
      domain-data
      (let ((safe-domain-data (try! (extract-safe-domain-data domain-data))))
        (let ((expires-at (get expires-at safe-domain-data)))
          ;; Verify ownership
          (asserts! (is-eq caller (get owner safe-domain-data)) (err ERR_UNAUTHORIZED))
          
          ;; Check domain not expired
          (let ((is-expired (try! (is-domain-expired expires-at))))
            (asserts! (not is-expired) (err ERR_DOMAIN_EXPIRED))
            
            ;; Update metadata
            (map-set domains { name: domain-name } (merge safe-domain-data { metadata: metadata }))
            (try! (log-domain-action domain-name "update-metadata"))
            
            (ok true)
          )
        )
      )
      (err ERR_DOMAIN_NOT_FOUND)
    )
  )
)

(define-public (create-subdomain (parent (buff 64)) (label (buff 64)) (metadata (buff 256)))
  (let (
    ;; Validate inputs inline
    (parent-name parent)
    (label-name label)
    (subdomain-metadata metadata)
    (parent-length (len parent-name))
    (label-length (len label-name))
    (metadata-length (len subdomain-metadata))
    (caller tx-sender)
    (current-height stacks-block-height)
  )
    ;; Inline validation
    (asserts! (>= parent-length MIN_DOMAIN_LENGTH) (err ERR_INVALID_DOMAIN_NAME))
    (asserts! (<= parent-length MAX_DOMAIN_LENGTH) (err ERR_INVALID_DOMAIN_NAME))
    (asserts! (> parent-length u0) (err ERR_INVALID_DOMAIN_NAME))
    (asserts! (is-valid-name-chars parent-name) (err ERR_INVALID_DOMAIN_NAME))
    (asserts! (>= label-length MIN_DOMAIN_LENGTH) (err ERR_INVALID_LABEL))
    (asserts! (<= label-length MAX_DOMAIN_LENGTH) (err ERR_INVALID_LABEL))
    (asserts! (> label-length u0) (err ERR_INVALID_LABEL))
    (asserts! (is-valid-name-chars label-name) (err ERR_INVALID_LABEL))
    (asserts! (<= metadata-length MAX_METADATA_LENGTH) (err ERR_INVALID_METADATA))
    (asserts! (not (is-eq caller 'SP000000000000000000002Q6VF78)) (err ERR_INVALID_PRINCIPAL))
    (asserts! (> current-height u0) (err ERR_INVALID_BLOCK_HEIGHT))
    
    ;; Validate parent domain
    (match (validate-parent-domain parent-name)
      parent-domain
      (begin
        ;; Verify caller owns parent domain
        (asserts! (is-eq caller (get owner parent-domain)) (err ERR_UNAUTHORIZED))
        
        ;; Create subdomain
        (map-set subdomains { parent: parent-name, label: label-name } {
          owner: caller,
          metadata: subdomain-metadata,
          created-at: current-height
        })
        
        (ok {
          parent: parent-name,
          label: label-name,
          owner: caller
        })
      )
      error-code (err error-code)
    )
  )
)

(define-public (transfer-subdomain (parent (buff 64)) (label (buff 64)) (new-owner principal))
  (let (
    ;; Validate inputs inline
    (parent-name parent)
    (label-name label)
    (recipient new-owner)
    (parent-length (len parent-name))
    (label-length (len label-name))
    (caller tx-sender)
  )
    ;; Inline validation
    (asserts! (>= parent-length MIN_DOMAIN_LENGTH) (err ERR_INVALID_DOMAIN_NAME))
    (asserts! (<= parent-length MAX_DOMAIN_LENGTH) (err ERR_INVALID_DOMAIN_NAME))
    (asserts! (> parent-length u0) (err ERR_INVALID_DOMAIN_NAME))
    (asserts! (is-valid-name-chars parent-name) (err ERR_INVALID_DOMAIN_NAME))
    (asserts! (>= label-length MIN_DOMAIN_LENGTH) (err ERR_INVALID_LABEL))
    (asserts! (<= label-length MAX_DOMAIN_LENGTH) (err ERR_INVALID_LABEL))
    (asserts! (> label-length u0) (err ERR_INVALID_LABEL))
    (asserts! (is-valid-name-chars label-name) (err ERR_INVALID_LABEL))
    (asserts! (not (is-eq recipient 'SP000000000000000000002Q6VF78)) (err ERR_INVALID_PRINCIPAL))
    (asserts! (not (is-eq caller 'SP000000000000000000002Q6VF78)) (err ERR_INVALID_PRINCIPAL))
    
    ;; Get subdomain
    (match (get-subdomain parent-name label-name)
      subdomain-data
      (let ((safe-subdomain-data (try! (extract-safe-subdomain-data subdomain-data))))
        ;; Verify ownership
        (asserts! (is-eq caller (get owner safe-subdomain-data)) (err ERR_UNAUTHORIZED))
        
        ;; Validate parent domain still exists and is active
        (match (validate-parent-domain parent-name)
          parent-domain
          (begin
            ;; Update subdomain ownership
            (map-set subdomains { parent: parent-name, label: label-name } 
              (merge safe-subdomain-data { owner: recipient }))
            
            (ok {
              parent: parent-name,
              label: label-name,
              old-owner: caller,
              new-owner: recipient
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
  (let (
    ;; Validate inputs inline
    (parent-name parent)
    (label-name label)
    (metadata new-metadata)
    (parent-length (len parent-name))
    (label-length (len label-name))
    (metadata-length (len metadata))
    (caller tx-sender)
  )
    ;; Inline validation
    (asserts! (>= parent-length MIN_DOMAIN_LENGTH) (err ERR_INVALID_DOMAIN_NAME))
    (asserts! (<= parent-length MAX_DOMAIN_LENGTH) (err ERR_INVALID_DOMAIN_NAME))
    (asserts! (> parent-length u0) (err ERR_INVALID_DOMAIN_NAME))
    (asserts! (is-valid-name-chars parent-name) (err ERR_INVALID_DOMAIN_NAME))
    (asserts! (>= label-length MIN_DOMAIN_LENGTH) (err ERR_INVALID_LABEL))
    (asserts! (<= label-length MAX_DOMAIN_LENGTH) (err ERR_INVALID_LABEL))
    (asserts! (> label-length u0) (err ERR_INVALID_LABEL))
    (asserts! (is-valid-name-chars label-name) (err ERR_INVALID_LABEL))
    (asserts! (<= metadata-length MAX_METADATA_LENGTH) (err ERR_INVALID_METADATA))
    (asserts! (not (is-eq caller 'SP000000000000000000002Q6VF78)) (err ERR_INVALID_PRINCIPAL))
    
    ;; Get subdomain
    (match (get-subdomain parent-name label-name)
      subdomain-data
      (let ((safe-subdomain-data (try! (extract-safe-subdomain-data subdomain-data))))
        ;; Verify ownership
        (asserts! (is-eq caller (get owner safe-subdomain-data)) (err ERR_UNAUTHORIZED))
        
        ;; Validate parent domain still exists and is active
        (match (validate-parent-domain parent-name)
          parent-domain
          (begin
            ;; Update subdomain metadata
            (map-set subdomains { parent: parent-name, label: label-name } 
              (merge safe-subdomain-data { metadata: metadata }))
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

(define-public (set-marketplace-contract (marketplace-addr principal))
  (let (
    ;; Validate inputs inline
    (marketplace marketplace-addr)
    (current-owner (var-get contract-owner))
    (caller tx-sender)
  )
    ;; Inline validation
    (asserts! (not (is-eq marketplace 'SP000000000000000000002Q6VF78)) (err ERR_INVALID_PRINCIPAL))
    (asserts! (not (is-eq current-owner 'SP000000000000000000002Q6VF78)) (err ERR_INVALID_PRINCIPAL))
    (asserts! (not (is-eq caller 'SP000000000000000000002Q6VF78)) (err ERR_INVALID_PRINCIPAL))
    (asserts! (is-eq caller current-owner) (err ERR_UNAUTHORIZED))
    
    (var-set marketplace-contract (some marketplace))
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
    (asserts! (<= withdrawal-amount u1000000000000) (err ERR_INVALID_PRICE))
    (asserts! (not (is-eq current-owner 'SP000000000000000000002Q6VF78)) (err ERR_INVALID_PRINCIPAL))
    (asserts! (not (is-eq caller 'SP000000000000000000002Q6VF78)) (err ERR_INVALID_PRINCIPAL))
    (asserts! (is-eq caller current-owner) (err ERR_UNAUTHORIZED))
    
    (try! (as-contract (stx-transfer? withdrawal-amount tx-sender current-owner)))
    (ok true)
  )
)
