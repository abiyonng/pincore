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
;; INPUT VALIDATION FUNCTIONS
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

;; Enhanced domain name validation
(define-private (validate-domain-name (name (buff 64)))
  (let ((name-len (len name)))
    (if (and
      (>= name-len MIN_DOMAIN_LENGTH)
      (<= name-len MAX_DOMAIN_LENGTH)
      (> name-len u0)
      (is-valid-name-chars name)
    )
      (ok name)
      (err ERR_INVALID_DOMAIN_NAME)
    )
  )
)

;; Enhanced label validation
(define-private (validate-label (label (buff 64)))
  (let ((label-len (len label)))
    (if (and
      (>= label-len MIN_DOMAIN_LENGTH)
      (<= label-len MAX_DOMAIN_LENGTH)
      (> label-len u0)
      (is-valid-name-chars label)
    )
      (ok label)
      (err ERR_INVALID_LABEL)
    )
  )
)

;; Enhanced metadata validation
(define-private (validate-metadata (metadata (buff 256)))
  (if (<= (len metadata) MAX_METADATA_LENGTH)
    (ok metadata)
    (err ERR_INVALID_METADATA)
  )
)

;; Enhanced price validation
(define-private (validate-price (price uint))
  (if (and 
    (> price u0)
    (<= price u1000000000000) ;; Reasonable upper limit
  )
    (ok price)
    (err ERR_INVALID_PRICE)
  )
)

;; Principal validation
(define-private (validate-principal (addr principal))
  (if (not (is-eq addr 'SP000000000000000000002Q6VF78))
    (ok addr)
    (err ERR_INVALID_PRINCIPAL)
  )
)

;; Block height validation
(define-private (validate-block-height (height uint))
  (if (> height u0)
    (ok height)
    (err ERR_INVALID_BLOCK_HEIGHT)
  )
)

;; Validate uint values from map data
(define-private (validate-uint (value uint))
  (if (>= value u0)
    (ok value)
    (err ERR_INVALID_PRICE)
  )
)

;; Validate string values
(define-private (validate-string (value (string-ascii 20)))
  (if (> (len value) u0)
    (ok value)
    (err ERR_INVALID_DOMAIN_NAME)
  )
)

;; =============================================================================
;; SAFE DATA EXTRACTION FUNCTIONS
;; =============================================================================

;; Safely extract and validate domain data
(define-private (get-safe-domain-data (domain-data {owner: principal, expires-at: uint, metadata: (buff 256), created-at: uint}))
  (let (
    (safe-owner (try! (validate-principal (get owner domain-data))))
    (safe-expires-at (try! (validate-uint (get expires-at domain-data))))
    (safe-metadata (try! (validate-metadata (get metadata domain-data))))
    (safe-created-at (try! (validate-uint (get created-at domain-data))))
  )
    (ok {
      owner: safe-owner,
      expires-at: safe-expires-at,
      metadata: safe-metadata,
      created-at: safe-created-at
    })
  )
)

;; Safely extract and validate subdomain data
(define-private (get-safe-subdomain-data (subdomain-data {owner: principal, metadata: (buff 256), created-at: uint}))
  (let (
    (safe-owner (try! (validate-principal (get owner subdomain-data))))
    (safe-metadata (try! (validate-metadata (get metadata subdomain-data))))
    (safe-created-at (try! (validate-uint (get created-at subdomain-data))))
  )
    (ok {
      owner: safe-owner,
      metadata: safe-metadata,
      created-at: safe-created-at
    })
  )
)

;; Safely extract demand tracker data
(define-private (get-safe-demand-data (demand-data {attempts: uint, last-attempt: uint}))
  (let (
    (safe-attempts (try! (validate-uint (get attempts demand-data))))
    (safe-last-attempt (try! (validate-uint (get last-attempt demand-data))))
  )
    (ok {
      attempts: safe-attempts,
      last-attempt: safe-last-attempt
    })
  )
)

;; =============================================================================
;; PRIVATE FUNCTIONS
;; =============================================================================

(define-private (is-domain-expired (expires-at uint))
  (let ((validated-expires-at (try! (validate-uint expires-at))))
    (ok (> stacks-block-height validated-expires-at))
  )
)

(define-private (is-in-grace-period (expires-at uint))
  (let ((validated-expires-at (try! (validate-uint expires-at))))
    (match (is-domain-expired validated-expires-at)
      is-expired (ok (and is-expired (<= stacks-block-height (+ validated-expires-at GRACE_PERIOD))))
      error-code (err error-code)
    )
  )
)

(define-private (log-domain-action (validated-name (buff 64)) (action (string-ascii 20)))
  (let (
    (validated-block (try! (validate-block-height stacks-block-height)))
    (validated-action (try! (validate-string action)))
    (validated-actor (try! (validate-principal tx-sender)))
  )
    (map-set domain-history
      { name: validated-name, block: validated-block }
      { action: validated-action, actor: validated-actor }
    )
    (ok true)
  )
)

(define-private (validate-parent-domain (validated-parent (buff 64)))
  (match (map-get? domains { name: validated-parent })
    parent-domain
    (let ((safe-domain-data (try! (get-safe-domain-data parent-domain))))
      (match (is-domain-expired (get expires-at safe-domain-data))
        is-expired
        (if is-expired
          (err ERR_DOMAIN_EXPIRED)
          (ok safe-domain-data)
        )
        error-code (err error-code)
      )
    )
    (err ERR_DOMAIN_NOT_FOUND)
  )
)

(define-private (calculate-registration-fee (validated-name (buff 64)))
  (let (
    (domain-length (len validated-name))
    (demand-info (default-to 
      { attempts: u0, last-attempt: u0 }
      (map-get? demand-tracker { name: validated-name })
    ))
    (base-fee BASE_REGISTRATION_FEE)
  )
    (let (
      (safe-demand-data (unwrap-panic (get-safe-demand-data demand-info)))
      (premium-fee (if (<= domain-length u3)
        (* base-fee PREMIUM_MULTIPLIER_3_CHAR)
        (if (<= domain-length u4)
          (* base-fee PREMIUM_MULTIPLIER_4_CHAR)
          base-fee
        )
      ))
      (safe-attempts (get attempts safe-demand-data))
    )
      (+ premium-fee (* safe-attempts DEMAND_PRICE_INCREMENT))
    )
  )
)

(define-private (update-demand-tracker (validated-name (buff 64)))
  (let (
    (current-demand (default-to 
      { attempts: u0, last-attempt: u0 }
      (map-get? demand-tracker { name: validated-name })
    ))
    (validated-block (try! (validate-block-height stacks-block-height)))
  )
    (let ((safe-demand-data (try! (get-safe-demand-data current-demand))))
      (map-set demand-tracker { name: validated-name } {
        attempts: (+ (get attempts safe-demand-data) u1),
        last-attempt: validated-block
      })
      (ok true)
    )
  )
)

;; Check if caller is authorized marketplace contract
(define-private (is-marketplace-authorized)
  (match (var-get marketplace-contract)
    marketplace-addr 
    (match (validate-principal marketplace-addr)
      validated-marketplace (is-eq contract-caller validated-marketplace)
      error-code false
    )
    false
  )
)

;; =============================================================================
;; READ-ONLY FUNCTIONS
;; =============================================================================

(define-read-only (is-available (name (buff 64)))
  (match (validate-domain-name name)
    validated-name
    (let ((domain-info (map-get? domains { name: validated-name })))
      (match domain-info
        domain-data
        (match (get-safe-domain-data domain-data)
          safe-domain-data
          (match (is-domain-expired (get expires-at safe-domain-data))
            is-expired (> stacks-block-height (+ (get expires-at safe-domain-data) GRACE_PERIOD))
            error-code false
          )
          error-code false
        )
        true
      )
    )
    error-code false
  )
)

(define-read-only (get-domain (name (buff 64)))
  (match (validate-domain-name name)
    validated-name (map-get? domains { name: validated-name })
    error-code none
  )
)

(define-read-only (get-subdomain (parent (buff 64)) (label (buff 64)))
  (match (validate-domain-name parent)
    validated-parent
    (match (validate-label label)
      validated-label (map-get? subdomains { parent: validated-parent, label: validated-label })
      error-code none
    )
    error-code none
  )
)

(define-read-only (resolve (name (buff 64)))
  (match (validate-domain-name name)
    validated-name
    (let ((domain-info (map-get? domains { name: validated-name })))
      (match domain-info
        domain-data
        (match (get-safe-domain-data domain-data)
          safe-domain-data
          (match (is-domain-expired (get expires-at safe-domain-data))
            is-expired
            (if is-expired
              (err ERR_DOMAIN_EXPIRED)
              (ok {
                owner: (get owner safe-domain-data),
                metadata: (get metadata safe-domain-data),
                expires-at: (get expires-at safe-domain-data),
                created-at: (get created-at safe-domain-data)
              })
            )
            error-code (err error-code)
          )
          error-code (err error-code)
        )
        (err ERR_DOMAIN_NOT_FOUND)
      )
    )
    error-code (err error-code)
  )
)

(define-read-only (resolve-subdomain (parent (buff 64)) (label (buff 64)))
  (match (validate-domain-name parent)
    validated-parent
    (match (validate-label label)
      validated-label
      (match (validate-parent-domain validated-parent)
        parent-domain
        (let ((subdomain-info (get-subdomain validated-parent validated-label)))
          (match subdomain-info
            subdomain-data
            (match (get-safe-subdomain-data subdomain-data)
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
      error-code (err error-code)
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

(define-read-only (get-marketplace-contract)
  (var-get marketplace-contract)
)

(define-read-only (get-registration-price (name (buff 64)))
  (match (validate-domain-name name)
    validated-name (calculate-registration-fee validated-name)
    error-code u0
  )
)

(define-read-only (get-demand-info (name (buff 64)))
  (match (validate-domain-name name)
    validated-name (map-get? demand-tracker { name: validated-name })
    error-code none
  )
)

(define-read-only (whoami)
  tx-sender
)

;; =============================================================================
;; PUBLIC FUNCTIONS
;; =============================================================================

(define-public (register (name (buff 64)) (metadata (buff 256)))
  (let (
    (validated-name (try! (validate-domain-name name)))
    (validated-metadata (try! (validate-metadata metadata)))
    (validated-block (try! (validate-block-height stacks-block-height)))
  )
    (asserts! (is-available validated-name) (err ERR_DOMAIN_UNAVAILABLE))
    
    (let ((registration-fee (calculate-registration-fee validated-name)))
      (try! (update-demand-tracker validated-name))
      
      (try! (stx-transfer? registration-fee tx-sender (as-contract tx-sender)))
      
      (let (
        (expiry (+ validated-block DOMAIN_REGISTRATION_PERIOD))
        (validated-owner (try! (validate-principal tx-sender)))
      )
        (map-set domains { name: validated-name } {
          owner: validated-owner,
          expires-at: expiry,
          metadata: validated-metadata,
          created-at: validated-block
        })
        
        (var-set domain-counter (+ (var-get domain-counter) u1))
        (try! (log-domain-action validated-name "register"))
        
        (ok {
          domain: validated-name,
          owner: validated-owner,
          expires-at: expiry,
          price-paid: registration-fee
        })
      )
    )
  )
)

(define-public (renew (name (buff 64)))
  (let (
    (validated-name (try! (validate-domain-name name)))
  )
    (match (get-domain validated-name)
      domain-data
      (let ((safe-domain-data (try! (get-safe-domain-data domain-data))))
        (let (
          (safe-current-owner (get owner safe-domain-data))
          (safe-current-expiry (get expires-at safe-domain-data))
          (safe-metadata (get metadata safe-domain-data))
          (safe-created-at (get created-at safe-domain-data))
          (validated-sender (try! (validate-principal tx-sender)))
        )
          (asserts! (is-eq validated-sender safe-current-owner) (err ERR_UNAUTHORIZED))
          (let (
            (is-expired (try! (is-domain-expired safe-current-expiry)))
            (in-grace (try! (is-in-grace-period safe-current-expiry)))
          )
            (asserts! (or (not is-expired) in-grace) (err ERR_DOMAIN_EXPIRED))
            
            (try! (stx-transfer? BASE_REGISTRATION_FEE validated-sender (as-contract tx-sender)))
            
            (let ((new-expiry (+ safe-current-expiry DOMAIN_REGISTRATION_PERIOD)))
              (map-set domains { name: validated-name } {
                owner: safe-current-owner,
                expires-at: new-expiry,
                metadata: safe-metadata,
                created-at: safe-created-at
              })
              (try! (log-domain-action validated-name "renew"))
              
              (ok {
                domain: validated-name,
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
    (validated-name (try! (validate-domain-name name)))
    (validated-new-owner (try! (validate-principal new-owner)))
  )
    (match (get-domain validated-name)
      domain-data
      (let ((safe-domain-data (try! (get-safe-domain-data domain-data))))
        (let (
          (safe-expires-at (get expires-at safe-domain-data))
          (validated-sender (try! (validate-principal tx-sender)))
        )
          (asserts! (is-eq validated-sender (get owner safe-domain-data)) (err ERR_UNAUTHORIZED))
          (let ((is-expired (try! (is-domain-expired safe-expires-at))))
            (asserts! (not is-expired) (err ERR_DOMAIN_EXPIRED))
            
            (map-set domains { name: validated-name } (merge safe-domain-data { owner: validated-new-owner }))
            (try! (log-domain-action validated-name "transfer"))
            
            (ok {
              domain: validated-name,
              old-owner: validated-sender,
              new-owner: validated-new-owner
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
    (validated-name (try! (validate-domain-name name)))
    (validated-from (try! (validate-principal from)))
    (validated-to (try! (validate-principal to)))
  )
    (asserts! (is-marketplace-authorized) (err ERR_UNAUTHORIZED))
    
    (match (get-domain validated-name)
      domain-data
      (let ((safe-domain-data (try! (get-safe-domain-data domain-data))))
        (let ((safe-expires-at (get expires-at safe-domain-data)))
          (asserts! (is-eq validated-from (get owner safe-domain-data)) (err ERR_UNAUTHORIZED))
          (let ((is-expired (try! (is-domain-expired safe-expires-at))))
            (asserts! (not is-expired) (err ERR_DOMAIN_EXPIRED))
            
            (map-set domains { name: validated-name } (merge safe-domain-data { owner: validated-to }))
            (try! (log-domain-action validated-name "marketplace-transfer"))
            
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
    (validated-name (try! (validate-domain-name name)))
    (validated-metadata (try! (validate-metadata new-metadata)))
  )
    (match (get-domain validated-name)
      domain-data
      (let ((safe-domain-data (try! (get-safe-domain-data domain-data))))
        (let (
          (safe-expires-at (get expires-at safe-domain-data))
          (validated-sender (try! (validate-principal tx-sender)))
        )
          (asserts! (is-eq validated-sender (get owner safe-domain-data)) (err ERR_UNAUTHORIZED))
          (let ((is-expired (try! (is-domain-expired safe-expires-at))))
            (asserts! (not is-expired) (err ERR_DOMAIN_EXPIRED))
            
            (map-set domains { name: validated-name } (merge safe-domain-data { metadata: validated-metadata }))
            (try! (log-domain-action validated-name "update-metadata"))
            
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
    (validated-parent (try! (validate-domain-name parent)))
    (validated-label (try! (validate-label label)))
    (validated-metadata (try! (validate-metadata metadata)))
    (validated-block (try! (validate-block-height stacks-block-height)))
  )
    (match (validate-parent-domain validated-parent)
      parent-domain
      (let ((validated-sender (try! (validate-principal tx-sender))))
        (asserts! (is-eq validated-sender (get owner parent-domain)) (err ERR_UNAUTHORIZED))
        
        (map-set subdomains { parent: validated-parent, label: validated-label } {
          owner: validated-sender,
          metadata: validated-metadata,
          created-at: validated-block
        })
        
        (ok {
          parent: validated-parent,
          label: validated-label,
          owner: validated-sender
        })
      )
      error-code (err error-code)
    )
  )
)

(define-public (transfer-subdomain (parent (buff 64)) (label (buff 64)) (new-owner principal))
  (let (
    (validated-parent (try! (validate-domain-name parent)))
    (validated-label (try! (validate-label label)))
    (validated-new-owner (try! (validate-principal new-owner)))
  )
    (match (get-subdomain validated-parent validated-label)
      subdomain-data
      (let ((safe-subdomain-data (try! (get-safe-subdomain-data subdomain-data))))
        (let ((validated-sender (try! (validate-principal tx-sender))))
          (asserts! (is-eq validated-sender (get owner safe-subdomain-data)) (err ERR_UNAUTHORIZED))
          
          (match (validate-parent-domain validated-parent)
            parent-domain
            (begin
              (map-set subdomains { parent: validated-parent, label: validated-label } 
                (merge safe-subdomain-data { owner: validated-new-owner }))
              
              (ok {
                parent: validated-parent,
                label: validated-label,
                old-owner: validated-sender,
                new-owner: validated-new-owner
              })
            )
            error-code (err error-code)
          )
        )
      )
      (err ERR_SUBDOMAIN_NOT_FOUND)
    )
  )
)

(define-public (set-subdomain-metadata (parent (buff 64)) (label (buff 64)) (new-metadata (buff 256)))
  (let (
    (validated-parent (try! (validate-domain-name parent)))
    (validated-label (try! (validate-label label)))
    (validated-metadata (try! (validate-metadata new-metadata)))
  )
    (match (get-subdomain validated-parent validated-label)
      subdomain-data
      (let ((safe-subdomain-data (try! (get-safe-subdomain-data subdomain-data))))
        (let ((validated-sender (try! (validate-principal tx-sender))))
          (asserts! (is-eq validated-sender (get owner safe-subdomain-data)) (err ERR_UNAUTHORIZED))
          
          (match (validate-parent-domain validated-parent)
            parent-domain
            (begin
              (map-set subdomains { parent: validated-parent, label: validated-label } 
                (merge safe-subdomain-data { metadata: validated-metadata }))
              (ok true)
            )
            error-code (err error-code)
          )
        )
      )
      (err ERR_SUBDOMAIN_NOT_FOUND)
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

(define-public (set-marketplace-contract (marketplace-addr principal))
  (let (
    (validated-marketplace (try! (validate-principal marketplace-addr)))
    (validated-current-owner (try! (validate-principal (var-get contract-owner))))
    (validated-sender (try! (validate-principal tx-sender)))
  )
    (asserts! (is-eq validated-sender validated-current-owner) (err ERR_UNAUTHORIZED))
    (var-set marketplace-contract (some validated-marketplace))
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
