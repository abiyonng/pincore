# PinCore: Decentralized Domain Name & Marketplace Smart Contract

This Clarity smart contract implements a decentralized domain name registry and marketplace for the Stacks blockchain. It allows users to register, renew, transfer, and trade domain names, as well as create and manage subdomains. The contract supports dynamic pricing, auctions, and marketplace features.

---

## Features

- **Domain Registration:**  
  Register new domains with dynamic pricing based on length and demand.
- **Renewal:**  
  Renew domains during their active or grace period.
- **Transfer:**  
  Transfer domain ownership to another principal.
- **Metadata:**  
  Set and update metadata for domains and subdomains.
- **Subdomains:**  
  Create, transfer, and update subdomains under owned domains.
- **Marketplace:**  
  List domains for sale, buy listed domains, and pay marketplace fees.
- **Auctions:**  
  Auction domains, place bids, and finalize auctions.
- **Admin Controls:**  
  Change contract owner, enable/disable marketplace, update pricing, and emergency fund withdrawal.

---

## Usage

### Domain Management

- **Register a Domain**
  ```
  (register (name (buff 64)) (metadata (buff 256)))
  ```
- **Renew a Domain**
  ```
  (renew (name (buff 64)))
  ```
- **Transfer a Domain**
  ```
  (transfer (name (buff 64)) (new-owner principal))
  ```
- **Set Domain Metadata**
  ```
  (set-metadata (name (buff 64)) (new-metadata (buff 256)))
  ```

### Subdomain Management

- **Create Subdomain**
  ```
  (create-subdomain (parent (buff 64)) (label (buff 64)) (metadata (buff 256)))
  ```
- **Transfer Subdomain**
  ```
  (transfer-subdomain (parent (buff 64)) (label (buff 64)) (new-owner principal))
  ```
- **Set Subdomain Metadata**
  ```
  (set-subdomain-metadata (parent (buff 64)) (label (buff 64)) (new-metadata (buff 256)))
  ```

### Marketplace

- **List Domain for Sale**
  ```
  (list-domain (name (buff 64)) (price uint))
  ```
- **Unlist Domain**
  ```
  (unlist-domain (name (buff 64)))
  ```
- **Buy Domain**
  ```
  (buy-domain (name (buff 64)))
  ```

### Auctions

- **Create Auction**
  ```
  (create-auction (name (buff 64)) (starting-price uint) (duration uint))
  ```
- **Place Bid**
  ```
  (place-bid (auction-id uint) (bid-amount uint))
  ```
- **Finalize Auction**
  ```
  (finalize-auction (auction-id uint))
  ```

### Admin Functions

- **Set Contract Owner**
  ```
  (set-contract-owner (new-owner principal))
  ```
- **Enable/Disable Marketplace**
  ```
  (set-marketplace-enabled (enabled bool))
  ```
- **Update Pricing**
  ```
  (update-pricing (length uint) (base-price uint) (multiplier uint))
  ```
- **Emergency Withdraw**
  ```
  (emergency-withdraw (amount uint))
  ```

---

## Read-Only Functions

- **Check Domain Availability**
  ```
  (is-available (name (buff 64)))
  ```
- **Get Domain Info**
  ```
  (get-domain (name (buff 64)))
  ```
- **Resolve Domain**
  ```
  (resolve (name (buff 64)))
  ```
- **Get Registration Price**
  ```
  (get-registration-price (name (buff 64)))
  ```
- **Get Marketplace/Auction Info**
  ```
  (get-domain-listing (name (buff 64)))
  (get-auction (auction-id uint))
  ```

---

## Constants

- **DOMAIN_SUFFIX:** `.stx`
- **DOMAIN_REGISTRATION_PERIOD:** ~1 year
- **GRACE_PERIOD:** ~1 week
- **BASE_REGISTRATION_FEE:** 0.5 STX (in micro-STX)
- **Marketplace/Auction Parameters:** Premium multipliers, fee percent, auction durations, etc.

---

## Error Codes

- `ERR_DOMAIN_UNAVAILABLE`, `ERR_DOMAIN_TOO_SHORT`, `ERR_DOMAIN_TOO_LONG`, `ERR_INVALID_METADATA`, `ERR_UNAUTHORIZED`, `ERR_INSUFFICIENT_FUNDS`, `ERR_DOMAIN_NOT_FOUND`, `ERR_SUBDOMAIN_NOT_FOUND`, `ERR_DOMAIN_EXPIRED`, `ERR_NOT_FOR_SALE`, `ERR_INVALID_PRICE`, `ERR_AUCTION_NOT_FOUND`, `ERR_AUCTION_ENDED`, `ERR_AUCTION_ACTIVE`, `ERR_BID_TOO_LOW`, `ERR_INVALID_AUCTION_DURATION`, `ERR_CANNOT_BID_OWN_AUCTION`

---

## License

This contract is provided for educational and demonstration purposes.  
Please review and audit before deploying to mainnet.

---

## Author

PinCore Smart Contract for Stacks Blockchain  
(c) 2025 abiyong-charles
