# PinCore: Decentralized Domain Registry & Marketplace

PinCore is a decentralized domain name system and marketplace built on the Stacks blockchain. It enables users to register, manage, trade, and auction blockchain-based domain names in a secure, trustless, and permissionless way.

---

## Features

- **Domain Registration:**  
  Register unique `.pincore` domains with **dynamic pricing** based on domain length and demand.
- **Domain Renewal & Grace Period:**  
  Renew domains before expiry or during a **grace period** (only the previous owner can renew during grace).
- **Domain Transfer:**  
  Transfer domain ownership to another user.
- **Subdomain Management:**  
  Create, transfer, and update subdomains under owned domains, with parent domain validation.
- **Marketplace:**  
  List domains for sale, buy listed domains, and pay/collect marketplace fees.
- **Auctions:**  
  Auction domains, place bids (with escrow and automatic refunds), and finalize auctions.
- **Demand Tracking:**  
  Tracks registration attempts per domain for analytics and dynamic pricing.
- **Domain History Tracking:**  
  Logs actions (register, transfer, renew, etc.) with block height and actor for transparency and analytics.
- **Marketplace Authorization:**  
  Only an authorized marketplace contract can perform marketplace transfers, improving security.
- **Trait-Based Modularity:**  
  Marketplace and registry interact via a trait interface, allowing modular upgrades and integration.
- **Marketplace Fee:**  
  2.5% fee is charged on every marketplace sale or auction, collected by the marketplace contract owner.
- **Marketplace Enable/Disable:**  
  Owner can pause or resume the marketplace for maintenance or emergencies.
- **Comprehensive Validation & Error Handling:**  
  Enhanced validation for all inputs and extensive error codes for robust security and user feedback.
- **Admin Controls:**  
  Change contract owners, set marketplace contract, enable/disable marketplace, and emergency fund withdrawal.
- **Security:**  
  Strict validation and authorization checks for all sensitive actions. All STX transfers are handled with proper checks and error handling.

---

## Smart Contracts

### 1. pincore-registry.clar
Handles core domain logic:
- Registration, renewal, transfer, and metadata management
- Subdomain creation and management
- Dynamic pricing and demand tracking
- Domain and subdomain validation
- Domain history and analytics
- Marketplace contract authorization (only the authorized marketplace can call marketplace-transfer)
- Admin controls for contract owner and emergency withdrawal

### 2. pincore-marketplace.clar
Handles trading and auctions:
- List, unlist, and buy domains (with fee deduction)
- Create and finalize auctions (with bid escrow, refund, and extension logic)
- Place bids with automatic refunds for outbid users
- Trait-based integration with registry for ownership checks and transfers
- Marketplace fee collection and admin controls
- Enable/disable marketplace for maintenance or emergencies

---

## Usage

### Domain Registration & Management

```clarity
;; Register a domain
(register (name (buff 64)) (metadata (buff 256)))

;; Renew a domain
(renew (name (buff 64)))

;; Transfer a domain
(transfer (name (buff 64)) (new-owner principal))

;; Set domain metadata
(set-metadata (name (buff 64)) (new-metadata (buff 256)))
```

### Subdomain Management

```clarity
;; Create a subdomain
(create-subdomain (parent (buff 64)) (label (buff 64)) (metadata (buff 256)))

;; Transfer a subdomain
(transfer-subdomain (parent (buff 64)) (label (buff 64)) (new-owner principal))

;; Set subdomain metadata
(set-subdomain-metadata (parent (buff 64)) (label (buff 64)) (new-metadata (buff 256)))
```

### Marketplace

```clarity
;; List a domain for sale
(list-domain-with-registry (name (buff 64)) (price uint) (registry <registry-trait>))

;; Buy a listed domain
(buy-domain-with-registry (name (buff 64)) (registry <registry-trait>))

;; Unlist a domain
(unlist-domain (name (buff 64)))
```

### Auctions

```clarity
;; Create an auction
(create-auction-with-registry (name (buff 64)) (starting-price uint) (duration uint) (registry <registry-trait>))

;; Place a bid
(place-bid (auction-id uint) (bid-amount uint))

;; Finalize an auction
(finalize-auction-with-registry (auction-id uint) (registry <registry-trait>))
```

### Admin Functions

```clarity
;; Set contract owner
(set-contract-owner (new-owner principal))

;; Set marketplace contract (registry only)
(set-marketplace-contract (marketplace-addr principal))

;; Enable/disable marketplace (marketplace only)
(set-marketplace-enabled (enabled bool))

;; Emergency withdraw
(emergency-withdraw (amount uint))
```

---

## Enhancements Over Basic Implementations

- **Dynamic Pricing:**  
  Registration fees increase for shorter or high-demand domains.
- **Grace Period:**  
  Protects expired domains from immediate sniping.
- **Demand & History Tracking:**  
  Enables analytics and fairer pricing.
- **Comprehensive Validation:**  
  Prevents invalid names, metadata, prices, and unauthorized actions.
- **Escrow & Refunds:**  
  Auction bids are escrowed and previous bidders are automatically refunded.
- **Trait-Based Modularity:**  
  Marketplace and registry are decoupled, allowing upgrades and integration with other contracts.
- **Marketplace Authorization:**  
  Only the authorized marketplace can transfer domains on behalf of users.
- **Admin Controls:**  
  Owners can update key settings and withdraw funds securely.
- **Security & Transparency:**  
  All sensitive actions are validated and logged for auditability.

---

## Marketplace Fees

- **2.5% fee** is charged on every marketplace sale or auction, collected by the marketplace contract owner.

---

## Security

- All sensitive actions require strict validation of sender, ownership, and domain state.
- Marketplace and registry contracts are modular and interact via defined traits.
- All STX transfers are handled with proper checks and error handling.

---

## License

This project is provided for educational and demonstration purposes.  
**Review and audit before deploying to mainnet.**

---

## Author

(c) 2025 abiyong-charles

---

## Contact

For questions or support, open an issue or contact the repository owner.
