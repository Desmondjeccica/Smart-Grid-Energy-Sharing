# Smart Grid Energy Sharing Contract

A decentralized energy sharing platform built on Stacks blockchain that enables peer-to-peer energy trading within neighborhoods.

## Features

- Register as energy producer or consumer
- Set and update energy availability and prices
- Purchase energy directly from producers
- Track energy transactions and settlements
- Automatic STX-based payments
- Built-in safety checks and limits

## Usage

### For Producers

1. Register as producer:
```clarity
(contract-call? .energy-sharing register-as-producer u100 u5)
```

2. Update available energy:
```clarity
(contract-call? .energy-sharing update-available-energy u150)
```

3. Update energy price:
```clarity
(contract-call? .energy-sharing update-energy-price u6)
```

### For Consumers

1. Register as consumer:
```clarity
(contract-call? .energy-sharing register-as-consumer u50)
```

2. Buy energy:
```clarity
(contract-call? .energy-sharing buy-energy 'ST1PQHQKV0RJXZFY1DGX8MNSNYVE3VGZJSRTPGZGM u20)
```

### Query Functions

- Get producer details:
```clarity
(contract-call? .energy-sharing get-producer-details 'ST1PQHQKV0RJXZFY1DGX8MNSNYVE3VGZJSRTPGZGM)
```

- Get consumer details:
```clarity
(contract-call? .energy-sharing get-consumer-details 'ST1PQHQKV0RJXZFY1DGX8MNSNYVE3VGZJSRTPGZGM)
```

- Get total shared energy:
```clarity
(contract-call? .energy-sharing get-total-energy-shared)
```

## Limits

- Minimum energy transaction: 1 unit
- Maximum energy transaction: 1000 units
- Price range: 1-100 STX per unit
