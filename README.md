# 🌾 Smart Crop Insurance Contract 

A decentralized crop insurance platform that automatically compensates farmers based on weather data from trusted oracles.

## 🎯 Features

- 🔒 Secure policy purchase system
- 🌡️ Oracle-based weather data integration
- 💸 Automatic claim processing
- ⚡ Instant compensation payouts
- 🔄 Policy management system

## 📋 Contract Functions

### For Farmers
- `purchase-insurance`: Purchase a new insurance policy
- `file-claim`: Submit an insurance claim
- `cancel-policy`: Cancel an active policy
- `get-policy`: View policy details

### For Oracle
- `submit-weather-data`: Submit temperature and rainfall data

## 🚀 Getting Started

1. Deploy the contract using Clarinet:
```bash
clarinet contract deploy Smart-Crop-Insurance
```

2. Purchase insurance by calling:
```bash
clarinet contract call purchase-insurance amount duration
```

3. Oracle submits weather data:
```bash
clarinet contract call submit-weather-data temperature rainfall
```

## 💡 Usage Example

1. Farmer purchases insurance for 10,000 STX:
```bash
clarinet contract call purchase-insurance u10000 u1000
```

2. Oracle reports adverse weather:
```bash
clarinet contract call submit-weather-data u40 u50
```

3. Claims are processed automatically if conditions are met

## ⚠️ Requirements

- Clarinet 1.0.0 or higher
- Stacks blockchain node
- STX tokens for policy purchase

## 🔗 Contract Details

- Minimum insurance amount: 1,000 STX
- Maximum insurance amount: 100,000 STX
- Compensation multiplier: 100%
- Temperature threshold: 35°C
- Minimum rainfall: 100mm
```

