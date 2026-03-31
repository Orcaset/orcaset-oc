# Three-Statement Financial Model

A simple three-statement model linking income, balance sheet, and cash flow.

## Statement Structure

```text
Financial Model
├── Income Statement
│   ├── Gross Profit
│   │   ├── Revenue
│   │   └── COGS
│   ├── Opex
│   ├── Depreciation
│   ├── Tax
│   └── Net Income
├── Cash Flow Statement
│   ├── Operations
│   │   ├── Net Income
│   │   └── Depreciation Add Back
│   ├── Investing
│   │   └── Capex
│   ├── CF Financing
│   └── Net Cash Change
└── Balance Sheet
    ├── Assets
    │   ├── Cash
    │   └── PPE Net
    ├── Liabilities & Equity
    │   ├── Common Stock
    │   └── Retained Earnings
    └── Check
        └── Balance Check
```

## Line Items

| Line Item                 | Logic                                                                     |
| ------------------------- | ------------------------------------------------------------------------- |
| **Revenue**               | Starts at $1,000/month, grows 5% annually (Actual/360 day count).         |
| **COGS**                  | Revenue x -0.30 (30% of revenue).                                         |
| **Gross Profit**          | Revenue + COGS.                                                           |
| **Opex**                  | Constant -$200/month.                                                     |
| **Depreciation**          | Prior period's PPE Net x (10% / 12).                                      |
| **Tax**                   | (Gross Profit + Opex + Depreciation) x -0.20 (20% tax rate).              |
| **Net Income**            | Earnings before tax + Tax.                                                |
| **Depreciation Add Back** | Reverses the non-cash depreciation charge.                                |
| **CF Operations**         | Net Income + Depreciation Add Back.                                       |
| **Capex**                 | Revenue x -0.05 (5% of revenue).                                          |
| **CF Financing**          | Constant $0.                                                              |
| **Net Cash Change**       | CF Operations + CF Investing + CF Financing.                              |
| **Cash**                  | Starts at $1,000. Accumulates Net Cash Change.                            |
| **PPE Net**               | Starts at $10,000. Increases by Capex, decreases by Depreciation.         |
| **Common Stock**          | Constant $5,000.                                                          |
| **Retained Earnings**     | Starts at $6,000 (Initial Assets - Common Stock). Accumulates Net Income. |
| **Balance Check**         | Total Assets - Total Liabilities & Equity. Should be $0.                  |
