# Simple 3-Statement Model

Simple three-statement financial model.

```text
financial_model
├── income_statement
│   ├── gross_profit: revenue - cogs
│   │   ├── revenue: Initial value 1000.0, growing at 5% annually
│   │   └── cogs: 30% of revenue
│   ├── opex: Fixed 200.0 per month
│   ├── depreciation: 10% of beginning period ppe_net / 12 (straight line)
│   ├── tax: 20% of (gross_profit - opex - depreciation)
│   └── net_income: gross_profit - opex - depreciation - tax
├── cash_flow_statement
│   ├── operations
│   │   ├── net_income_add_back: Link to income_statement.net_income
│   │   ├── depreciation_add_back: Link to income_statement.depreciation
│   │   └── cf_ops: net_income + depreciation
│   ├── investing
│   │   ├── capex: 5% of revenue (outflow)
│   │   └── cf_invest: -capex
│   ├── financing
│   │   └── cf_finance: 0.0 (simplified, no debt/equity changes)
│   └── net_cash_change: cf_ops + cf_invest + cf_finance
└── balance_sheet
    ├── assets
    │   ├── cash: Previous cash + cash_flow_statement.net_cash_change
    │   ├── ppe_net: Previous ppe_net + capex - depreciation
    │   └── total_assets: cash + ppe_net
    ├── liabilities_and_equity
    │   ├── common_stock: Constant 5000.0
    │   ├── retained_earnings: Previous retained_earnings + income_statement.net_income
    │   └── total_liabilities_equity: common_stock + retained_earnings
    └── check
        └── balance_check: total_assets - total_liabilities_equity (should be 0)
```
