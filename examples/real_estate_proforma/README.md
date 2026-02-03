# Commercial Real Estate Pro Forma

This example illustrates a simple commercial real estate model for both a single property and a portfolio of properties. The executables print 120 monthly periods by default.

## `run_property`

Prints out a pro forma statement for a single fictitious property. The Orcaset model in this examples matches the Excel model in the folder.

## `run_portfolio`
Demonstrates portfolio-scale aggregation and model reusability by creating 1,000 properties from the same Orcaset model. The model uses the same default assumptions for each property so that it is easy to verify that the portfolio aggregates correctly. You can easily modify the number of properties changing the `num_properties` or add properties to the portfolio with different assumptions.

## Statement Structure

```text
Levered Cash Flow
├── Unlevered Cash Flow
│   ├── Net Operating Income
│   │   ├── Effective Gross Income
│   │   │   ├── Gross Potential Rent
│   │   │   │   ├── Base Rent
│   │   │   │   ├── Parking Income
│   │   │   │   ├── CAM Recoveries
│   │   │   │   └── Other Income
│   │   │   └── Vacancy & Credit Loss
│   │   └── Operating Expenses
│   │       ├── Property Taxes
│   │       ├── Insurance
│   │       ├── Utilities
│   │       ├── Repairs & Maintenance
│   │       ├── Property Management
│   │       ├── Janitorial
│   │       ├── Landscaping
│   │       └── Security
│   └── Capital Expenditures
│       ├── Capital Reserves
│       ├── Tenant Improvements
│       └── Leasing Commissions
└── Debt Service
    ├── Debt Balance
    ├── Interest Expense
    └── Amortization
```

## Key Concepts

- **Reusing Component Types**: The portfolio executable creates multiple properties by reusing the same property model component types.
- **Portfolio Aggregation**: The example efficiently aggregates across 1,000 properties to rapidly produce a portfolio-level statement. Users can easily navigate down to specific properties or line items within a property for detailed analysis.
