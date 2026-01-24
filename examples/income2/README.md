# Income Model 2

This example demonstrates a more complex income model with multiple line items nested into groups.

## Statement Structure

```text
Income Statement
├── Cost of Revenue
│   ├── Revenue
│   │   ├── Recurring
│   │   └── Non-Recurring
│   ├── Recurring (Cost)
│   └── Non-Recurring (Cost)
├── Gross Profit
├── Operating Expenses
│   └── Admin
└── Operating Income
```

## Line Items

| Line Item | Logic |
|-----------|-------|
| **Revenue / Recurring** | Starts at 10,000, grows 5% annually. |
| **Revenue / Non-Recurring** | Random walk with drift. Starts at 2,000, drift 50, volatility 500. |
| **Cost of Revenue / Recurring** | `Recurring Revenue * -0.30` (30% expense margin). |
| **Cost of Revenue / Non-Recurring** | `Non-Recurring Revenue * -0.40` (40% expense margin). |
| **Gross Profit** | `Total Revenue + Total Cost of Revenue`. |
| **Opex / Admin** | Starts at -1,500, grows 3% annually. |
| **Operating Income** | `Gross Profit + Admin`. |

## Key Concepts

- **Hierarchical Grouping**: Demonstrates nesting groups (Revenue inside Cost of Revenue section).
