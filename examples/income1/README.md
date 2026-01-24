# Income Model 1

This example demonstrates a simple income statement with mutual dependencies between Revenue and Operating Expense lines.

## Statement Structure

```text
Income Statement
├── Revenue
│   ├── Software
│   └── Services
├── Operating Expenses
│   ├── COGS
│   └── Admin
└── Income
```

## Line Items

| Line Item | Logic |
|-----------|-------|
| **Revenue / Software** | Starts at 1,000, grows 10% annually. |
| **Revenue / Services** | First month 500. Subsequently calculated as `Total Opex * -0.5`. (Depends on Opex). |
| **Opex / COGS** | `Software Revenue * -0.3` (30% of Software Revenue). |
| **Opex / Admin** | Starts at -100, grows 5% annually. |
| **Income** | `Total Revenue + Total Opex`. |

## Key Concepts

- **Circular Dependency**: Services revenue depends on total operating expenses, while COGS depends on software revenue. This is handled using `lazy` evaluation and mutually recursive modules.
