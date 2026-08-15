# Freshman Financial Shock after the Great Recession

This project analyzes whether state-level recession severity affected financial anxiety and labor supply plans among college freshmen entering between 2003–2010.

## Project Structure

```
code/
├── freshmansetup.R      Data merging and variable construction
└── freshmananalysis.R   Regression analysis and output

results/
└── freshmanpaper.pdf    Final paper with findings
```

## Overview

This project uses the HERI Freshman Survey merged with state-level unemployment data to estimate the causal impact of the 2008 Great Recession on:
1. **Financial Anxiety**: Whether students report major concern about affording college
2. **Work Expectations**: Whether students plan to work to help pay for college

The difference-in-differences design with continuous state-level recession exposure (2007–2009 unemployment change) found **null results**: state recession severity had no significant differential effect on either outcome.

## Key Findings

- Financial anxiety coefficient: β₃ = −0.0011 (p = 0.421)
- Work expectation coefficient: β₃ = +0.0007 (p = 0.735)
- Results robust across five functional forms, event-study specifications, and demographic subgroups
- No heterogeneous effects by parental income or first-generation status

## Data

- **HERI Freshman Survey** (2003–2007, 2009–2010)
- **BLS unemployment rates** (49 states + DC)
- Final sample: ~2.5M student-year observations

## To Run

1. Run `code/freshmansetup.R` to merge and construct variables
2. Run `code/freshmananalysis.R` to estimate models and generate tables/figures
3. See `results/freshmanpaper.pdf` for full analysis

## Author

Joseph [Last Name]
