# NCAA Tournament First Round Spread Analysis (2014–2025)

An R-based analysis investigating which seeds in the NCAA Men's Basketball Tournament have covered first-round point spreads at the highest rate over the past 11 tournaments

(No Tournament in 2020 - COVID)

## Overview

Every year, one March weekend becomes the college basketball fan's ultimate dream: hours of non-stop postseason action across 4 major networks and cities across the country that all make up the
NCAA Tournament's opening round.

This project takes a data-driven approach to identifying which seeds have been the most profitable against the spread (ATS) over the tournament's opening weekend.

**Key Finding:** 11 seeds have covered the first-round spread at a higher rate than any other seed from 2014 to 2025.

## Data

- **Source:** Manually collected from publicly available historical tournament results and point spread data
- **Scope:** NCAA Men's Basketball Tournament, First Round (Round of 64)
- **Timeframe:** 2014–2025 (11 tournaments - No Tournament in 2020 due to COVID)
- **Variables:** Year, Seed, Team, Spread, Result

## Tech Stack

| Tool | Purpose |
|------|---------|
| **R** | Core language |
| **tidyverse / dplyr** | Data wrangling, transformation, and aggregation |
| **gt / showtext / scales** | Data visualization |

## Analysis

The project answers one central question: **Which seeds have covered the first-round spread most consistently?**

Steps included:
1. Structuring and cleaning the manually collected dataset
2. Calculating cover/non-cover outcomes for each game
3. Aggregating cover percentages by seed across all 11 tournaments
4. Visualizing seed-level ATS performance

## Key Takeaways

- **11 seeds** led all seeds in first-round spread cover rate over this span
- Results reinforce importance of certain tournament dynamics (e.g., play-in momentum for 11 seeds, difficulty in properly seeding unexpected high major conference champions, or mid major at large qualifiers)
