# EP Aging Curve Explorer (R / Shiny)

A population-level hitter aging curve, built with the classic **delta method**
(year-over-year change among players who qualified in both consecutive
seasons), with an optional overlay of any individual player's own career
trajectory against that baseline.

Uses Sean Lahman's public-domain MLB database via the `Lahman` CRAN package —
the same source behind the EP College Pipeline Intelligence work in Python,
now in R.

## Run it

```r
install.packages(c("shiny", "Lahman", "dplyr", "tidyr", "ggplot2", "DT"))
shiny::runApp("app.R")
```

## What this demonstrates

- **R + Shiny**, end to end (reactive UI, `selectizeInput` server-side search
  over 20k+ players, `ggplot2` + `DT` output)
- **A named statistical method** (delta method for aging curves — Tango,
  2006) rather than an ad hoc smoother, with the anchor-age / forward-backward
  cumulative-sum logic implemented from scratch
- **EP's standing methodology habit**: every output is labeled CONFIRMED vs.
  a stated limitation (age is `season year − birth year`, not exact age —
  called out in-app, not hidden)
- A parameterized pipeline (metric, minimum PA, year range, player) instead
  of a one-off script — the same "reusable tool, not a single chart" bar used
  across the EP repos

## Possible next steps

- Swap the birth-year age approximation for `birthDate`-based exact age
  (Lahman's `People` table has day-level birth dates)
- Add a second delta-method line split by era (e.g., pre/post-2015) to show
  how aging curves have shifted
- Port the same pattern to a pitching metric (K%, BB%, velocity if a
  Statcast-linked dataset is available)
