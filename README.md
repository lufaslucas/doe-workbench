# DoE Workbench

Interactive Shiny application for **Design of Experiments (DoE)** and **regression analysis**. Built for statisticians and engineers who need to design experiments, build models, and communicate results.

## Installation

```r
# Install from GitHub
remotes::install_github("lufaslucas/DoEWorkbench")
```

## Usage

```r
# Launch the app
DoEWorkbench::run_doe_workbench()

# Or specify a port
DoEWorkbench::run_doe_workbench(port = 8080)

# Shorthand alias also works
DoEWorkbench::run_app()
```

## Features

- **Data**: Upload CSV/Excel or generate designs (factorial, fractional factorial, response surface, Latin square, space-filling)
- **Assign Roles**: Set column roles (Response, Factor, Covariate, Block), types, and transformations (centring, coding)
- **Design**: Alias structure analysis, power analysis, design balance checks, simulation with confounding
- **Explore**: Scatterplot matrix, box plots, line plots, parallel coordinates with linked brushing
- **Models**: Formula builder, model fitting, backward elimination, multiple comparisons (Student, Dunnett, Tukey)
- **Results**: ANOVA, coefficients, effects plots, contour/surface plots, profiler, residual diagnostics, robustness checks
- **Report**: Export analysis as HTML/Word/PDF

## Requirements

- R >= 4.1.0
- Key dependencies: shiny, bslib, plotly, ggplot2, car, emmeans

Optional (for design generation): FrF2, DoE.base, rsm, AlgDesign, lhs, MaxPro, skpr

## Session Management

Sessions can be saved and loaded as `.rds` files. Use **Finalize** to create a password-protected read-only copy for project consumption - recipients can explore results but cannot modify data or models.
