# doe-workbench

Interactive Shiny application for **Design of Experiments (DoE)** and **regression analysis**. Built for statisticians and engineers who need to design experiments, build models, and communicate results.

## Installation

```r
# Install the latest development branch
remotes::install_github("lufaslucas/doe-workbench@round2-enhancements")

# Or install from main (stable)
remotes::install_github("lufaslucas/doe-workbench")
```

## Usage

```r
# Launch the app
doe.workbench::run_doe_workbench()

# Or specify a port
doe.workbench::run_doe_workbench(port = 8080)

# Shorthand alias also works
doe.workbench::run_app()
```

## Features

### Data & Design
- **Import**: CSV, Excel, or clipboard; built-in example datasets (CRD, RCBD, Latin Square, fuel economy)
- **Generate**: Factorial, fractional factorial, CCD, Box-Behnken, Latin square, BIB, LHS, D-optimal, custom designs via agricolae, FrF2, rsm, AlgDesign, lhs, MaxPro, skpr
- **Assign Roles**: Set column roles (Response, Factor, Covariate, Block), types, and transformations (centring, coding)

### Design Assessment
- **Alias structure**: Pre-model detection of confounded terms with push-to-Models workflow
- **Correlation map**: Model and model+alias correlation heatmaps with orthogonal contrast evaluation
- **Power analysis**: Per-term power for all model terms (factors, blocks, covariates, interactions, polynomials)
- **Design balance**: Replication, run-order, and carryover checks with covariate generation
- **Simulation**: Simulate response data with user-defined effects, confounding analysis, and add simulated Y to dataset

### Explore
- Distribution plots, by-factor box/violin plots, covariate scatter, block plots, needle plots, parallel coordinates
- JMP-style linked brushing across all plots (lasso, box select, click)
- Treatment-first ordering in comparative mode

### Models
- Formula builder with staged term addition, append mode, and custom formula editor
- Backward elimination with pruning of selected models
- Multiple comparisons: Student t, Dunnett, Tukey, Max-t with per-term controls and auto-recompute

### Results
- **ANOVA**: Type III with VIF collinearity highlighting, per-term summary, optional columns (df, SS, MS, F)
- **Coefficients**: Interval plot, alias indicators, dynamic interpretation guide, VIF highlighting
- **Effects**: Partial effects (LS means/partial regression), leverage plots, side-by-side view
- **Contour/Surface**: 2D contour and 3D surface plots for response surface models
- **Profiler**: JMP-style prediction profiler with aligned subplot widths, CI/PI bands, shared y-scales
- **Residuals**: vs fitted, vs terms, vs omitted, QQ, histogram, outlier identification with refit-without-outliers
- **Robustness**: Model review, LS mean vs limit, pairwise ordering/significance, covariate mediation, outlier influence, coefficient robustness
- **Summary Metrics**: R2, Adj R2, RMSE, PRESS RMSE, AIC, BIC comparison across models

### Report
- Custom report builder: add items with comments, download as HTML/Word/PDF

## Design Visualisation
- Scatterplot matrix with Pearson r and Cramer's V
- 2D design map (scatter or heatmap) with faceting, colour-by, shape-by-treatment
- 3D interactive design space with categorical axis labels, jitter, and settings-driven colour palette
- Latin square auto-detection with col-vs-row heatmap default

## Requirements

- R >= 4.1.0
- Key dependencies: shiny, bslib, plotly, ggplot2, car, emmeans

Optional (for design generation): FrF2, DoE.base, rsm, AlgDesign, lhs, MaxPro, skpr

## Session Management

Sessions can be saved and loaded as `.rds` files. Use **Finalize** to create a password-protected read-only copy for project consumption - recipients can explore results but cannot modify data or models.

## Development

For live development, the app auto-detects source mode (DESCRIPTION + R/ present) and uses `pkgload::load_all()` instead of the installed package. Just restart the app to pick up code changes.

```r
# Run from source (no install needed)
shiny::runApp("inst/app", port = 4564)
```
