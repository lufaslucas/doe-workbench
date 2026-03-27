# Test Session Log — 2026-03-27

## Session Start
- App: DoE Workbench (http://localhost:4564)
- Dataset tested: Fuel Economy (uploaded CSV) in Comparative (ANOVA) mode
- Started: monitoring active

---

## Summary

### Bugs (error) — crashes, error messages, broken functionality
| # | Location | Description | Status |
|---|----------|-------------|--------|
| B1 | Data > Upload Dataset | Example datasets fail for specific datasets — RCBD works after refresh, CCD does not load, Latin Square causes screen grey-out. Dataset-specific failures in in-memory generation code. | OPEN |
| B2 | Explore > Needle Plot | Displays `Error: [object Object]` — caused by `geom_segment` using numeric x on a factor axis for the grand mean reference line, plus faceting duplicated data (global `aggregate` didn't include facet variable). **Fixed**: extracted `build_needle_plot()`, replaced with `geom_hline` + `ave()` for per-panel group means. Test: `tests/testthat/explore/test_needle_plot.R` (8 tests, 17 expectations). | FIXED |
| B3 | Models > Append Formula | Append + Run Selected Models greys out screen; custom formula lost instead of added to existing fitted models. Workflow: run models → add custom formula → deselect all but custom → Append → Run → grey screen. | OPEN |
| B5 | Results > ANOVA | Table shows "No matching records found" (0 of 0 entries filtered from 4) when model set includes a one-way ANOVA (single factor, e.g. `~ Treatment`). Staleness triggered by the simpler model structure. Works when that model excluded. | OPEN |
| B9 | Results > Profiler | `Error: The length of the widths argument must be equal to the number of columns` (appears twice). Column widths vector length doesn't match variable panel count. | OPEN |

### Bugs (non-error) — incorrect behaviour, no error message
| # | Location | Description | Status |
|---|----------|-------------|--------|
| B4 | Models > Alias Handling | "Combine" for confounded terms doesn't update formula list or relabel parameters. Combined label appears in Effects dropdown but needs refinement: only combine labels for fully aliased (1:1) terms; correlated (non-1:1) terms need distinct labelling. | OPEN |
| B6 | Results > Coefficients | Warning says "highlighted rows show aliased pairs" but no amber/highlight applied to rows. | OPEN |
| B7 | Results > Coefficients > Guide | Interpretation text may not cover all variable type x transform combinations. Needs audit of `coef_interpretation_ui`. | OPEN |
| B8 | Results > Effects > Side by Side | Plots recreated independently — effects plot differs from Partial Effects tab. Should reuse the exact same plots, just arranged side by side. | OPEN |
| B10 | Results > Residuals > Standard Diagnostics | Linked point selection not working — selecting a point in one diagnostic plot doesn't highlight in others. `rv$selected_obs` not wired to residual plots. | OPEN |
| B11 | Results > Residuals > vs Model Terms | Colour-by ignored when colour variable matches x-axis variable. "Residuals vs Treatment" all same colour despite colour-by = Treatment. "Residuals vs Date.Block" colours correctly. | OPEN |
| B12 | Results > Residuals > Outliers | Internal/External residual type toggle doesn't update plot. Stays on "Internally Studentised" regardless of selection. | OPEN |
| B13 | Results > Residuals > Outliers | Row selection in outlier table causes lag cascade (all plotly re-renders). **Blocks multi-row selection** — can't select 2+ outliers for refit because re-render from first selection interferes. Needs debouncing or proxy updates. | OPEN |
| B14 | Results > Lack of Fit | Table permanently blank after deselecting all models — reselecting (even Select All) doesn't recover. Reactive chain stuck after empty state. | OPEN |
| B15 | Results > Residuals > Outliers > Refit | Iterative outlier removal may not carry forward cumulative exclusion list (or may just be mislabelled). Need to verify n across original and iterative models. | OPEN |

### Features — new functionality or enhancements
| # | Location | Description | Status |
|---|----------|-------------|--------|
| F1 | Results > ANOVA > Type I | Should run single model with user-defined term ordering (sequential SS are order-dependent). Drag-to-reorder UI. | PENDING |
| F2 | Results > Lack of Fit | Table needs p-value colouring and note highlighting, consistent with other tables. | PENDING |
| F3 | Results > Effects > Leverage | One line per model with model-coloured traces. Always faceted view. Remove "panel view by" option. | PENDING |
| F4 | Results > Residuals | Default "Colour by" to Treatment (`.treatment`) in Comparative mode. Currently requires manual selection. | PENDING |
| F5 | Results > Model checkboxes | Laggy with many models — need faster bulk controls (shift-click range, deferred apply, or grouping). | PENDING |
| F6 | Results > Model grouping | Group models by lineage — original + its outlier-removal refits as collapsible group. Select/deselect whole group or individuals. | PENDING |

---

## What's Left to Test

### Example datasets (automated testing in progress)
| Dataset | Status |
|---------|--------|
| RCBD (2 factors, blocks, covariate) | PASS (after browser refresh) |
| Latin Square (4x4, 2 factors) | FAIL (grey screen) |
| Mediator Trap (covariate is a response) | NOT TESTED |
| Block-Covariate Collinearity | NOT TESTED |
| 2^4-1 Fractional Factorial | NOT TESTED |
| CCD (Central Composite Design) | FAIL (no data loaded) |
| Historical MLR (confounded) | NOT TESTED |
| Unbalanced 1-way (unequal n) | NOT TESTED |

### MLR (Regression) workflow — NOT STARTED
User plans to repeat the full workflow in Regression (MLR) mode:
- [ ] Load MLR-appropriate dataset (Historical MLR or CCD)
- [ ] Assign roles in regression mode
- [ ] Build polynomial/interaction formulas
- [ ] Run models
- [ ] Check ANOVA, Coefficients, Effects, Residuals, Profiler
- [ ] Test contour/surface plots (need 2+ numeric predictors)
- [ ] Test outlier removal in MLR context

### Areas not yet exercised in this session
- [ ] Design tab (Generate Design, Balance, Alias Structure)
- [ ] Report tab (Custom report builder)
- [ ] Save/Load state
- [ ] Finalize (read-only mode)
- [ ] Explore tab (beyond Needle Plot) — Boxplot, Scatterplot Matrix, Parallel Coordinates
- [ ] Multiple Comparisons (MC) tab in detail
- [ ] Robustness tab (7 sub-tabs)

---

## Detailed Event Log

### [Check 1] — Data tab (Upload Dataset)
- **Page state**: Data tab > Upload Dataset sub-tab. No data loaded.
- **Console errors**: none
- **Network failures**: none
- **Screenshot**: ss_6013z2nep

### [Check 2-3] — Data tab (Upload Dataset)
- **Page state**: No change.
- **Console errors**: AdBlock extension error only (not app-related)
- **Network failures**: none

### [B1] — Data > Example datasets — BUG
- **Screenshots**: ss_1765v4sin, ss_0775xo67z

### [B2] — Explore > Needle Plot — BUG
- **Screenshot**: ss_6944xk9xf

### [B3] — Models > Append Formula — BUG
- **Screenshot**: ss_6081qhbu4

### [B4] — Models > Alias Handling — BUG (non-error)
- Combined label visible in Effects dropdown but formula list not updated.

### [B5] — Results > ANOVA — BUG
- **Screenshot**: ss_23718ou68

### [B6] — Results > Coefficients > Alias highlighting — BUG (non-error)
- **Screenshot**: ss_6327hwjih

### [B8] — Results > Effects > Side by Side — BUG (non-error)
- **Screenshots**: ss_0940az761

### [B9] — Results > Profiler — BUG
- **Screenshot**: ss_1435tfz4r

### [B10] — Results > Residuals > Linked Selection — BUG (non-error)
- **Screenshot**: ss_3162q3ptz

### [B11] — Results > Residuals > Colour-by — BUG (non-error)
- **Screenshot**: ss_425248gkz

### [B12] — Results > Residuals > Type toggle — BUG (non-error)
- **Screenshot**: ss_917197qm5

### [B13] — Results > Residuals > Outlier table lag — BUG (non-error)
- Blocks multi-row outlier selection.

### [B14] — Results > Lack of Fit > Deselect recovery — BUG (non-error)
- **Screenshot**: ss_0783bwl14

### [B15] — Results > Outlier refit cumulative exclusion — BUG (non-error)
- Needs data point count verification.
