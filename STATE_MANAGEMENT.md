# State Management Guide

This document explains how shared state is intended to work in this app, how modules should interact with it, and how to avoid reintroducing the cross-page bugs that motivated the refactor.

## Goal

The app should have:

- one canonical source of truth for durable state
- explicit state transitions
- scoped invalidation when upstream state changes
- UI controls that reflect shared state, rather than secretly owning it

The practical outcome is:

- changing Design spec should not silently corrupt Models spec
- changing Models spec should invalidate stale formulas/results
- reset/load should restore both backend state and visible UI state
- downstream pages should not read stale or page-local values

## State Ownership

Durable state lives in `rv` and is initialized by `make_default_rv()` in [R/config.R](/Users/dipesh/Documents/AI/DataAnalysisApp/R/config.R).

State-transition helpers live in [R/app_state.R](/Users/dipesh/Documents/AI/DataAnalysisApp/R/app_state.R).

Broad ownership by domain:

- Data layer
  - `rv$data`
  - `rv$roles`
  - `rv$col_types`
  - `rv$transforms`
  - `rv$coding_values`
  - `rv$level_labels`

- Design spec
  - `rv$design_model_formula`
  - `rv$design_alias_formula`
  - `rv$alias_threshold`

- Model spec
  - `rv$model_active_response`
  - `rv$model_custom_formula`
  - `rv$model_max_way`
  - `rv$model_poly_degree`
  - `rv$model_include_covariates`
  - `rv$model_formula_covariates`
  - `rv$model_max_covariates_per_formula`
  - `rv$model_include_cov_fac`
  - `rv$model_include_blocks`
  - `rv$model_include_block_fac`
  - `rv$model_append_formulas`

- Generated formula state
  - `rv$formulas`
  - `rv$formula_aliases`
  - `rv$alias_labels`
  - `rv$inestimable_terms`
  - `rv$formula_gen`

- Analysis outputs
  - `rv$models`
  - `rv$model_errors`
  - `rv$model_notes`
  - `rv$vif_df`
  - `rv$excluded_obs`
  - `rv$prune_notes`

- Multiple comparisons
  - config:
    - `rv$mc_on`
    - `rv$mc_alpha`
    - `rv$mc_terms`
    - `rv$mc_methods`
  - outputs:
    - `rv$mc_results`

- Design-derived outputs
  - `rv$design_metadata`
  - `rv$sim_data`

- UI-only persisted state
  - `rv$selected_obs`
  - `rv$report_items`

## Core Rule

If a value matters across tabs, survives reset/load, or affects analysis, it must live in `rv`.

Module `input$...` values should be treated as:

- event sources
- temporary UI state
- a view over canonical state

They should not be the only owner of important values.

## Correct Write Pattern

Use this pattern for durable state:

1. User changes a control
2. Module observer writes the canonical value into `rv` using a setter/helper
3. If the change invalidates downstream outputs, call the relevant action/helper
4. UI sync functions update controls from `rv` during reset/load/programmatic changes

Example pattern:

```r
observeEvent(input$alias_full_formula, {
  if (isTRUE(rv$read_only)) return()
  set_design_model_formula(rv, input$alias_full_formula)
  apply_design_spec_change(rv)
}, ignoreInit = TRUE)
```

## Correct Read Pattern

Use this pattern for durable state:

- computations read from `rv`
- cross-module exports return `rv` values
- downstream modules do not read another module's widget state directly

Good:

```r
response <- rv$model_active_response
```

Bad:

```r
response <- input$active_response
```

Bad is acceptable only when the value is truly ephemeral and not part of shared state.

## Use the Action Helpers

The intended action layer is in [R/app_state.R](/Users/dipesh/Documents/AI/DataAnalysisApp/R/app_state.R).

### Invalidation helpers

Use these when you need scoped clearing:

- `invalidate_design_outputs(rv)`
- `invalidate_formula_outputs(rv)`
- `invalidate_model_outputs(rv)`
- `invalidate_mc_outputs(rv, full = TRUE/FALSE)`

### Action functions

Use these for upstream changes:

- `apply_data_change(rv)`
- `apply_role_change(rv)`
- `apply_design_spec_change(rv)`
- `apply_model_spec_change(rv)`
- `apply_generated_formulas(rv, ...)`
- `apply_fitted_models(rv, ...)`
- `apply_mc_config_change(rv)`

### Setter helpers

Use these for canonical spec fields:

- Design setters
  - `set_design_model_formula()`
  - `set_design_alias_formula()`
  - `set_alias_threshold()`

- Model setters
  - `set_model_active_response()`
  - `set_model_custom_formula()`
  - `set_model_max_way()`
  - `set_model_poly_degree()`
  - `set_model_include_covariates()`
  - `set_model_formula_covariates()`
  - `set_model_max_covariates_per_formula()`
  - `set_model_include_cov_fac()`
  - `set_model_include_blocks()`
  - `set_model_include_block_fac()`
  - `set_model_append_formulas()`

## Invalidation Rules

These are the intended rules.

- Data load/generation invalidates:
  - Design outputs
  - generated formulas
  - fitted models
  - MC outputs/config
  - UI selection state

- Role/type/transform changes invalidate:
  - Design outputs
  - generated formulas
  - fitted models
  - MC outputs/config
  - UI selection state

- Analysis mode changes invalidate:
  - generated formulas
  - fitted models
  - MC outputs/config

- Design spec changes invalidate:
  - Design-derived outputs only
  - they do not mutate Models spec unless the user explicitly pushes something to Models

- Model spec changes invalidate:
  - generated formulas
  - fitted models
  - MC outputs/config
  - they do not mutate Design spec

- Formula generation invalidates:
  - fitted models
  - MC results

- Model fitting invalidates:
  - MC results only

- MC config changes invalidate:
  - MC results only

If code changes behavior outside these rules, it should be treated as suspicious and reviewed explicitly.

## What Not To Do

Do not:

- write directly to multiple `rv$...` fields inline when an action helper should be used
- read cross-tab state from `input$...` if there is already a canonical `rv` field
- update a UI widget and assume that means shared state is updated
- update shared state and assume the widget will visually reset itself
- duplicate reset logic across modules

Bad example:

```r
rv$formulas <- formulas
rv$formula_aliases <- aliases
rv$models <- list()
rv$mc_results <- list()
```

Prefer:

```r
apply_generated_formulas(rv, formulas, aliases = aliases)
```

## UI Sync Contract

Each module that owns durable controls should expose a sync function.

Current examples:

- Models:
  - `sync_ui_from_rv()`
  - `reset_ui()`

- Design:
  - `sync_ui_from_rv()`
  - `reset_ui()`

Use these for:

- session load
- full reset
- programmatic cross-module updates

Do not duplicate `updateTextInput()` / `updateSelectInput()` logic all over the app if the module already exposes a sync/reset API.

## Save / Load

Anything the user expects to survive save/load should come from canonical `rv` state.

That means:

- save should serialize `rv`
- load should restore `rv` through setters/actions where validation matters
- after load, modules should sync UI from `rv`

If a value is only in `input$...`, it is probably not save/load safe.

## When Adding New State

When introducing a new piece of state, answer these questions:

1. Is it durable or ephemeral?
2. Which domain owns it?
3. What upstream changes invalidate it?
4. Does it need a setter helper?
5. Does it need a sync/reset UI path?
6. Does it need to be included in save/load?
7. What tests prove the invalidation contract?

If you cannot answer these, the state change is probably underspecified.

## Minimum Checklist For New Code

Before merging a stateful change, verify:

- a canonical `rv` field exists for durable state
- a helper/setter exists if normalization is needed
- invalidation is explicit
- downstream consumers read from `rv`
- reset/load keep UI and `rv` in sync
- tests cover at least the helper path
- app-level tests cover any important UI-sync path

## Recommended Test Split

- `testthat`:
  - helper logic
  - invalidation helpers
  - save/load defaulting
  - action-function behavior

- `shinytest2`:
  - reset/load visible UI sync
  - cross-module propagation
  - a few critical stale-state regressions

Do not try to test every state transition in the browser. Keep most of the contract at the helper layer.

## Current Known Caveat

Some browser-backed tests around select-like controls can be flaky under `shinytest2` even when the visual UI looks correct manually. Treat those as test-harness issues first, but do not weaken the state model to satisfy flaky browser timing.

The right approach is:

- keep canonical state correct
- keep UI sync explicit
- tighten browser tests with waiting/polling where needed

## Short Version

If you only remember four rules, use these:

1. Durable state lives in `rv`, not in `input$...`.
2. Upstream changes go through action helpers.
3. Downstream computations read canonical `rv` state.
4. Reset/load must sync both backend state and visible UI.
