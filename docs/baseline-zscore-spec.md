# Baseline Z-Score API Specification

## Overview

This spec describes changes to the stats API to support z-score calculation for all observed data periods, including pre-baseline, baseline, and post-baseline periods.

## Current Behavior

**Parameters:**
- `y` = observed data (typically just baseline values)
- `b` = baseline length (baseline = `y[1:b]`)
- `h` = forecast horizon

**Issues:**
1. Frontend only sends baseline data in `y`, not full observed series
2. Z-scores only calculated for data in `y`
3. Forecast period z-scores set to `0` instead of `NA`
4. No support for pre-baseline z-scores

## Proposed Changes

### New Parameter: `bs` (baseline start)

Add optional `bs` parameter to specify where baseline period begins.

**Parameters:**
- `y` = ALL observed data (pre-baseline + baseline + post-baseline)
- `bs` = baseline start index (1-indexed, optional)
- `be` = baseline end index (1-indexed, optional, replaces `b` when `bs` is used)
- `b` = baseline length (kept for backwards compatibility)
- `h` = forecast horizon

### Backwards Compatibility

| Request | Baseline Period |
|---------|-----------------|
| `y=...&b=5&h=3` | `y[1:5]` (current behavior) |
| `y=...&bs=3&be=5&h=3` | `y[3:5]` (new behavior) |

When only `b` is provided (no `bs`), behavior is unchanged: baseline = `y[1:b]`.

### Example

**Scenario:** User has data for 2015-2024 (10 years), wants baseline 2017-2019.

```
GET /?y=100,105,110,115,120,150,180,160,140,130&bs=3&be=5&h=3&s=1&t=0&m=mean
```

- `y[1:2]` = 2015-2016 (pre-baseline observed)
- `y[3:5]` = 2017-2019 (baseline period, model fitted here)
- `y[6:10]` = 2020-2024 (post-baseline observed)
- `h=3` = 2025-2027 (forecast, no observed data)

**Response:**
```json
{
  "y": [108.3, 108.3, 108.3, 108.3, 108.3, 108.3, 108.3, 108.3, 108.3, 108.3, 108.3, 108.3, 108.3],
  "lower": [null, null, null, null, null, null, null, null, null, null, 95.2, 95.2, 95.2],
  "upper": [null, null, null, null, null, null, null, null, null, null, 121.4, 121.4, 121.4],
  "zscore": [-1.66, -0.66, 0.34, 1.34, 2.34, 8.34, 14.34, 10.34, 6.34, 4.34, null, null, null]
}
```

**Output lengths:** `length(y) + h` = 10 + 3 = 13 values

### Z-Score Calculation by Period

| Period | Z-Score Formula | Notes |
|--------|-----------------|-------|
| Pre-baseline | `(observed - predicted) / baseline_sd` | Model extrapolates backwards via `new_data` |
| Baseline | `(observed - fitted) / baseline_sd` | Standard residuals from model fit |
| Post-baseline | `(observed - predicted) / baseline_sd` | Model forecasts forward |
| Forecast | `NA` | No observed data to compare |

Where:
- `baseline_sd` = standard deviation of baseline residuals
- `predicted` = model prediction for that time point
- `fitted` = model fitted value (only available for baseline period)

### Validation Rules

1. `bs` must be >= 1
2. `be` must be > `bs`
3. `be` must be <= `length(y)`
4. Baseline period (`y[bs:be]`) must have at least 3 non-NA values
5. If `bs` is provided, `be` must also be provided (and `b` is ignored)
6. If only `b` is provided, `bs` defaults to 1 and `be` defaults to `b`

### Implementation Notes

#### R/fable approach

```r
# Fit model on baseline only
df_baseline <- df |> filter(index >= bs & index <= be)
mdl <- df_baseline |> model(...)

# Get fitted values for baseline
baseline_fitted <- mdl |> augment()

# Predict pre-baseline using new_data
if (bs > 1) {
  pre_baseline_df <- df |> filter(index < bs)
  pre_baseline_pred <- mdl |> forecast(new_data = pre_baseline_df)
}

# Predict post-baseline + forecast using forecast(h=...)
post_baseline_h <- length(y) - be + h
post_pred <- mdl |> forecast(h = post_baseline_h)

# Calculate z-scores
baseline_residuals <- observed[bs:be] - fitted[bs:be]
baseline_sd <- sd(baseline_residuals)

# Pre-baseline z-scores
zscore[1:(bs-1)] <- (observed[1:(bs-1)] - pre_baseline_pred) / baseline_sd

# Baseline z-scores
zscore[bs:be] <- baseline_residuals / baseline_sd

# Post-baseline z-scores
zscore[(be+1):length(y)] <- (observed[(be+1):length(y)] - post_pred[1:n_post]) / baseline_sd

# Forecast z-scores
zscore[(length(y)+1):(length(y)+h)] <- NA
```

### Response Structure

No changes to response structure, just semantics:

```json
{
  "y": [...],      // length = length(input_y) + h
  "lower": [...],  // length = length(input_y) + h, NA for non-forecast periods
  "upper": [...],  // length = length(input_y) + h, NA for non-forecast periods
  "zscore": [...]  // length = length(input_y) + h, NA for forecast periods
}
```

### Frontend Changes Required

1. Send ALL observed data in `y`, not just baseline
2. Add `bs` and `be` parameters based on user's baseline selection
3. Handle `null`/`NA` z-scores for forecast periods (display as empty or N/A)

### Benefits

1. **Complete z-scores**: Z-scores calculated for all observed periods, not just baseline
2. **Consistent data**: FE always sends full dataset, response always covers full range
3. **No alignment issues**: Response indices directly map to input indices
4. **Simpler visibility changes**: When user adjusts chart visibility (not baseline), no re-request needed since all data is already available

### Migration Path

1. Deploy backend with `bs`/`be` support (backwards compatible)
2. Update frontend to send full data with `bs`/`be`
3. Old frontend requests continue to work with `b` parameter
