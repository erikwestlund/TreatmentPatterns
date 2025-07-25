# Changelog

## [Unreleased] - 2025-01-24

### Fixed

#### 1. Fixed row removal bug in `doEraCollapse` function
- **File**: `R/constructPathways.R`, line 420
- **Issue**: The function was using incorrect syntax in the filter statement that prevented proper row removal
- **Fix**: Already correctly implemented in this fork using `dplyr::filter(!.data$row %in% rows)`
- **Impact**: This ensures `eraCollapseSize` works correctly to collapse consecutive eras of the same event

#### 2. Fixed consecutive same-event transitions after combination processing
- **File**: `R/constructPathways.R`, lines ~92 (after doCombinationWindow)
- **Issue**: Same-event transitions (e.g., "DR Screening, In Office → DR Screening, In Office") were appearing in treatment pathways even when `eraCollapseSize` was set to a large value
- **Root Cause**: 
  - The pipeline runs `doEraCollapse` before `doCombinationWindow`
  - After `doCombinationWindow` processes the data, it can create new situations where the same event appears consecutively (e.g., when a combination period ends)
  - Since `doEraCollapse` only runs once before combinations, it cannot handle these newly adjacent same events
- **Fix**: Added logic to re-run `doEraCollapse` after combination window processing to handle newly created adjacencies
- **Impact**: This ensures that consecutive same-event transitions are properly collapsed according to the `eraCollapseSize` parameter throughout the entire pipeline

#### 3. Fixed combinationWindow=0 to properly skip combination processing
- **File**: `R/constructPathways.R`, line ~86 (doCombinationWindow call)
- **Issue**: Setting `combinationWindow = 0` still resulted in combination events being created
- **Root Cause**: The combination detection logic identifies ALL overlapping events regardless of the combinationWindow value, then processes them into combinations
- **Fix**: Added check to skip `doCombinationWindow` entirely when `combinationWindow = 0`
- **Impact**: When users set `combinationWindow = 0`, no combination events will be created

### Technical Details

These fixes address critical issues in the TreatmentPatterns module:
1. The row removal bug was already fixed in this fork
2. The re-collapse after combinations ensures consistent behavior throughout the pipeline
3. The combinationWindow=0 fix provides users with a way to completely disable combination detection

### Testing Recommendations

When testing these fixes:
1. Set `combinationWindow = 0` to prevent combination detection
2. Set `eraCollapseSize = 99999` (or another large value) to collapse all consecutive same events
3. Verify that pathways no longer contain transitions like "Event A → Event A"
4. Test with various combinationWindow values to ensure combinations work when desired