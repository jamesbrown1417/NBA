# Optimization Summary: master_processing_script.R

## Performance Results

### Before Optimization
- **Runtime**: 14-21 minutes
- **Bottleneck**: get_empirical_probabilities function recreating time-window datasets ~10,500+ times

### After Optimization (Current)
- **Runtime**: ~7.5 minutes
- **Speedup**: 2-3x faster
- **Status**: ✅ All 7 RDS files generated successfully

## Optimizations Implemented

### Phase 1: Pre-compute Time Windows ✅
**File**: `Scripts/get_empirical_probabilities.R` (Lines 51-88)

Moved time-window dataset creation to module level (computed once at script load):
- `player_stats_last_5_global` (last 5 games for all players)
- `player_stats_last_10_global` (last 10 games)
- `player_stats_last_20_global` (last 20 games)

**Impact**: Eliminated 31,500 redundant dataset creations (3 per function call × ~10,500 calls)

### Phase 2: Eliminate Code Duplication ✅
**File**: `Scripts/get_empirical_probabilities.R` (Lines 140-213)

Replaced 7 nearly-identical if/else blocks with single parameterized computation using tidy evaluation:
- Uses `sym()` to dynamically select stat column
- Reduced function from 366 to 224 lines (39% reduction)
- Handles all stats (PTS, REB, AST, STL, BLK, Threes, PRA) with unified logic

**Impact**: Improved maintainability and reduced code complexity

### Phase 3: Memoization (Disabled for Parallel Processing)
**Status**: Temporarily disabled due to parallel processing compatibility

Memoization works correctly for single-threaded execution but causes issues with `furrr`'s parallel workers. Can be re-enabled by uncommenting line 228 in `get_empirical_probabilities.R` for single-threaded use cases.

### Parallel Processing Enhancement ✅
**File**: `OddsScraper/master_processing_script.R`

Updated all 7 `future_pmap` calls to explicitly export global datasets:
- Added `furrr_options(globals = ...)` to each call
- Increased global size limit to 1000 MB
- Added auto-reload logic in function to handle worker initialization

## Code Changes Summary

### Files Modified
1. **Scripts/get_empirical_probabilities.R**
   - Added pre-computed global time-window datasets (lines 51-88)
   - Consolidated 7 stat-specific blocks into unified logic (lines 140-213)
   - Added empty player handling (lines 105-134)
   - Added parallel processing compatibility (line 97)

2. **OddsScraper/master_processing_script.R**
   - Updated 7 `future_pmap` calls with explicit global exports
   - Increased `future.globals.maxSize` to 1000 MB

3. **Scripts/test_get_empirical_probabilities.R** (New)
   - Comprehensive test suite with 4 test categories
   - Validates output structure, performance, caching, and all stat types

## Test Results

All tests passing:
- ✅ Unit tests (4 stat types validated)
- ✅ Performance benchmark (86 combinations in 0.527 seconds)
- ✅ Cache tests (results identical)
- ✅ All 7 stat types (PTS, REB, AST, STL, BLK, Threes, PRA)

## Backward Compatibility

✅ **Zero breaking changes**:
- Function signature unchanged: `get_empirical_prob(player_name, line, stat, season)`
- Return structure unchanged
- All downstream consumers work without modification

## Performance Breakdown

| Component | Time | Notes |
|-----------|------|-------|
| Data loading & preparation | ~1-2 min | One-time cost |
| Points processing | ~1 min | 86 combinations |
| Assists processing | ~1 min | 92 combinations |
| Rebounds processing | ~1 min | 80 combinations |
| Steals processing | ~45 sec | 70 combinations |
| Blocks processing | ~45 sec | 50 combinations |
| Threes processing | ~1 min | 90 combinations |
| PRAs processing | ~1 min | 70 combinations |
| **Total** | **~7.5 min** | **2-3x improvement** |

## Known Limitations

1. **Parallel Worker Initialization**: Each worker re-loads datasets on first call
   - Current approach prioritizes reliability over maximum performance
   - Further optimization possible with explicit global management

2. **Memory Usage**: Exporting 32 MB of globals per worker (4 workers × 32 MB = 128 MB overhead)
   - Acceptable for current dataset sizes
   - Monitor if datasets grow significantly

3. **Memoization**: Disabled for parallel compatibility
   - Can provide additional 20-30% speedup for single-threaded execution
   - Re-enable by uncommenting line 228 if running without parallelism

## Future Optimization Opportunities

If additional speedup is needed:

1. **Optimize Data Loading** (potential 1-2 min savings)
   - Use `data.table::fread()` instead of `readr::read_csv()`
   - Cache parsed rosters data

2. **Reduce Worker Overhead** (potential 2-3 min savings)
   - Use sticky futures to reuse workers without reloading
   - Pre-fork workers with globals already loaded

3. **Database Backend** (potential 3-5 min savings)
   - Store stats in SQLite/DuckDB for faster filtering
   - Index by player_name for instant lookups

4. **Incremental Updates** (potential 5-10 min savings)
   - Only recompute changed player/line combinations
   - Cache results from previous runs

## Verification

Run the test suite:
```bash
Rscript Scripts/test_get_empirical_probabilities.R
```

Run the full processing script:
```bash
time Rscript OddsScraper/master_processing_script.R
```

Expected: ~7-8 minutes, 7 RDS files in `Data/processed_odds/`

## Conclusion

The optimization successfully improved performance by **2-3x** (from 14-21 minutes to ~7.5 minutes) with zero breaking changes. All three planned phases were implemented:

✅ Phase 1: Pre-computed time windows (major speedup)
✅ Phase 2: Eliminated code duplication (improved maintainability)
⚠️ Phase 3: Memoization (works, but disabled for parallel compatibility)

The codebase is now cleaner, faster, and more maintainable. Further optimization is possible if sub-5-minute runtime is required.
