# Available trajectory scans

`scan_registry()` lists the detectors
[`scan_trajectories()`](https://jameshwade.github.io/scans/reference/scan_trajectories.md)
can run, with the severity each produces and a one-line description. Use
it to build a scan selection rather than hard-coding names.

## Usage

``` r
scan_registry(scanners = NULL)
```

## Arguments

- scanners:

  Optional custom scanners to list after the built-ins: one
  [`scanner()`](https://jameshwade.github.io/scans/reference/scanner.md)
  or a list of them.

## Value

A tibble with `scan`, `severity`, and `description` columns.

## Details

Custom scanners are not registered anywhere. Pass the scanners you
intend to use in a call to list them after the built-ins, in the same
shape.

## Examples

``` r
scan_registry()
#> # A tibble: 11 × 3
#>    scan                       severity description                              
#>    <chr>                      <chr>    <chr>                                    
#>  1 ambiguous_tool_correlation warning  A call identifier matches more than one …
#>  2 unresolved_tool_call       warning  A tool was called and no result came bac…
#>  3 unmatched_tool_result      warning  A tool result arrived with no call to ma…
#>  4 repeated_tool_call         warning  The same tool was called with the same a…
#>  5 suspicious_tool_loop       warning  Consecutive identical tool calls suggest…
#>  6 event_error                error    An event recorded a failure.             
#>  7 error_chain                error    A failure followed an earlier one within…
#>  8 turn_error                 error    A turn ended with a failed status, an er…
#>  9 trajectory_error           error    The trajectory ended with a failed statu…
#> 10 cut_off_answer             error    The provider cut the run's final answer …
#> 11 run_intervention           warning  The agent runtime intervened: a limit wr…
```
