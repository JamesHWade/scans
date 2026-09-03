# Available trajectory scans

`scan_registry()` lists the detectors
[`scan_trajectories()`](https://jameshwade.github.io/scans/reference/scan_trajectories.md)
can run, with the severity each produces and a one-line description. Use
it to build a scan selection rather than hard-coding names.

## Usage

``` r
scan_registry()
```

## Value

A tibble with `scan`, `severity`, and `description` columns.

## Examples

``` r
scan_registry()
#> # A tibble: 9 × 3
#>   scan                       severity description                               
#>   <chr>                      <chr>    <chr>                                     
#> 1 ambiguous_tool_correlation warning  A call identifier matches more than one c…
#> 2 unresolved_tool_call       warning  A tool was called and no result came back.
#> 3 unmatched_tool_result      warning  A tool result arrived with no call to mat…
#> 4 repeated_tool_call         warning  The same tool was called with the same ar…
#> 5 suspicious_tool_loop       warning  Consecutive identical tool calls suggest …
#> 6 event_error                error    An event recorded a failure.              
#> 7 error_chain                error    A failure followed an earlier one within …
#> 8 turn_error                 error    A turn ended with a failed status, an err…
#> 9 trajectory_error           error    The trajectory ended with a failed status…
```
