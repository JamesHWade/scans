# Write or read a saved investigation

`write_investigation()` writes a value created by
[`investigation_snapshot()`](https://jameshwade.github.io/scans/reference/investigation_snapshot.md).
`read_investigation()` validates and reopens it without contacting its
source or rerunning scanners. Saved scanner identities can be reopened
even when they are absent from the installed registry. Pass the result
to
[`scans_app()`](https://jameshwade.github.io/scans/reference/scans_app.md)
or inspect its `bundle`, `analysis`, `settings`, and `manifest`.

## Usage

``` r
write_investigation(x, path, overwrite = FALSE)

read_investigation(path, max_bytes = 50 * 1024^2)
```

## Arguments

- x:

  A saved investigation from
  [`investigation_snapshot()`](https://jameshwade.github.io/scans/reference/investigation_snapshot.md)
  or `read_investigation()`.

- path:

  Path to a local JSON file.

- overwrite:

  Whether to replace an existing file. Defaults to `FALSE`.

- max_bytes:

  Largest file to read, in bytes. Defaults to 50 MiB. Use `Inf` to
  disable the size limit.

## Value

`write_investigation()` returns `x` invisibly. `read_investigation()`
returns a `scans_investigation` value.

## Details

Files use the versioned scans JSON format. They contain retained
content, not an anonymized report. SHA-256 identifiers detect changes to
the evidence or analysis; they do not establish who created the file.
Unsupported formats, invalid references, and edited identifiers are
rejected.

## Examples

``` r
bundle <- TrajectoryBundle(
  data.frame(trajectory_id = "run-1", source_type = "manual",
             status = "completed"),
  data.frame(), data.frame()
)
saved <- investigation_snapshot(bundle)
path <- tempfile(fileext = ".scans.json")
write_investigation(saved, path)
reopened <- read_investigation(path)
reopened$manifest$snapshot_id
#> [1] "sha256:abe100009a05adf100424e85c9c2c3962b1ee023cab3da1733eba5891ad3d00a"
```
