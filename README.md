# tree-diff-r

A lightweight JSON tree diff utility in R, together with a Shiny browser UI for comparing two JSON payloads side-by-side in the browser.

## What it does

This project mirrors the core behavior of the Python version in the workspace:

- recursively compares nested JSON-like objects
- diffs scalar values, adds, removes, and moves within arrays
- keeps list reordering and nested object changes readable
- exposes the same diff logic in a Shiny app that lets a user paste JSON text or upload files

## Install R on Windows

1. Install R from CRAN:

   - Recommended: use the official CRAN installer from https://cran.r-project.org/bin/windows/base/
   - Or install via winget:

   ```powershell
   winget install --id RProject.R --source winget
   ```

2. Optional but recommended: install RStudio Desktop from https://posit.co/download/rstudio-desktop/

3. Install the required R packages:

   ```powershell
   Rscript -e "install.packages(c('renv'), repos='https://cloud.r-project.org')"
   Rscript -e "renv::restore()"
   ```

## Run the Shiny app

From the project root:

```powershell
cd tree-diff-r
Rscript app.R
```

You can also launch it from the project directory in RStudio with:

```r
shiny::runApp("app.R")
```

The browser will open a local Shiny UI where you can:

- paste JSON into the two text areas
- upload two JSON files
- click the diff button
- inspect the pretty-printed result in the browser

## Run the library directly

```r
source("R/tree_diff.R")
base <- jsonlite::fromJSON("samples/base.json", simplifyVector = FALSE)
compare <- jsonlite::fromJSON("samples/compare.json", simplifyVector = FALSE)
print(diff_value(base, compare))
```

## Sample data

The project includes sample inputs in the `samples/` folder and an expected diff output in `samples/diff_paths.json`.

