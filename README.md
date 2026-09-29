# tree-diff-r

A lightweight JSON, XML, and CSV tree diff utility in R, together with a Shiny browser UI for comparing structured files side-by-side.

## What it does

This project mirrors the core behavior of the Python version in the workspace:

- recursively compares JSON structures, XML elements, and CSV rows
- diffs scalar values, adds, removes, and moves within arrays
- keeps list reordering and nested object changes readable
- exposes the same diff logic in a Shiny app that lets users paste or upload supported files

## Live demo

A hosted version of the app is available here:

https://gerrycampion.github.io/tree-diff-r/

> Note: the app may take a few seconds to load the first time because it is running as an `r-shinylive` static app.

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
   Rscript -e "install.packages(c('renv', 'languageserver', 'vscDebugger'), repos=c('https://cloud.r-project.org','https://manuelhentschel.r-universe.dev')"
   Rscript -e "renv::restore()"
   ```

4. Allow an app through Controlled folder access:
- Open the Start Menu, search for Windows Security, and open it.
- Click on Virus & threat protection.
- Scroll down to Ransomware protection and click Manage ransomware protection.
- Click Allow an app through Controlled folder access.
- Click Add an allowed app and select Recently blocked apps.
- Locate rterm.exe in the list (usually found inside your R installation path, like C:\Program Files\R\R-x.x.x\bin\Rterm.exe) and choose to allow it.
- Locate rscript.exe in the list (usually found inside your R installation path, like C:\Program Files\R\R-x.x.x\bin\Rscript.exe) and choose to allow it.

## VS Code setup

This project includes workspace recommendations for the VS Code R tooling.

VS Code can install all recommended extensions automatically when you open the workspace and accept the prompt, or by running the command "Extensions: Install Recommended Extensions" from the Command Palette.

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

- choose the JSON, XML, or CSV sample pair
- paste or upload two inputs; file extensions and sample selection set the input type automatically
- choose JSON, XML, or CSV manually when entering text
- click the diff button
- inspect the structured diff and its paths in the browser

## Run the library directly

```r
source("R/tree_diff.R")
base <- jsonlite::fromJSON("samples/base.json", simplifyVector = FALSE)
compare <- jsonlite::fromJSON("samples/compare.json", simplifyVector = FALSE)
print(diff_value(base, compare))
```

The Shiny app also accepts XML and CSV text through `compute_format_diff()`. XML paths use XPath-style element, attribute, and text segments. CSV paths use one-based row numbers and column names. The XML parser is provided by `xml2` and is included in `renv.lock`.

## GitHub Pages / static export

This repo includes a GitHub Actions workflow that exports the Shiny app to a static site with `r-shinylive` and publishes it to GitHub Pages.

- Workflow: `.github/workflows/deploy-shinylive.yml`
- Export directory: `docs/`
- GitHub Pages source: GitHub Actions

The app is exported with:

```r
shinylive::export(".", "docs")
```

After the workflow runs on `main`, the static app is available through the repository's GitHub Pages site.

## Sample data

The project includes JSON sample inputs in `samples/` and XML and CSV sample pairs in `samples/xml/` and `samples/csv/`. The app exposes all three pairs through its Sample set selector.

