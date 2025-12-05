# OmicsQ: A Toolkit for Quantitative Omics Analysis

[![Bridge](https://img.shields.io/badge/bridge-bio.tools%20%E2%86%92%20github-blue.svg?labelColor=orange&logo=data:image/svg%2bxml;base64,PHN2ZyBpZD0idXVpZC0zYTVmNzA5MS0xMzM2LTQxNGUtYjBmNS1jMDdkMmQwYWM2MDEiIHhtbG5zPSJodHRwOi8vd3d3LnczLm9yZy8yMDAwL3N2ZyIgdmlld0JveD0iMCAwIDEwLjAxIDYuOCI+PHBhdGggZD0iTTUuNjYsNS42NmwtLjc0Ljc0Yy0uMjYuMjYtLjU5LjQtLjk1LjRzLS43LS4xNC0uOTYtLjRMLjUyLDMuOTFjLS4yMy0uMjMtLjM3LS41My0uMzktLjg1TDAsMS40NWMtLjAzLS4zNy4xLS43NC4zNi0xLjAxQy42NC4xNCwxLjA0LS4wMiwxLjQ1LjAxbDEuNjEuMTJjLjMyLjAzLjYyLjE3Ljg0LjM5bC42Mi42MWMuMTYuMTcuMTYuNDMsMCwuNTktLjE3LjE2LS40My4xNi0uNTksMGwtLjYxLS42MWMtLjA5LS4wOS0uMi0uMTQtLjMzLS4xNWwtMS42LS4xM2MtLjE2LS4wMS0uMzIuMDUtLjQyLjE3LS4xLjEtLjE1LjI1LS4xNC4zOWwuMTMsMS42MWMuMDEuMTIuMDYuMjQuMTUuMzJsMi40OSwyLjVjLjEuMDkuMjMuMTUuMzcuMTUuMTMsMCwuMjYtLjA2LjM2LS4xNWwuNzQtLjc0Yy4wMi4xNC4wOS4yOC4yLjM4LjEuMTEuMjQuMTguMzkuMloiIGZpbGw9IiNmZmYiLz48cGF0aCBkPSJNMTAsMS40NWwtLjEzLDEuNjFjLS4wMi4zMi0uMTYuNjItLjM5Ljg1bC0yLjQ5LDIuNDljLS4yNi4yNi0uNi40LS45Ni40LS4xMiwwLS4yNS0uMDItLjM3LS4wNS0uMjItLjA3LS4zNC0uMy0uMjgtLjUyLjA2LS4yMi4yOS0uMzQuNTEtLjI4LjA1LjAxLjEuMDIuMTQuMDIuMTQsMCwuMjctLjA2LjM3LS4xNWwyLjQ5LTIuNWMuMDktLjA4LjE1LS4yLjE1LS4zMmwuMTMtMS42MWMuMDEtLjE0LS4wNC0uMjktLjE0LS4zOS0uMS0uMTItLjI2LS4xOC0uNDItLjE3bC0xLjYuMTNjLS4xMy4wMS0uMjQuMDYtLjMzLjE1bC0xLjc3LDEuNzdjLS4wMy0uMTQtLjEtLjI3LS4yMS0uMzgtLjEtLjExLS4yNC0uMTgtLjM4LS4ybDEuNzgtMS43OGMuMjItLjIyLjUyLS4zNi44NC0uMzlsMS42MS0uMTJzLjA3LS4wMS4xLS4wMWMuMzgsMCwuNzQuMTYuOTkuNDQuMjYuMjcuMzkuNjQuMzYsMS4wMVoiIGZpbGw9IiNmZmYiLz48Y2lyY2xlIGN4PSI4LjA1IiBjeT0iMS45NiIgcj0iLjcxIiBmaWxsPSIjZmZmIi8+PGNpcmNsZSBjeD0iMS45NiIgY3k9IjEuOTYiIHI9Ii43MSIgZmlsbD0iI2ZmZiIvPjxwYXRoIGQ9Ik00LjUyLDUuMjVjLS4xMi4xMi0uMzEuMTUtLjQ2LjA5LS4wNS0uMDItLjA5LS4wNS0uMTMtLjA5bC0uMzMtLjMzYy0uMjUtLjI1LS4zOS0uNTktLjM5LS45NXMuMTQtLjcuMzktLjk1bC4zMS0uMzFjLjE2LS4xNi40Mi0uMTYuNTgsMCwuMTUuMTUuMTcuMzkuMDMuNTZsLS4zMy4zM2MtLjEuMS0uMTYuMjMtLjE2LjM3cy4wNi4yNy4xNi4zN2wuMzMuMzNjLjE2LjE3LjE2LjQyLDAsLjU4WiIgZmlsbD0iI2ZmZiIvPjxwYXRoIGQ9Ik02Ljc5LDMuOTdjMCwuMzYtLjE0LjctLjM5Ljk1bC0uMzMuMzNjLS4xNy4xNi0uNDMuMTYtLjU5LDAtLjE1LS4xNS0uMTYtLjM5LS4wMy0uNTVsLjM2LS4zNmMuMS0uMS4xNi0uMjMuMTYtLjM3cy0uMDYtLjI3LS4xNi0uMzdsLS4zLS4zYy0uMTYtLjE2LS4xNi0uNDIsMC0uNTkuMDgtLjA4LjE5LS4xMi4yOS0uMTIuMDcsMCwuMTQuMDIuMi4wNXMuMTEuMDguMTYuMTNjLjA4LjA4LjE1LjE1LjI0LjI0LjI1LjI1LjM5LjU5LjM5Ljk1WiIgZmlsbD0iI2ZmZiIvPjwvc3ZnPg==)](https://bio-tools.github.io/biohackathon2025/)
[![bio.tools](https://img.shields.io/badge/bio.tools-OmicsQ-blue.svg?labelColor=gray&logo=data:image/svg%2bxml;base64,PHN2ZyBpZD0idXVpZC02ZTIxYTIzOC04NWFmLTRlNDctYjI1OC05ZTEyZDg2MzJmYmUiIHhtbG5zPSJodHRwOi8vd3d3LnczLm9yZy8yMDAwL3N2ZyIgdmlld0JveD0iMCAwIDQzMi44NCA0MzIuODQiPjxwYXRoIGQ9Ik01NS4zLDQyNy4yN2wtNDkuNzMtNDkuNzNjLTcuNDMtNy40My03LjQzLTE5LjQ3LDAtMjYuODlsMTMxLjYzLTEzMS42M2M3LjQzLTcuNDMsMTkuNDctNy40MywyNi44OSwwbDQ5LjczLDQ5LjczYzcuNDMsNy40Myw3LjQzLDE5LjQ3LDAsMjYuODlsLTEzMS42MywxMzEuNjNjLTcuNDMsNy40Mi0xOS40Niw3LjQyLTI2Ljg5LDBaIiBmaWxsPSIjZmZmIi8+PHBhdGggZD0iTTIyNy43MSwyNTUuNzFsLTcuMTgsNy4zNy02LjM0LDYuNC01MS4xMy01MC4xOSw2LjM0LTYuNCw3LjE4LTcuMzdjNi42NC02Ljc2LDE3LjQ1LTYuODYsMjQuMjEtLjIybDI2LjY5LDI2LjJjNi43Nyw2LjY0LDYuODcsMTcuNDUuMjMsMjQuMjFoMFoiIGZpbGw9IiNmZmYiLz48cGF0aCBkPSJNNDMwLjQsMTguNTNsLTE2LjExLTE2LjEiIGZpbGw9IiNmZmYiLz48cGF0aCBkPSJNMzQ4LjQ2LDYzLjM3bC0xMTkuNzQsMTE5LjczLTI0Ljk0LDI0Ljk0LDIxLjAxLDIxLjAxLDI0Ljk0LTI0Ljk0LDExOS43NC0xMTkuNzMiIGZpbGw9IiNmZmYiLz48cGF0aCBkPSJNMzY5LjQ0LDg0LjM4bDI3LjE2LDEuOSwzNi4yNC02NS4zTDQxMS44NSwwbC02NS4zLDM2LjIzLDEuOSwyNy4xNyIgZmlsbD0iI2ZmZiIvPjxwYXRoIGQ9Ik0xMTIuNjEsMTU1LjZjLTI5LjE0LDExLjgzLTYzLjgsNS45NC04Ny40NC0xNy43QzIuOTYsMTE1LjY5LTMuNTgsODMuNzcsNS41NCw1NS44MyIgZmlsbD0iI2ZmZiIvPjxwYXRoIGQ9Ik01Ny4xMiw0LjI2YzI3LjkxLTkuMTUsNTkuODctMi41OCw4Mi4wNywxOS42MywyMy42MSwyMy42MSwyOS41Myw1OC4yNSwxNy43LDg3LjM5IiBmaWxsPSIjZmZmIi8+PHBhdGggZD0iTTI3Ny4yMywzMjAuMjJjLTExLjgzLDI5LjE0LTUuOTQsNjMuOCwxNy43LDg3LjQ0LDIyLjIxLDIyLjIxLDU0LjEzLDI4Ljc1LDgyLjA3LDE5LjYzIiBmaWxsPSIjZmZmIi8+PHBhdGggZD0iTTMyMS41NiwyNzUuOTRjMjkuMTQtMTEuODMsNjMuNzgtNS45Miw4Ny4zOSwxNy43LDIyLjIxLDIyLjIxLDI4Ljc4LDU0LjE2LDE5LjYzLDgyLjA3IiBmaWxsPSIjZmZmIi8+PHBhdGggZD0iTTE2My45MSwyMDcuNTRMNDIuNyw5MS4yNmw1MC43MS00OC4xMiwxMzAuMjMsMTM1LjU3LTkuMjIsOS4xOC0xMy4wOSwxMi4yOC01Ljk4LTQuMTloMGMtLjI4LS4xOC00LjMtMi4yMy00LjYtMi4zNi0uMzctLjE2LTEuOTUtMS4xNi01LjctLjg0LTMuMTEuMjctNS43NCwxLjYyLTguNTcsMy45OGwtMTIuNTgsMTAuNzhoLjAxWiIgZmlsbD0iI2ZmZiIvPjxwYXRoIGQ9Ik0yMjQuMTgsMjY4LjI0bDExNS4zMywxMjAuNDQsNDguMzMtNTAuODktMTM0LjUyLTEyOS4zNC05LjIyLDkuMjYtMTIuMzQsMTMuMTMsNC4xNSw1Ljk1aDBjLjE3LjI4LDIuMiw0LjI4LDIuMzMsNC41OC4xNi4zNywxLjE1LDEuOTQuOCw1LjY4LS4yOCwzLjEtMS42NSw1Ljc0LTQuMDMsOC41OGwtMTAuODQsMTIuNjJoLjAxWiIgZmlsbD0iI2ZmZiIvPjwvc3ZnPg==)](https://bio.tools/OmicsQ)
[![License: Apache 2](https://img.shields.io/badge/License-Apache2-blue.svg)](https://opensource.org/licenses/apache-2-0)
[![bio.tools](https://img.shields.io/badge/bio.tools-OmicsQ-005472)](https://bio.tools/omicsq)
[![DOI](https://zenodo.org/badge/DOI/10.5281%2Fzenodo.17068405.svg)](https://doi.org/10.5281/zenodo.17068405)
![](www/OmicsQWorkflow.png)

OmicsQ is a Shiny-based web application designed for preprocessing and quantitative analysis of Omics data. 
It streamlines the preparation of data for subsequent analysis using specialized tools such as 
[PolySTest](https://computproteomics.bmb.sdu.dk/app_direct/PolySTest/) for statistical testing and
[VSClust](https://computproteomics.bmb.sdu.dk/app_direct/VSClust/) for clustering analysis. 

Run OmicsQ via your browser on our server: [OmicsQ](https://computproteomics.bmb.sdu.dk/app_direct/OmicsQ/)

Visit [ComputProteomics Group Page](https://computproteomics.bmb.sdu.dk/) for more information on related research and tools.

## Key features

**Interactivity**: Easily browse the data and selectively analyze and visualize according to your needs.   
 
**User-Friendly Experience**: Avoid installation issues by using the app directly in your browser, 
with a simple interface for easy navigation.  

**Flexibility**: Easily upload multiple file formats (text, Excel) and use the automated assignment of sample groups and 
batches.  

**Data Pre-processing**: Batch correction, normalization, filtering, and missing value filtering with visual feedback.

**Web Service Integration**: Submit processed data directly to the following 
tools:  
[PolySTest](https://computproteomics.bmb.sdu.dk/app_direct/PolySTest/) for statistical testing with proper treatment of missing data  
[VSClust](https://computproteomics.bmb.sdu.dk/app_direct/VSClust/) for variance-sensitive clustering analysis also of incomplete data sets  
[ComplexBrowser](https://computproteomics.bmb.sdu.dk/app_direct/ComplexBrowser/) for exploration of quantitative behavior of protein complexes (mainly applicable to proteomics data).  

**Data Export**: Download the processed data or analysis results for further offline analysis.


## Installation

OmicsQ can be run locally on your machine or via Docker. Pick the option that suits you best.

### Option 1: Run Locally (R)

1) Install R (4.2 or newer) and optionally RStudio.

2) Install required packages. In an R session:

```r
# CRAN packages
install.packages(c(
  "shiny", "shinyBS", "shinyWidgets", "shinycssloaders", "shinythemes",
  "shinyjs", "DT", "data.table", "readxl", "openxlsx", "stringdist",
  "limma", "matrixStats", "jsonlite", "gridExtra", "ggplot2", "gplots",
  "viridis", "ggrepel", "ggiraph"
))

# Bioconductor packages
install.packages("BiocManager")
BiocManager::install(c("MsCoreUtils", "BEclear", "sva", "UniProt.ws"), ask = FALSE)
```

3) Clone this repository and run the app from the repo root:

```bash
git clone https://github.com/computproteomics/OmicsQ.git
cd OmicsQ
```

```r
shiny::runApp(".")
```

### Option 2: Run with Docker

This bundles all dependencies and runs a Shiny server.

Build the image locally:

```bash
git clone https://github.com/computproteomics/OmicsQ.git
cd OmicsQ
docker build -t omicsq .
```

Run the container and expose on port 3838:

```bash
docker run --rm -p 3838:3838 omicsq
```

Open http://localhost:3838 in your browser.

## Documentation and tutorial

For detailed instructions on how to use OmicsQ, please refer to the 
[User Manual](https://computproteomics.bmb.sdu.dk/app_direct/OmicsQ/tutorial/Tutorial.html). 
There, you can also find a tutorial on how to use the app for your data analysis.

## Workflow

{width=75%}

## License

OmicsQ is released under the MIT License. See the LICENSE file for details.

## Citation

If you use OmicsQ in your research, please cite our work:

Xuan-Tung Trinh et al. OmicsQ: A Toolkit for Quantitative Analysis of Omics Data. [preprint](https://arxiv.org/abs/2504.19813)

## Contributing

We welcome contributions! Please submit a pull request or open an issue to get involved.

## Contact

For questions, comments, or suggestions, please contact the development team at [veits@bmb.sdu.dk].
