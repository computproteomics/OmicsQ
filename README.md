# OmicsQ: A Toolkit for Quantitative Omics Analysis

[![License: MIT](https://img.shields.io/badge/License-MIT-blue.svg)](https://opensource.org/licenses/MIT)

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

OmicsQ can be run locally or via a Docker container for a seamless setup without installation.

### Option 1: Running Locally

To run OmicsQ locally, follow these steps:

1. Install R (version 4.0 or higher) from CRAN.

2. Install RStudio (optional but recommended) from the RStudio website.

3. Install the required R packages: Open R or RStudio and install the necessary packages by running the following command:

```r
install.packages("BiocManager")
BiocManager::install(c("shiny", "shinyBS", "shinyWidgets", "shinycssloaders", 
                   "shinythemes", "shinyjs", "DT", "data.table", "readxl", 
                   "stringdist", "limma", "matrixStats", "MsCoreUtils", 
                   "jsonlite", "BEclear", "sva", "gridExtra", "ggplot2", 
                   "gplots","viridis"))
```

4. Clone the OmicsQ repository: Download the latest version of OmicsQ from GitHub, each in bash:
```
git clone https://github.com/computproteomics/OmicsQ.git
```

5. Run the app: In R or RStudio, set your working directory to the cloned repository, and then start the app:

```r
    shiny::runApp("OmicsQ")
```

### Option 2: Using Docker

For an easier and more consistent setup, OmicsQ is also available as a Docker container. This approach ensures that all 
dependencies are correctly configured and avoids potential installation issues.

1. Install Docker: Follow the instructions on the Docker website to install Docker.

2. Pull the OmicsQ Docker image: Run the following command to download the Docker container.
```
docker pull veitveit/omicsq:latest
```

3. Run the Docker container: Start the OmicsQ app using Docker.

```
docker run --rm -p 3838:3838 computproteomics/omicsq:latest
```

4 Access the app: Open a web browser and go to http://localhost:3838 to start using OmicsQ.

## Documentation and tutorial

For detailed instructions on how to use OmicsQ, please refer to the 
[User Manual](https://computproteomics.bmb.sdu.dk/app_direct/OmicsQ/tutorial/Tutorial.html). 
There, you can also find a tutorial on how to use the app for your data analysis.

## Workflow

![](www/OmicsQWorkflow.png){width=75%}

## License

OmicsQ is released under the MIT License. See the LICENSE file for details.

## Citation

If you use OmicsQ in your research, please cite our work:

Xuan-Tung Trinh et al. OmicsQ: A Toolkit for Quantitative Analysis of Omics Data. In preparation.

## Contributing

We welcome contributions! Please submit a pull request or open an issue to get involved.

## Contact

For questions, comments, or suggestions, please contact the development team at [veits@bmb.sdu.dk].