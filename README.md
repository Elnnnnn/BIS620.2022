# bis620.2022

This package is to launch a clinical trial inquiry dashboard. The primary function of this package is `shiny_app()`, designed to launch a R Shiny app without any parameters, featuring various functionalities (as outlined below) and utilizes data sourced from ClinicalTrials.gov.
<!-- badges: start -->
[![R-CMD-check](https://github.com/Elnnnnn/bis620.2022/actions/workflows/R-CMD-check.yml/badge.svg)](https://github.com/Elnnnnn/bis620.2022/actions/workflows/R-CMD-check.yml)
<!-- badges: end -->

<!-- badges: start -->
[![Test-coverage](https://github.com/Elnnnnn/bis620.2022/actions/workflows/test-coverage.yaml/badge.svg)](https://github.com/Elnnnnn/bis620.2022/actions/workflows/test-coverage.yaml)
<!-- badges: end -->

[Test coverage webpage](https://app.codecov.io/gh/Elnnnnn/bis620.2022/tree/main/R)

## Author
-[Zhongyu Cai](https://github.com/ZoeyCai2001)  

-[Bowen Zhao](https://github.com/bowenwen)  

-[Xinyu Zhang](https://github.com/Elnnnnn)

## Installation

To install the development version of bis620.2022, you can use the following command in R, assuming you have the `devtools` package installed:

```r
# install.packages("devtools")
devtools::install_github("Elnnnnn/bis620.2022")
```

## Introduction



To initiate the Shiny app, simply execute the following command:

```r
shiny_app()
```

## Data

The clinical trial inquiry webpage incorporates embedded data within the `shiny_app()` function, eliminating the need for users to install or download the data separately. However, users can also load the data using the command:

```r
data("studies")
```

The dataset contains 13 columns and 471252 rows, originating from [National Library of Medicine](https://clinicaltrials.gov/). Additionally, users have the option to download the entire database independently using the following commands:

```r
# devtools::install_github("presagia-analytics/ctrialsgov")
library(ctrialsgov) 
# ONLY RUN THIS ONCE!
#  ctgov_get_latest_snapshot()
```


**Feature 1: “Data Range Selection**

The “Date Range Selection” feature in the Clinical Trials Query is a user interface component that provides the capability to filter clinical trials based on specified start and completion dates. Concerning the “User Interface” part, A dateRangeInput UI element allows users to select a date range for the beginning of clinical trials. By default, it spans from 3000 days before the current system date to the current date, ensuring comprehensive coverage of recent and past trials, as well as the Completion Date Range. Concerning the “server” part, the query_kwds function is enhanced to incorporate date filtering. When called, it accepts optional start_date_range and completion_date_range parameters. Overall, the server filters the studies based on the provided keywords and further refines the data set according to the selected start and completion dates.


**Feature 2: Word Cloud**

The Word Cloud feature, both visually appealing and functional, simplifies the understanding of data. Initially, the "getTermMatrix'' function processes the filtered clinical trial data to extract a list denoting the frequency of each word. Subsequently, the Wordcloud functionality crafts a graphic depiction of word frequency. In the generated word cloud, words that are frequently mentioned in the study data are prominently displayed in a larger font, while less common words are prominently displayed in a smaller font. In this way, Word Cloud features not only provide snapshots of popular research topics, but also help explore datasets efficiently and interactively, saving users from the tedious task of manually sifting through large amounts of textual data.


**Feature 3: World Map**

The feature generates a map where each country is shaded based on the number of clinical trials conducted. Countries with a high number of studies are represented by darker shadows, allowing users to quickly understand the geographical distribution of research efforts at a glance. Use the “plot_countries” function to create a map highlighting the amount of research conducted in each country. Based on the country name, the function uses joinCountryData2Map to merge the data set with the Spatial Data Frame (spdf). Finally, the map is drawn using the mapCountryData function. Study quantities are drawn using a gradient color scheme from white to black.


**Feature 4: American Map**

It provides a visual representation of which states in America have higher concentrations of trials. The colour intensity represents the trial frequency—states with more trials are displayed in a darker shade, while those with fewer trials appear lighter. We first group the data by state and count the number of trials per state, creating a summary frame suitable for visualization. The “plot_states_us” function within the clinical trials data platform is designed to generate a colour-coded map visualizing the distribution of clinical trials across the various states of the United States. It acts as an analytical tool for professionals who need to understand the geographic distribution of clinical trials within the U.S. quickly.


**Feature 5: Top 10 Investigated Diseases Pie Chart**

 It is a graphical representation that highlights the diseases most frequently studied in clinical trials. It categorizes and sums the occurrences of each disease or medical condition within the database and then displays the ten most prevalent as segments of a pie chart. By ranking diseases according to the number of associated studies and showcasing the top 10, the chart offers a clear, immediate depiction of the current research emphasis within the clinical trial landscape. Each pie segment is colour-coded and labelled with a percentage, and a corresponding legend provides a quick reference, making the chart an efficient tool for identifying predominant areas of medical research.


**Feature 6: Link to Website**

In this function, a new column link is created by concatenating the URL with the NCT ID(National Clinical Trial identifier) to form a hyperlink. Then further modify the link column so that only the NCT ID is displayed if the URL is not available. The function then selects and renames the specific column to be displayed in the final table: the newly created hyperlink under the NCT ID, along with the brief title, start date, and finish date. This results in a table where NCT IDs are clickable links that direct users to their respective study pages, providing an easy and direct route to more detailed information about each clinical trial.





