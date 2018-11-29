# Changes in Humanities Enrolments in NZ Tertiary Institutions

This is the repo for the data analysis and visualisations included in a story published by RNZ's [In Depth section](https://www.radionz.co.nz/news/in-depth) on October 11, 2018.

It contains the Education Ministry data and code I used to analyse changes to enrolments in humanities majors at New Zealand tertiary institutions, focusing on those studying at degree level or higher. It also contains code for the visualisations that accompanied the final story.

- [Data](#data)
- [Analysis](#analysis)
- [Feedback](#feedback)

### Prerequisites

You'll need the latest versions of [R](https://www.r-project.org/) and [R Studio](https://www.rstudio.com/).
This project was created with R version 3.5.1.

## Data: Provider-based-enrolments-predominant-field-of-study-2017-final.xlsx

The analysis and charts are based on [data](https://www.educationcounts.govt.nz/statistics/tertiary-education/participation) collected by New Zealand's Education Ministry and published on the ministry's statistics and research site, [educationcounts.govt.nz](educationcounts.govt.nz).

## Analysis

|File|Details|
|---|---|
|01-humanities-analysis.Rmd|R Markdown containing the R code used to import and analyse the Education Ministry data, and create the draft versions of the charts included in RNZ's reporting|
|01-humanities-analysis.html|HTML output of the Rmd file|
|humanities-exploration.R|Initial R script used to explore the data|

---

## Feedback

I am a new R user and still learning so would welcome feedback on the code, which contains elements I am sure are inefficient, or any other aspect of this repo.
[I'm a journalist at Radio New Zealand (RNZ)](https://www.radionz.co.nz/authors/kate%20-newton) - you can contact me at kate.newton@rnz.co.nz
