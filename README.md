# fullhouse (v1.1)

`fullhouse` is an R package containing scripts for performing Full House Modeling and data sets. These data sets include era-adjusted statistics for baseball players and preliminary data for an analysis for historical figures.

## Installation

You can install the development version from GitHub with `devtools`:

```r
#install.packages("devtools")
devtools::install_github(repo = "DEck13/fullhouse")
```


## Era-adjusted baseball statistics

The package includes pre‑computed datasets of era‑adjusted player statistics,
generated via Full House Modeling. To explore these stats:

- Visit our companion website:  
  https://eckeraadjustment.web.illinois.edu/
- View the worked examples in `inst/era_adjusted_V2.1.Rmd`, published here: https://eckeraadjustment.web.illinois.edu/era_adjusted_V2.1.html
- **Watch the full walkthrough video** (line‑by‑line code demo for these examples): [YouTube Video Link](https://www.youtube.com/watch?v=TmZTFWIzJXc)



## Vignette 

[Here is a reproducible technical report](https://htmlpreview.github.io/?https://github.com/DEck13/historical-impact-resources/blob/main/pantheon_fullhouse.html) that uses functions from the `fullhouse` R package in an analysis of the impact of historical figures through the lens of era-adjustment modeling.

This vignette and additional content on era-adjustment modeling for historical impact can be found at [this GitHub repo](https://github.com/DEck13/historical-impact-resources/).


## v1.1 

In this version, the latent talent generating process is modeled using a standard normal distribution (previous versions used a Pareto distribution). This change simplifies computations and aligns with our current implementations. The resulting era-adjusted statistics remain unchanged.

