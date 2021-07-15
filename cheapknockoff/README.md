# reproducible code for cheapknockoff

The folder contains code for reproducing numerical studies in [Yu, Witten, and Bien (2020) *Controlling Costs: Feature Selection on a Budget*](https://arxiv.org/abs/1910.03627).

The package `cheapknockoff` contains the implementation of the proposed method.
To install `cheapknockoff` from [github](http://github.com), type in R console
```R
devtools::install_github("hugogogo/cheapknockoff", build_vignettes = TRUE)
```
Note that the installation above requires using R package [devtools](https://CRAN.R-project.org/package=devtools)
(which can be installed using `install.packages("devtools")`).

Please check the accompanying vignette on how to use the `cheapknockoff` package. To read vignette, after installing the package, type in R console
```R
browseVignettes("cheapknockoff")
```

Simulation studies depend on `simulator` [Bien (2016) * The Simulator: An Engine to Streamline Simulations](https://arxiv.org/abs/1607.00021).
