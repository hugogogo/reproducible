# reproducible code for sprinter

The folder contains code for reproducing numerical studies in [Yu, Bien, and Tibshirani (2019) *Reluctant Interaction Modeling*](https://arxiv.org/abs/1907.08414).

To install `sprintr` from [github](http://github.com), type in R console
```R
devtools::install_github("hugogogo/sprintr", build_vignettes = TRUE)
```
Note that the installation above requires using R package [devtools](https://CRAN.R-project.org/package=devtools)
(which can be installed using `install.packages("devtools")`).

Simulation studies heavily depend on `simulator` [Bien (2016) * The Simulator: An Engine to Streamline Simulations](https://arxiv.org/abs/1607.00021).
