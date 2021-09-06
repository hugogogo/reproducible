# Reproducible code for natural / organic lasso 

The folder contains code for reproducing numerical studies in [Yu & Bien (2019) *Estimating the error variance in a high-dimensional linear model*](https://academic.oup.com/biomet/article/106/3/533/5498375).

The package `natural` contains the implementation of the proposed methods. See [this](https://github.com/hugogogo/natural) for installation instructions.

This folder contains:

- lambda_cv: contains codes to run simulation studies for all methods using cross-validation. "plot_for_paper.R" generate Figure 1 in the main paper and Figure 1 in the supplement. See the comments of the code for details.

- lambda_fixed: contains codes to run simulations for all methods using a pre-specified value of tuning parameter. "plot_for_paper.R" generate Figure 2 in the main paper and Figure 2 in the supplement. See the comments of the code for details.

- paired_tests.R: used to run paired tests for the significance of the performance difference among methods. It generates Table 1 in the supplement.

- MSD_study: contains code and data for Section 6 (Million Song dataset). "MSD.R" runs the numerical study, and "table_for_paper.R" generates Table 1 in the paper and Table 2 in the supplement. The dataset is publicly available [here](https://archive.ics.uci.edu/ml/datasets/yearpredictionmsd)

Simulation studies depend on `simulator` [Bien (2016) * The Simulator: An Engine to Streamline Simulations](https://arxiv.org/abs/1607.00021).
