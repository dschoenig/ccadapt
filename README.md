# Bayesian analysis of climate change adaptation willingness

This repository implements a fully Bayesian analysis of an online survey
designed to elicit information on climate change adaptation behaviours
of private forest owners in Canada. The survey design is based on
protection motivation theory in relation to climate change adaptation,
as proposed by Grothmann & Patt (2005). Original survey data and
description is available at
[https://doi.org/10.5683/SP3/YYKRDC](https://doi.org/10.5683/SP3/YYKRDC).

Given the information density of the survey (close to 200 contrasts),
the statistical analysis proceeds in three steps:

1. Variable selection for individual adaptation actions
2. Item-response model across all adaptation actions
3. Computation of marginal effects at the response scale

Details are provided in the [methods section](#methods).

## Overview

All raw data, as well as intermediate and final results are already
contained in the repository. Most scripts are designed to be used in a
high performance computing setting, and are thus invoked with arguments.
To fully replicate the analysis, script have to be called in order, and
with the following arguments (see also `src/analsysis.sh`):

```bash
Rscript 0_data_preparation.R

Rscript 1_models_varsel.R 1 willingness 4
Rscript 1_models_varsel.R 2 willingness 4
Rscript 1_models_varsel.R 3 willingness 4
Rscript 1_models_varsel.R 4 willingness 4
Rscript 1_models_varsel.R 5 willingness 4
Rscript 1_models_varsel.R 7 willingness 4

Rscript 1_cv_varsel.R 1 willingness loo 8
Rscript 1_cv_varsel.R 2 willingness loo 8
Rscript 1_cv_varsel.R 3 willingness loo 8
Rscript 1_cv_varsel.R 4 willingness loo 8
Rscript 1_cv_varsel.R 5 willingness loo 8
Rscript 1_cv_varsel.R 7 willingness loo 8

Rscript 2_variable_selection willingness

# Model fitting hard-coded to 32 threads:
Rscript 3_irt_2pl.R willingness

# Validation to be performed manually:
Rscript 3_irt_validation.R willingness

Rscript 4_marginal_effects.R willingness 4
Rscript 4_comparison_actions.R willingness 4
```

## Methods

To explain motivations underlying the willingness to implement adaption
actions in the future, we focused on the subset of respondents who
stated that they owned at least one managed woodlot, had taken part in
the decision-making of these woodlots, and intended to take part in the
decision making in the future (507 of 611 respondents). We coded
responses related to each adaption action (i.e. response items) as
binary response variables, assigning "1" if respondents stated an intent
to implement these actions "within the next five years" or in "six to
ten years", and assigning "0" in case respondents stated that they would
not implement the action, were unlikely to implement it, or did not know
the action. We then proceeded with a Bayesian analysis in three steps:
(1) preliminary variable selection for each adaptation action
individually; (2) joint modelling of adaptation willingness across all
adaptation actions simultaneously; (3) estimation of marginal effects
based on the posterior predictive distribution of the joint model. All
analyses were performed in R (R Core Team, 2024). We used *data.table*
(Barrett et al., 2024) for data processing and *ggplot2* (Wickham, 2016)
for visualization. 

### Variable selection

In the first step, we used a Bayesian generalized linear model for each
adaptation action. The binary response item was assumed to follow a
Bernoulli distribution, and the distribution mean was modelled by a
linear predictor that contained treatment contrasts for each categorical
covariate (i.e. single-choice and multiple-choice questions), and
polynomial contrasts (up to third degree) for each Likert scale
covariate and each quantitative covariate, resulting in a total of 198
coefficients. For treatment contrasts, the reference level referred to
"no" (for binary items), not chosen (for multiple choice items), or the
most frequently picked option (for single-choice questions with multiple
options). Given the high dimensionality of the data, we employed a
regularized horseshoe prior (Piironen & Vehtari, 2017) over all
coefficients of the linear predictor, where the degrees of freedom of
the the student-t prior of the local shrinkage parameters, ν, is set to
3, and the ratio of expected nonzero to zero coefficients to 0.1. Models
were fitted with brms (Bürkner, 2017) and Stan (Stan Development Team,
2024), using No-U-Turn Hamiltonian Markov Chain Monte Carlo (MCMC)
sampling with 4 chains, each set to 7500 warmup iterations, 2500
sampling iterations and a thinning factor of 2, yielding 5000 total
samples of the joint posterior. Sampling was validated via convergence
statistics (Vehtari et al., 2021). We then employed projection
predictive variable selection (Piironen et al., 2017) to select the most
relevant coefficients (i.e. treatment or polynomial contrasts) for each
adaptation action, using forward search and the expected log pointwise
predictive density (ELPD) as performance measure. We determined as
relevant all coefficients that were contained in the smallest model for
which the difference in ELPD to the best model was not further than one
standard deviation away from zero (see
`results/willingness/plots/varsel/var.sel.pdf`).

## Joint modelling of adaptation willingness

For joint modelling we pooled all response items (i.e. adaptation
actions), and all covariate contrasts that were determined relevant for
at least on of the response items. To assess the willingness to
implement adaptation actions we estimated a 2-parameter logistic (2PL)
item response model (Lord, 1980), with varying item discrimination,
partial pooling of item and person parameters, and the effects of
covariate contrasts varying between items, as described in detail by
Bürkner (2021). This results in the following model for the total number
of items *I* and total number of respondents *P*:

$$
\begin{aligned}
y_{ip} \sim & \ \mathrm{Bernoulli}\left(\psi_{ip}\right) \\
\ln{\alpha_{ip}} = & \ a_0 + a_i \\
\mathrm{logit}\,\psi_{ip} = & \ \eta_0 + \alpha_{ip} \eta_{ip} \\
\eta_{ip} = & \ \theta_p + \xi_i + \sum^I_{i=1}\sum^J_{j=1} b_{0j} x_{ijp} + b_{ij} x_{ijp} \\
\left(a_1, ..., a_I, \xi_1, ..., \xi_I, b_{11}, ..., b_{IJ}\right) \sim & \ \mathrm{MVN} \left(0, \mathbf{\Sigma}\right) \\
\mathbf{\Sigma} = & \  \mathbf{D \Omega D} \\
\mathbf{D} = & \ \mathrm{diag}\left(\sigma_{a_1}, ..., \sigma_{a_I}, \sigma_{\xi_1}, ..., \sigma_{\xi_I}, \sigma_{b_{11}}, ..., \sigma_{b_{IJ}}\right) \\
\end{aligned}
$$

where y<sub>ip</sub> denotes the binary response for item *i* by person
*p*, and ψ<sub>ip</sub> denotes the corresponding mean of the Bernoulli
distribution. The *logit* of ψ<sub>ip</sub> is modelled with two
parameters, the response item discrimination α and the linear predictor
η. The former is composed of an intercept a<sub>0</sub> and item-varying
coefficients a<sub>i</sub>; the latter is composed of an intercept
η<sub>0</sub>, respondent-varying coefficients θ<sub>p</sub>, and
item-varying coefficients ξ<sub>i</sub>, as well as population-level
coefficients b<sub>0j</sub> and item-varying coefficients b<sub>ij</sub>
for a total of J covariate contrasts observed for each item and
respondent, x<sub>ijp</sub>. Corresponding standard deviations are
denoted by σ<sub>⋅</sub>. All item-varying coefficients are drawn from a
multivariate normal distribution (MVN), decomposed into a diagonal
variance matrix **D** and a correlation matrix **Ω**.

As suggested by Bürkner (2021), we chose weakly informative regularizing
priors to allow for more efficient exploration of the joint posterior
distribution during MCMC sampling. As α is modelled on the log-scale, we
chose less diffuse priors for a<sub>0</sub> and a<sub>i</sub> than for
the coefficients in η.

$$ \begin{aligned}
a_0 \sim & \ \mathrm{Normal}\left(0, 1\right) \\
\eta_0 \sim & \ \mathrm{Normal}\left(0, 5\right) \\
\theta_p \sim & \ \mathrm{Normal} \left(0, 3\right) \\
b_{0j} \sim & \ \mathrm{Normal} \left(0, 3\right) \\
\sigma_{a_i} \sim & \ \mathrm{HalfNormal}\left(0, 1\right) \\
\sigma_{\xi_i} \sim & \ \mathrm{HalfNormal}\left(0, 3\right) \\
\sigma_{b_{ij}} \sim & \ \mathrm{HalfNormal}\left(0, 3\right) \\
\mathbf{\Omega} \sim & \ \mathrm{LKJ}\left(1\right)
\end{aligned}
$$

where LKJ refers to the LKJ correlation distribution proposed by
Lewandowski et al. (2009).

We used the same MCMC sampler as for the individual models described
above, using 4 chains each with 7500 warmup and 2500 sampling
iterations, resulting in 10000 total samples for the joint posterior. We
again used convergence diagnostics to validate MCMC sampling (Vehtari et
al., 2021), and inspected posterior predictive checks (Gabry et al.,
2019) and quantile residuals (Hartig, 2024) for statistical validation. 


### Marginal effects

To estimate the effect of each covariate included in the item-response
model we calculate fully counterfactual marginal effects: first, we
duplicate the entire data a number of times equal to the number of
unique values for the selected covariate. In each duplicate, the
covariate is set in turn to one of the unique values, while maintaining
all other variables at their observed value. We then join all sets of
duplicated data and calculate the posterior predictive distribution of ψ
(i.e. the willingness to implement an adaptation action) for this joint
dataset via a the joint posterior of the item-response model. Posterior
predictive distributions are than averaged over each unique covariate
value to result in an average ψ per covariate value. These steps are
repeated for each covariate. Finally we compute pairwise differences of
the average ψ between covariate values, and determine the posterior
probability that each difference greater or less than zero. For
subsequent interpretation we focus on covariates this probability ≥ 90%
for at least one of the pairwise differences. Calculated marginal
effects and pairwise comparisons are included (as tables and figures) in
the `results` folder.


## References

Barrett, T., Dowle, M., Srinivasan, A., Gorecki, J., Chirico, M., &
Hocking, T. (2024). *data.table: Extension of 
`data.frame`* [Manual\]. https://CRAN.R-project.org/package=data.table\
Bürkner, P.-C. (2017). **brms**: An *R* Package for Bayesian Multilevel
Models Using *Stan*. *Journal of Statistical Software*, *80*(1).
https://doi.org/10.18637/jss.v080.i01

Bürkner, P.-C. (2021). Bayesian Item Response Modeling in R with brms
and Stan. *Journal of Statistical Software*, *100*, 1--54.
https://doi.org/10.18637/jss.v100.i05

Gabry, J., Simpson, D., Vehtari, A., Betancourt, M., & Gelman, A.
(2019). Visualization in Bayesian workflow. *Journal of the Royal
Statistical Society: Series A (Statistics in Society)*, *182*(2),
389--402. https://doi.org/10.1111/rssa.12378

Grothmann, T., & Patt, A. (2005). Adaptive capacity and human cognition:
The process of individual adaptation to climate change. *Global
Environmental Change*, *15*(3), 199--213.
https://doi.org/10.1016/j.gloenvcha.2005.01.002

Hartig, F. (2024). *DHARMa: Residual diagnostics for hierarchical
(multi-level / mixed) regression models* 
[Manual\].
https://CRAN.R-project.org/package=DHARMa

Lewandowski, D., Kurowicka, D., & Joe, H. (2009). Generating random
correlation matrices based on vines and extended onion method. *Journal
of Multivariate Analysis*, *100*(9), 1989--2001.
https://doi.org/10.1016/j.jmva.2009.04.008

Lord, F. M. (1980). *Applications of item response theory to practical
testing problems*. L. Erlbaum Associates.

Piironen, J., Paasiniemi, M., & Vehtari, A. (2020). Projective inference
in high-dimensional problems: Prediction and feature selection.
*Electronic Journal of Statistics*, *14*(1), 2155--2197.
https://doi.org/10.1214/20-EJS1711

Piironen, J., & Vehtari, A. (2017). Sparsity information and
regularization in the horseshoe and other shrinkage priors. *Electronic
Journal of Statistics*, *11*(2), 5018--5051.
https://doi.org/10.1214/17-EJS1337SI

R Core Team. (2024). *R: A language and environment for statistical
computing* 
[Manual\]. R Foundation for Statistical Computing.
https://www.R-project.org/

Stan Development Team. (2024). *Stan Modeling Language Users Guide and
Reference Manual* (Version 2.31) 
[Computer software\].
https://mc-stan.org

Vehtari, A., Gelman, A., Simpson, D., Carpenter, B., & Bürkner, P.-C.
(2021). Rank-Normalization, Folding, and Localization: An Improved Rˆ
for Assessing Convergence of MCMC (with Discussion). *Bayesian
Analysis*, *16*(2). https://doi.org/10.1214/20-BA1221

Wickham, H. (with Sievert, C.). (2016). *ggplot2: Elegant graphics for
data analysis* (Second edition). Springer international publishing.

