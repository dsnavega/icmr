icmr: Inductive Confidence Machine for Regression
=================================================

This packages provides an implementation of an Inductive Confidence
Machine for Regression based on Ridge Regression. The *telos* of this
package is to create a regression model that provides point estimates
and reliable predictive/confidence intervals with minimal assumptions,
which is particularly useful in domains. where the degree of uncertainty
of the prediction is as important as the prediction itself (point
estimate). Consult the list references for more details on conformal
prediction.

Install:
--------

    devtools::install_github(repo = "dsnavega/icmr")

Usage
-----

``` r
# Build a 'icmr' object
icmr.object <- icmr(x = x, y = y, control = icmr.control())

# Obtaining a prediction
prediction <- predict(object = icmr.object, newdata = z, alpha = 0.05)
```

In the **Usage** example \``x` is assumed to be a matrix of numeric
values and `y` a vector of numeric data. If `NA` values are present in
`x` by default the algorithm will process the data with standardization
and perform a fast mean imputation of missing values. This can be
overridden by providing `process = F` as an argument to the
`icmr.control()` function. In that case the user should apply is own
pipeline for pre-process the data and impute missing values.

References that inspired this implementation:
---------------------------------------------

1.  Balasubramanian VN, Ho S-S, Vovk V (2016) Conformal Prediction for
    Reliable Machine Learning
2.  Papadopoulos H (2012) Inductive Conformal Prediction: Theory and
    Application to Neural Networks. Tools Artif Intell.
    <a href="https://doi.org/10.5772/6078" class="uri">https://doi.org/10.5772/6078</a>
3.  Papadopoulos H (2015) Cross-Conformal prediction with ridge
    regression. In: Lecture Notes in Computer Science (including
    subseries Lecture Notes in Artificial Intelligence and Lecture Notes
    in Bioinformatics). Springer Verlag, pp 260–270
4.  Norinder U, Carlsson L, Boyer S, et al (2014) Introducing Conformal
    Prediction in Predictive Modeling. A Transparent and Flexible
    Alternative To Applicability Domain Determination. J Chem Inf Model
    54:1596–1603.
    <a href="https://doi.org/10.1021/ci5001168" class="uri">https://doi.org/10.1021/ci5001168</a>
5.  Papadopoulos H, Vovk V, Gammerman A (2007) Conformal prediction with
    neural networks. Proc - Int Conf Tools with Artif Intell ICTAI
    2:388–395.
    <a href="https://doi.org/10.1109/ICTAI.2007.47" class="uri">https://doi.org/10.1109/ICTAI.2007.47</a>
6.  Wang D, Wang P, Shi J (2018) A fast and efficient conformal
    regressor with regularized extreme learning machine. Neurocomputing
    304:1–11.
    <a href="https://doi.org/10.1016/j.neucom.2018.04.012" class="uri">https://doi.org/10.1016/j.neucom.2018.04.012</a>
7.  Norinder U, Carlsson L, Boyer S, Eklund M (2015) Introducing
    conformal prediction in predictive modeling for regulatory purposes.
    A transparent and flexible alternative to applicability domain
    determination. Regul Toxicol Pharmacol 71:279–284.
    <a href="https://doi.org/10.1016/j.yrtph.2014.12.021" class="uri">https://doi.org/10.1016/j.yrtph.2014.12.021</a>
8.  Shafer G, Vovk V (2007) A tutorial on conformal prediction
9.  Papadopoulos H, Haralambous H (2011) Reliable prediction intervals
    with regression neural networks. Neural Networks 24:842–851.
    <a href="https://doi.org/10.1016/j.neunet.2011.05.008" class="uri">https://doi.org/10.1016/j.neunet.2011.05.008</a>
10. Papadopoulos H (2015) Cross-Conformal prediction with ridge
    regression. In: Lecture Notes in Computer Science (including
    subseries Lecture Notes in Artificial Intelligence and Lecture Notes
    in Bioinformatics). Springer Verlag, pp 260–270
