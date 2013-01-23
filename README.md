BayesMed
========

This R package can be used to perform a default Bayesian hypothesis test for mediation, correlation, and partial correlation, either analytically or through the Savage-Dickey method (Dickey & Lientz, 1970). All tests make use of a Jeffreys-Zellner-Siow prior set-up (Liang et al., 2008).

The main functions jzs_med and jzs_medSD can be used to establish mediation in a data set. With jzs_cor and jzs_corSD you can establish correlation, and with jzs_partcor and jzs_partcorSD partial correlation.