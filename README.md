# COV_AB
Marc Cadotte

The function COV.AB.SES calculates the standardized effect sizes for species linear trend over time and for the average covariance with the other species in a community.

It requires a time(year-rows) x species (columns) matrix 

The output is a list that includes a data.frame with average abundance trend, it's SES, average covariance, it's SES, and species variance. The second element of the list is the covariance matrix with diagonal set to 0.

The analysis script runs this function on all control plots for the Nutrient Network data https://nutnet.org/

