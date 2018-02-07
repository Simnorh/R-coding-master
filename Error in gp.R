#When running the "changing functions for exp and gp" script as i have currrently set it up
# an error occurs for some stations e.g. 2.13 2.32 2.132 and more never tested for all just saw the error recurring several times when testing around 30-50 rivers
# it gives the error:
#Error in if (any(U < 0 | U > 1)) stop("null distribution function returned values outside [0,1]") : 
#  missing value where TRUE/FALSE needed
#In addition: Warning messages:
#  1: Deprecated: please use `purrr::possibly()` instead 
#2: In log(1 - k * (x - xi)/alfa) : NaNs produced
#[1] "Warning: gof_ad has failed with distr..."
#
#which i have traced back to the function goftest::ad.test (first line of the error message which is the first place it fails)
#
#the stations i've mentioned so far have varying amounts of datapoints from 50-150
#i get some similar error messages for the exp distribution but those only say something like this:
#[1] "Warning: gof_ad has failed with distr..." which is from the coding you did.
