
# Code for development of credit risk stages according to IFRS9 standards

<!-- badges: start -->
<!-- badges: end -->

The code uses data that is already preprocessed and only adds few new columns. It first develops 2 models, one for
observations with CRC data (the majority) and one for observations without CRC data, since CRC data is
the best available information for modelling. For the first 2 models default is one year default data. The most important output of the first 2 models are thresholds for stage 2 - PD's greater than these thresholds will send
a beneficiary to stage2
After the first 2 models are ran, the data is filtered for beneficiaries in stage2 and another 2 models are run, this time the default being 3 year PD's - 3 year being the proxy for lifetime PD.
Final provisions are calculated according to the stage of a beneficiary and given coefficients (not produced here).

## Running the code

Forking this repository will not work since the data is private and not available. A docker image is being built with github actions and stored on public github repository (docker pull ghcr.io/fizic37/ifrs:sha-f3a8aed)



