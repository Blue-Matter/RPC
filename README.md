# Reference Point Calculator (RPC)

[![DOI](https://joss.theoj.org/papers/10.21105/joss.05418/status.svg)](https://doi.org/10.21105/joss.05418)

Reference Point Calculator is an online App for exploring and deriving limit and target reference points for Canadian fisheries. Use of RPC falls into two general categories:
- Visualize the historical dynamics of fish stocks and fisheries to investigate and determine appropriate reference points  
- Design and test management procedures and measure their performance against reference points and other objectives

RPC is intended to be flexible to account for the unique biology of each exploited species and associated management measures. The App includes a variety of Canadian (and other international) case studies and aims to expand with increased use.

RPC is coded in R using [Shiny](https://shiny.rstudio.com/).

## Deployment

The App is available online at the Blue Matter website: https://shiny.bluematterscience.com/app/rpc/

By installing the package, the App can also be loaded in a local R session:

```
devtools::install_github("Blue-Matter/RPC")
RPC::RPC()
```

## User Manual

The user manual is available online at: https://blue-matter.github.io/openMSE/RPC-User-Guide.html

## Development

Feature requests and bug reports can be submitted as a issue in the Github repository: https://github.com/Blue-Matter/RPC/issues
