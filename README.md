# Reference Point Calculator (RPC)

Reference Point Calculator is an online App for exploring and deriving limit and target reference points for Canadian fisheries. Use of RPC falls into two general categories:
- Visualize the historical dynamics of fish stocks and fisheries to investigate and determine appropriate reference points  
- Design and test management procedures and measure their performance against reference points and other objectives

RPC is intended to be flexible to account for the unique biology of each exploited species and associated management measures. The App includes a variety of Canadian (and other international) case studies and aims to expand with increased use.

RPC is coded in R using [Shiny](https://shiny.rstudio.com/).

## Deployment

The App is available online at the Blue Matter website: https://apps.bluematterscience.com/RPC/

By installing the package, the App can also be loaded in a local R session:

```
devtools::install_github("Blue-Matter/RPC")
RPC::RPC()
```

## User Manual

The user manual is available online at: https://blue-matter.github.io/openMSE/RPC-User-Guide.html

## Development

RPC is currently in beta testing as features are added and the interface is improved from feedback from end users. Feature requests and bug reports can be submitted as a issue in the Github repository: https://github.com/Blue-Matter/RPC/issues
