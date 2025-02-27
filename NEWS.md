The source code of the `RPC` package is available for download from [GitHub](https://www.github.com/Blue-Matter/RPC). The App is also hosted online at https://shiny.bluematterscience.com/app/rpc.

## version 0.4.2

- Ensure `OM@SRrel` is numeric when the app uses `hist_SRR_change` to update the stock-recruit relationship.
- Add `spawn_time_frac` into per recruit and yield curve calculations, requires MSEtool 3.6.2
- Update yield curve function for MSEtool > 3.7.3

## version 0.4.1

- Check when `MSE@Misc$extended` is empty when calculating surplus production in projections. 
- Revert so that `extended = TRUE` in shiny app (introduced in 0.4.0)
- Update deprecated arguments, use geom_line with linewidth argument.

## version 0.4.0

- Forwards compatibility for MSEtool 3.6.1
- Remove 'beta' designation.
- Grid lines with plots.

## version 0.3.0

- Initial SSB0 is re-defined as the equilibrium spawning biomass calculated from the biological parameters in the first year of the model. Previously, it was defined as the first year biomass, but not all operating models start at unfished in year 1.

## version 0.2.1-2

- Update iSCAM OMs which now import MCMC output.
- Years added to surplus production phase plot.

## version 0.2.0

- Beta version presented to working group.

## version 0.1.0

- Alpha version presented to working group.
