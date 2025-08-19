# Removed "LOC" Variabels

Updated tables: (markdown text format to be converted into Word)

|Covariate            | FEELNAT  |  LNOISE   |
|:--------------------|:--------:|:---------:|
|(Intercept)          |  0.062   |  -0.001   |
|LCARTIF_sqrt         | -0.152** |  -0.124*  |
|LCARTIF_sqrt:RL_NDVI | 0.115**  |           |
|OVDIST_sqrt          |  0.027   |           |
|RL_NDVI              | 0.150*** |           |
|RL_NOISE             |          | -0.242*** |

|Covariate      |   MEAN   |    FA    |    BA    |    EC    |    ES    |
|:--------------|:--------:|:--------:|:--------:|:--------:|:--------:|
|(Intercept)    |  -0.005  |  0.006   |  -0.008  |  -0.006  |  0.027   |
|DISTKM_sqrt    |  -0.019  |          |          |          |  0.073.  |
|FEELNAT        | 0.252*** | 0.266*** | 0.252*** |  0.058   | 0.270*** |
|FEELNAT:LNOISE |  0.022   |  -0.036  |          |          |          |
|LCFOREST_sqrt  | -0.066.  |          |          | -0.114** |          |
|LNOISE         | 0.212*** | 0.170*** |          |          | 0.142**  |
|LNOISE:FEELNAT |          |          |          |          |  -0.013  |
|RL_NDVI        |          | -0.105** |          |          |          |


# Added  LNOISE ~ HM_NOISE / RL_NOISE
(including proportions)

# Prediction analysis
I left it as-is. Without changing the mediators. Removing most mediators would make the ML-models struggle a lot.

# Plotting Themes
Colour palettes and themes can be adjusted. Feel free to chose one:
- Themes: https://ggplot2.tidyverse.org/reference/ggtheme.html
- Palletes: https://ggplot2-book.org/scales-colour.html#brewer-scales

# Plots
see 
