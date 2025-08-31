# V1.3
## Plots
see https://lgraz.com/wsl--prs-analysis/notebooks/noise-plots-preview.html

## Removed "LOC" Variables
Updated tables: (Markdown text format to be converted into Word)

also available from https://lgraz.com/wsl--prs-analysis/#results

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


## Prediction analysis
I left it as-is. Without changing the mediators. Removing most mediators would make the ML-models struggle a lot.

## Plotting Themes
Color palettes and themes can be adjusted. Feel free to chose one:
- Themes: https://ggplot2.tidyverse.org/reference/ggtheme.html
- Pallets: https://ggplot2-book.org/scales-colour.html#brewer-scales

## Access to Repository for Natalia
https://github.com/LGraz/wsl--prs-analysis

In case you are not familiar with GitHub + Quarto, feel free to contact me. I can help you to set it up, and show you how to modify the plots in the most sustainable/efficient way.



# V1.4
- [x] removed "In general"
- [x] Implemented comments from email-attachement
  - Comment: Initially I used HM_noise since you were looking at those HM_Noise-groups (in the big table we replaced with this plot)
  - Implementing the fixed scaling does not change much for plot "Sound_Type" or "Attribute," but only in "Annoyance". There you nicely see the annoyance of "Road Traffic". Only the very slight increase of low-annoyance levels is hidden.
  - The y label now includes the range. e.g. "[0-10]"
- "I think it is enough to mention HM Noise only in the plots HM–RL for NDVI and noise level. They illustrate that people living at noisy places are s well unprivileged with available RLs."
  - Please explain what I should do here. You just say that the NDVI-version is less interesting. Or do you want me to switch the x/y axis (right now x-axis is HM_ which is nice to answer questions of the type, do high-HM_ participants have good restorative locations)? 
