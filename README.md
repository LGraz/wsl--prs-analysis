- ACTIVITY missing


> quantile(D$REC_DUR, 0.9, na.rm=TRUE)
90% 
200 
> quantile(D$REC_DUR, 0.95, na.rm=TRUE)
95% 
300 



Filtering:
- remove observations with all PRS vars missing
- Headphone = HEADPHNE == "No" | is.na(HEADPHNE),


Improve imputation by providing more variables connected to 
- PRS
- Mediators
- GIS-vars
each variable would be only allowed to be used for one of the three above
