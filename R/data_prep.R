options(xfun.cache_rds.clean = FALSE)

library(readxl)
D <- D_orig <- read_excel("data/df_for_lukas.xlsx")
D_varlookup <- read_excel("data/df_varlookup_for_lukas.xlsx")

D_types_orig <- sapply(D_orig, class)
D_types_lookup <- setNames(D_varlookup$vartype, D_varlookup$variable)

# check if all variables in D_orig are in D_varlookup & vice versa
# names(D_types_orig)[which(!(names(D_types_orig) %in% names(D_types_lookup)))]   # NO !!!
# names(D_types_lookup)[which(!(names(D_types_lookup) %in% names(D_types_orig)))] # yes

# # some variables are duplicated in D_varlookup
# D_types_lookup[duplicated(names(D_types_lookup))]


# SUBSET OF DATA
# Define categories of variables to include
# Mediators
Mediator_vars <- c(
  "FEELNAT", # values: [1-7]
  "LNOISE", # LNOISE values: [1-5]
  "LOC_SENS", # SENS = aggregated( LOC_... values: [1-5]) > using mean (?)
  "LOC_SOUN", 
  "LOC_SCEN", 
  "LOC_VISE", 
  "LOC_VEGE", 
  "LOC_FAUN")
PRS_orig_vars <- c(
  "LQUAL1",
  "LQUAL2",
  "LQUAL3",
  "LQUAL4",
  "LQUAL5",
  "LQUAL6",
  "LQUAL7",
  "LQUAL8",
  "LQUAL9",
  "LQUAL10",
  "LQUAL11")

GIS_vars <- c( 
  sqrt="LCARTIF",
  sqrt="LCFOREST",
  "HETER",
  sqrt="OVDIST",
  sqrt="VIS5K",
  "RL_NDVI",
  "RL_NOISE",
  # "HM_NDVI", # Include myself (i.e. not in varlookup), correct?
  # "HM_NOISE",
  sqrt="DISTKM",
  sqrt="JNYTIME", # outlier
  sqrt="STRIMP123",
  sqrt="STRIMP999"
)

Moderator_vars <- c("HM_NOISE")

# dictionary:
Dicts <- list(
  dict1 = c(
    "01_SDisagr" = 1,
    "02_Disagr" = 2,
    "03_RDisagr" = 3,
    "04_Neither-nor" = 4,
    "05_RAgree" = 5,
    "06_Agree" = 6,
    "07_SAgree" = 7),
  dict2 = c(
    "01_SDisagr" = 1,
    "02_Disagr" = 2,
    "03_RDisagr" = 3,
    "04_Neither-nor" = 4,
    "05_RAgree" = 5,
    "06_Agree" = 6,
    "07_SAgree" = 7),
  dict3 = c(
    "Strongly disagree" = 1,
    "Rather disagree" = 2,
    "Neither agree nor disagree" = 3,
    "Rather agree" = 4,
    "Strongly agree" = 5),
  dict4 = c(
    "Very" = 5,
    "Quite" = 4,
    "Fair" = 3,
    "Little" = 2,
    "Not" = 1),
  dict5 = c(
    "01_NotDom" = 1,
    "02_RNotDom" = 2,
    # no 3 in data
    "04_RDom" = 4,
    "05_VeryDom" = 5),
  dict6 = c(
    "Very bad" = 1,
    "Rather bad" = 2,
    "Neither good nor bad" = 3,
    "Rather good" = 4,
    "Very good" = 5),
  dict7 = c(
    "Not dominant" = 1,
    "Rather not dominant" = 2,
    "Neither dominant nor non-dominant" = 3,
    "Rather dominant" = 4,
    "Very dominant" = 5),
  dict8 = c(
    "01_VLittle" = 1,
    "02_RLittle" = 2,
    "03_Neither-nor" = 3,
    "04_RMuch" = 4,
    "05_VMuch" = 5),
  dict9 = c(
    "01_SDisagr" = 1,
    "02_RDisagr" = 2,
    "03_Neither-nor" = 3,
    "04_RAgree" = 4,
    "05_SAgree" = 5),
  dict10 = c(
    "yes" = TRUE,
    "no" = FALSE),
  dict11 = c(
    "Yes" = TRUE,
    "No" = FALSE),
  dict12 = c(
    "01_VBad" = 1,
    "02_RBad" = 2,
    "03_Neither-nor" = 3,
    "04_RGood" = 4,
    "05_VGood" = 5),
#  [1] "00" "02" "01" "03" "07" NA   "04" "09" "08" "05" "10" "06"
  dict13 = c(
    "00" = 0,
    "01" = 1,
    "02" = 2,
    "03" = 3,
    "04" = 4,
    "05" = 5,
    "06" = 6,
    "07" = 7,
    "08" = 8,
    "09" = 9,
    "10" = 10),
# [1] "2-3xMonth"   "More1xWeek"  "Less1xMonth" "7xWeek"      NA           
# [6] "1xWeek" 
  dict14 = c(
    "Less1xMonth" = 1,
    "2-3xMonth" = 2,
    "1xWeek" = 3,
    "More1xWeek" = 4,
    "7xWeek" = 5)
)

# variable HSANNOY1 has nlevels: 12"
#  [1] "Response scale mid-point" "3"                       
#  [3] "7"                        "No annoyance"            
#  [5] "8"                        NA        vars_w                
#  [7] "1"                        "2"                       
#  [9] "Very annoying"            "9"                       
# [11] "6"                        "4"                                        
# variable LIFESAT has nlevels: 12"
#  [1] "7"                    "Completely satisfied" "8"                   
#  [4] "6"                    "9"                    NA                    
#  [7] "1"                    "Scale midpoint"       "4"                   
# [10] "3"                    "2"                    "Not at all satisfied"
# variable HEALTH has nlevels: 6"
# [1] "4"              "Very good"      "Scale midpoint" NA              
# [5] "Very poor"      "2"             
# variable STRTOL has nlevels: 6"
# [1] "4"                      "2"                      "Scale midpoint"        
# [4] NA                       "I can shake stress off" "Stress gnaws at me"    
# variable PSTRESS has nlevels: 6"
# [1] "4"                 "2"                 "Scale midpoint"   
# [4] NA                  "No stress"         "Very great stress"
# variable WKSTRESS has nlevels: 6"
# [1] "Scale midpoint"    "No stress"         "2"                
# [4] "Very great stress" NA                  "4"                


# get a list of all variables that have less then 9 unique values and print them
vars_w_few_lvls <- names(D)[sapply(D, function(x) length(unique(x)) < 9)]
list_w_few_lvls <- lapply(D[vars_w_few_lvls], unique)

# check if all values are in our dictionary
stopifnot(all(
  (D_orig[grep("LQUAL", names(D), value = TRUE)] |> as.matrix() |> table() |> names() == names(Dicts$dict1))
))

# check all variabels in vars_w_few_lvls if the values are in some dictionary. If so, replace the values with the dictionary values
for (varnam in vars_w_few_lvls){
  found <- FALSE
  for (dict in Dicts){
    if (all(names(dict) %in% list_w_few_lvls[[varnam]])){
      found <- TRUE
      D[[varnam]] <- unname(dict[D_orig[[varnam]]])
      break
    }
  }
  if (interactive() && !found){
    print(paste("Warning: no dictionary found for variable", varnam))
    print(list_w_few_lvls[[varnam]])
    }
}

D_types <- sapply(D, class)
for (varnam in names(D_types_lookup)){
  vartype <- D_types_lookup[varnam]
  if (vartype == "GIS") 
    next
  # if types not equal print warning
  if (interactive() && D_types[varnam] != vartype){
    print(paste("Warning: variable", varnam, "has type", D_types[varnam], "but should be", vartype, "  nlevels:", length(unique(D[[varnam]]))))
    if (D_types[varnam] == "character"){
      print(unique(D[[varnam]]))
    }}
}


# FILTERING --------------------
# 1. FEELNATURE and NOISEANNOY_1 and HM/RL_NDVI and HM/RL _NOISE exclude
#NULL!
# 2. ACTIVITY exclude NA and 6 (no outdoor activity in the past 4 weeks)
# 3. HEADPHNE exclude 2 and 3 (wearing headphones)
# 4. MAPDATA exclude 0 (not mapped RL)
# 5. DISTKM excludes values above of the 90th percentile (extremely large values) and
# 6. DURATION excludes values above of the 90th percentile (extremely large values)
# 7. JOURNEY TIME excludes values above of the 99th percentile (extremely large values)
# 8. HM_NOISE exclude NA
# __Database Cleaning
KeepDF <- with(D,
  data.frame(
      #  !is.na(ACTIVITY) &
      #  !grepl('I have never been outside', ACTIVITY) &
    Headphone = !is.na(HEADPHNE) & HEADPHNE == "No",
    Distance = DISTKM <= quantile(DISTKM, 0.9, na.rm=TRUE),
    Duration = REC_DUR <= quantile(REC_DUR, 0.95, na.rm=TRUE) | is.na(REC_DUR),
    JourneyTime = JNYTIME <= quantile(JNYTIME, 0.99, na.rm=TRUE) | is.na(JNYTIME),
    HMNoise_NA = !is.na(HM_NOISE),
    Activity_NA = !is.na(D$REC_ACT),
    PRS_all_NA = apply(D[PRS_orig_vars], 1, \(x)sum(is.na(x))<11)
  )
)
# cor(KeepDF) |> round(1) 
##-> Distance, Duration, JourneyTime are correlated (~.2) - no surprise
##-> PRS_ALL_NA and Headphone is strongly correlated (.6)

cat("Nr of matches per filtercriteria (not disjoint)\n")
colSums(!KeepDF) |> sort(decreasing = TRUE) |> print()

KeepInd <- apply(KeepDF, 1, all)
cat("Keep ", sum(KeepInd), "of", nrow(D), "observations\n")
D <- subset(D, KeepInd)
# END FILTERING --------------------

# TRANSFORMATIONS
# distances(+ road lengths), time and proportions will be sqrt-transformed
for (i in seq_along(GIS_vars)){
  varnam <- GIS_vars[i]
  if (names(GIS_vars)[i] == "sqrt"){
    new_varnam <- paste0(varnam, "_sqrt")
    D[new_varnam] <- sqrt(D[varnam])
    GIS_vars[i] <- new_varnam
  }
}
GIS_vars <- unname(GIS_vars)


library(missForest)
# PRS imputation (missForest)
message("Impute PRS_orig_vars")
D[PRS_orig_vars] <- xfun::cache_rds({
  missForest(as.matrix(D[PRS_orig_vars]))
  }, 
  file = "PRS_imputation.rds", dir = "cache/",
  hash = list(as.matrix(D[PRS_orig_vars]))
)$ximp |> as.data.frame()



# PCA
pca. <- prcomp(scale(D[PRS_orig_vars], scale=FALSE))
# round(pca.$rotation, 2)
#' SEE COMMENTS IN index.qmd

X_centered <- D[PRS_orig_vars] |> scale(center=TRUE, scale=FALSE) |> as.matrix()
X_reduced <- X_centered %*% pca.$rotation[,1:4]

# cor(rowMeans(X_centered), X_centered %*% pca.$rotation[,1])
#= 0.9996  -> 1st PC is really just a weighted average of all variables

# check if all responses have roughly the same mean and sd
if(interactive())
  with(D, cbind(LQUAL1,LQUAL2,LQUAL3,LQUAL4,LQUAL5,LQUAL6,LQUAL7,LQUAL8,LQUAL9,LQUAL10,LQUAL11)) |> as.data.frame() |>
    sapply( \(x) c(mean=mean(x, na.rm=TRUE), sd=sd(x, na.rm=TRUE))) |> round(2)
# ->> yes

D$LA <- rowMeans(D[, c("LQUAL1", "LQUAL2", "LQUAL3")], na.rm=TRUE)
D$BA <- rowMeans(D[, c("LQUAL4", "LQUAL5", "LQUAL6")], na.rm=TRUE)
D$EC <- rowMeans(D[, c("LQUAL7", "LQUAL8", "LQUAL9")], na.rm=TRUE)
D$ES <- rowMeans(D[, c("LQUAL10","LQUAL11")], na.rm=TRUE)

D[c("PC1", "PC2", "PC3", "PC4")] <- as.data.frame(X_centered %*% pca.$rotation[,1:4])
PRS_vars <- c("LA","BA","EC","ES", "PC1", "PC2", "PC3", "PC4")

message("TODO: remove pca ?")


# before prediction machines, use missForest to impute NAs
message("impute mediators & GIS_vars for mlr")
Dmlr <- D
# mediator imputation (missForest)
Dmlr[Mediator_vars] <- xfun::cache_rds({
  missForest(as.matrix(Dmlr[Mediator_vars]))
  }, 
  file = "Mediator_imputation.rds", dir = "cache/",
  hash = list(as.matrix(Dmlr[Mediator_vars]))
)$ximp |> as.data.frame()

# GIS imputation (missForest)
Dmlr[GIS_vars] <- xfun::cache_rds({
  missForest(as.matrix(Dmlr[GIS_vars]))
  }, 
  file = "GIS_imputation.rds", dir = "cache/",
  hash = list(as.matrix(Dmlr[GIS_vars]))
)$ximp |> as.data.frame()



# Train / Test
set.seed(123)
train_ind <- sample(1:nrow(D), round(nrow(D) * 0.5))
D_trn <- D[ train_ind,]
D_tst <- D[-train_ind,]
