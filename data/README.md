- `final_IncludedIndecies.csv` -- which indices are kept from `df_for_lukas.xlsx`
- `final_Dataset_no_PRS_imputation.csv` -- `df_for_lukas.xlsx` with only kept indices from `final_IncludedIndecies.csv`, and translate ordinal items into integers
- `final_Dataset.csv` -- `final_Dataset_no_PRS_imputation.csv` but including the missForest imputation for PRS and Aggregate FA, BA, EC, ES, PRS from corresponding items
  - `final_Dataset_feature_selection.csv` -- train-subset of `final_Dataset.csv` used for stepwise feature (a.k.a. variables) selection
  - `final_Dataset_test_set.csv` -- test-set used for testing the linear models on beforehand selected features
- `final_Dataset_for_MLR_imputed-GIS-Mediators.csv` -- `final_Dataset.csv` but with additional imputed GIS and Mediator variables. This is used for our prediction (or machine learning) analysis. 

`final_Dataset_for_MLR_imputed-GIS-Mediators.csv` is the most processed data set of all mentioned. But we shall not use it for our linear-model feature selection procedure, as treating the imputation of GIS/Mediator variables correctly (for valid p-values) would have increased the complexity, which was not desired. 
