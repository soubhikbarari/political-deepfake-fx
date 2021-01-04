# Political Deepfake Videos Misinform the Public, But No More than Other Fake Media
## Soubhik Barari, Christopher Lucas, and Kevin Munger

This is the replication code repository for the aforementioned research article (currently a working paper).

- `deepfake.RData`: R object containing cleaned and anonymized results of three waves of the two survey experiments (*exposure* and *detection*) in `dat`, the raw but anonymized survey results in `dfsurvdat`, and clip-level results of the detection experiment.
- `1-weight_data.R`: appends post-stratification weights to `dat` object in `deepfake.RData` using a simple raking model from U.S. CPS data.
- `2-prereg_analyses.R`: replicates analyses specified in pre-analysis plan; generates outputs in `/table` and `/figure` found in Appendix of article.
- `3-prereg_analyses_sensitivity.R`: replicates sensitivity analysis on particular pre-registered specifications.
- `4-prereg_topline_results.R`: summarise pre-registered analysis results in a series of topline tables/figures; this replicates figures found in main text of article.
- `5-exploratory_analyses.R`: replicates exploratory analyses found in Appendix of article.

