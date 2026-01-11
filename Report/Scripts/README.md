# Scripts notes

The scripts in this folder are all the scripts I used to make my analyses, figures, and plots.

```         
Scripts/
├── Utils/
│   ├── Plot.R       # Make plots
│   ├── Predictor.R  # Define function that makes predictions on data scale
│   ├── Table1.R     # Make table 1
│   └── Table2.R     # Make table 2
└── Analysis.R       # Fit model
```

All the scripts in `Scripts/Utils/` start by sourcing `Scripts/Analysis.R`.

## Running the scripts

-   Make sure you have packages installed
-   The argument `data_path` in `Scripts/Analysis.R` needs to point to the data for all scripts (including those in `Utils/`) to work

You can use `make_pred` in the console with the calls

```{r}
make_pred(as.numeric(summary(df$tscent)[2]))
make_pred(as.numeric(summary(df$tscent)[5]))
make_pred(as.numeric(summary(df$tscent)[5])) - make_pred(as.numeric(summary(df$tscent)[2]))
```

To generate the example data I used in the report

Note that since each script clears variables before running, you need to run `Scripts/Utils/Predictor.R` again to use `make_pred`.