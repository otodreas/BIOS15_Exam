# Scripts notes

## Running the script

-   Make sure you have packages installed
-   Note that input files are read from the `Data` folder and output files are written to the `Output` directory, so if you didn't clone the entire repo and just downloaded the file, you will need to change those filepaths
    -   The data file can be found at `root/Report/Data/penstemon_copy.txt`

You can use `make_pred` in the console with the calls

```{r}
make_pred(as.numeric(summary(df$tscent)[2]))
make_pred(as.numeric(summary(df$tscent)[5]))
make_pred(as.numeric(summary(df$tscent)[5])) - make_pred(as.numeric(summary(df$tscent)[2]))
```

To generate the results I did in the report