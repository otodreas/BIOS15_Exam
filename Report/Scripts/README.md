# Scripts notes

## `MuMIn` package

-   Supressed warning from `MuMIn` package because the variables used in the original model call have the same values as when the model was fitted
-   In the variable `trigamma_pseudo_r2`, the matrix consists of two columns
    -   *Marginal* $R^2_{GLMM}$ values are variance explained only by the fixed effects (in our case `height`)
        -   "The model linking fruit number to plant height without accounting for random effects explained `r2 * 100 %` of the variance in fruit number ($pseudo-R^2 =$ `r2`)"
    -   *Conditional* $R^2_{GLMM}$ values are variance explained by the entire model
        -   "The model linking fruit number to plant height and covariates population of origin and experimental block explained `r2 * 100 %` of the variance in fruit number ($pseudo-R^2 =$ `r2`)"
-   I used the *Trigamma*-estimate, as recommended by the authors