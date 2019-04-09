## presentation notes

### bmb

- as mentioned in class, your t-test of pre-treatment differences is both fishy (you seem to have mean-standardized both groups *before* comparing!) and unnecessary: e.g. see https://www.ncbi.nlm.nih.gov/pmc/articles/PMC4310023/
- are you doing a t-test for survival? in general, you should use a GL(M)M with binomial responses for survival
- I forget what all of your predictors are, but using sum-to-zero contrasts where appropriate will probably make things easier to interpret
- I'd like to see some plots of raw data and predictions: one of my gripes about the effects plot is that it hides the data completely (this isn't always easy)

This is one way: https://stackoverflow.com/questions/37333487/overlay-2-alleffects-graphs

The `sjPlot` package similarly works but requires extra effort to show predictions + data
