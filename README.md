# SafeMerge

Merging data frames is dangerous! Done incorrectly, it can lead to data that is drastically different than intended, even though no error messages are generated. But determining whether a merge yielded the intended data frame can be difficult. SafeMerge aims to make this process so easy that it replaces the base merge function and becomes part of your routine interactive programming workflow.  
  
The backbone of the package is the SafeMerge function. It is a wrapper for the merge function that returns the merged dataset, but additionally generates errors, warnings, and printed information regarding  the merge, thus enabling the analyst to determine whether it too place as intended.   
  
The SafeMerge package comes with an extended vignette that illustrates common merging problems, and shows how SafeMerge can help you prevent them.   

## Installation

Get the development version from github:

```R
# install.packages("devtools")
devtools::install_github("ohumblet/SafeMerge")
```
