# RCBC Shiny Election Suite
A Shiny app for creating, administering, and tabulating multi-winner ranked choice elections.
# Installation
To use:
1. Download the repository and extract the files into a local directory.
2. Open `RCBC Shiny Voting Suite.Rproj` in RStudio.
3. Install the required dependencies: `install.packages(c("bslib", "digest", "dplyr", "gtools", "jsonlite", "magrittr", "readxl", "shiny", "shinyjs", "sortable", "stringi", "tidyverse", "uuid", "vote"))`
4. Run `shiny::runApp()` in the RStudio Console.
# Supported Methods
Currently, the suite supports the following election methods:
- [CPO-STV](https://en.wikipedia.org/wiki/CPO-STV)
- [STV](https://en.wikipedia.org/wiki/Single_transferable_vote) (from the [vote](https://cran.r-project.org/web/packages/vote/index.html) package)
- [Borda scores](https://en.wikipedia.org/wiki/Borda_count)
- Borda with tiebreakers
The code for the CPO STV and Borda methods is from [our own implementation](https://github.com/zenonsommers/cpo_stv) of these methods in R. 
# Sample Data
To access the sample elections, copy the contents of the `Sample Data` folder into the `Elections` folder. You may need to create the `Elections` folder manually if you have not yet run the app. Currently, the sample election `rcbc-cycle-1` is set to "not accepting new responses" as an example. The admin password for all sample elections is `password`.