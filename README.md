# mvpfDE
This is a implementation of the "Marginal Value of Public Funds" Methodology for Germany. 
See https://github.com/OpportunityInsights/welfare_analysis or their paper Hendren & Sprung-Keyser (2020): A Unified Welfare Analysis of Government Policies - Quarterly Journal of Economics 2020
## How to generate results & modify the assumptions:
Download the repository, open the `main.R` file and update the the working directory to point to the project folder. That's the folder where the `main.R` file is located.
Assumptions and some general settings can be set in `assumptions.R`. Running the R code inside `main.R` runs all the programs, performs the bootstrap and exports plots to the `plots` folder. 
## How to add an additional reform:
Each reform or government policy needs to have an identifier, i.e. an unique name.
To add a new reform, two files and a folder have to created:
1. Create a new folder in the `programs` directory named after the identifier
2. Copy the `exampleProgram.R` file from the `sample_files` folder to the created directory and rename it to `WhateverTheIdentifierIs.R`
This file contains all the logic necesarry to calculate the MVPF. 
All of these `.R` files follow the same pattern. Read the explanation in `exampleProgram.R` for further details.
3. Add a `.xlsx` excel file named `WhateverTheIdentifierIs.xlsx` to the estimates folder. A sample file which details the contents of this file is located in the sample_files folder.

The folder structure should then look like this:
```
Project Folder
│   README.md
│   main.R
|   programs.xlsx
│   ...
│
└───programs
│       └───someProgramIdentifier
|       |   │   someProgramIdentifier.R
|       |
│       └───AnotherProgramIdentifier
│       |   │   AnotherProgramIdentifier.R
|       |
|       └───WhateverTheIdentifierIs
│       |   │   WhateverTheIdentifierIs.R
|       |
|       ...
│   
└───estimates
    │   someProgramIdentifier.xlsx
    │   AnotherProgramIdentifier.xlsx
    |   WhateverTheIdentifierIs.xlsx
    |   ...
```

Finally, a new row to `programs.xlsx` may be added to add additional information about the reform such as the year, the category, the age of the benefciaries etc.
This last step is optional, but the reform will be missing in some of the graphs if the necessary information is not provided in `programs.xlsx`. In general, if a program does
not match the structure from above, a hopefully informative error message should be displayed.
