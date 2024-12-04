#this can be edited as much as we need, i just added a few preliminary points

Shiny app for DIG trial data exploration

This repository contains a Shiny app designed for exploring and visualizing data from the Digitalis Investigation Group (DIG) Trial.

#Project Description
The app allows users to interact with and analyze the trial data (`DIG.csv`). Key features include:

- Interactive data visualization
- Statistical summaries
- etc...

#Project Structure
- `data/`: contains our dataset (`DIG.csv`)
- `app.R`: the main shiny application code file
- `other/`: codebook and any other misc files
- 'README.md': description of the repository

#Data
The app uses the `DIG.csv` file, which contains the trial data. A codebook for understanding the variables can be found in the `DIG_code_book.pdf`.

#Requirements
To run this app, you will need the following R packages:

- shiny
- ggplot2
- dplyr
- etc...

These can be installed using:
install.packages(c("shiny", "ggplot2", "dplyr", etc...))

## Testing uploading KY