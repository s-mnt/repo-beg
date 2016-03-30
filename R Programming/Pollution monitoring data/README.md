## Introduction

For this assignment, three functions have been written that interact with dataset that accompanies this assignment. The dataset is contained in a zip file 'specdata.zip' that can be downloaded from here:

https://eventing.coursera.org/api/redirectStrict/W8Vl6NOElhUKYD3qKYVgKNNOyHiqeJOJxFuzxq2NjL3dFLa9ZjyqjAi0a0xRTGdZn8E3PjiHqPjuT5SDC2oomw.s7mcRCHFGz5GItPSje-QsA.ik54mi65_UWcfD-1IhdO9XRS_rpj0EJDpa6WgbbZCp_gAv52WQzmdcU1xXacR2DM75rznTgZzxxkGnAhSWQWKeaPjKcBwo2H5hQsYZDvAUFkOJgk7UsTpSokrtWfhJZhC4jnhW4UDqUGIA1RckPzl5QQ_oFDLhgZvRJ1jflW-IECIJHIWlbemOVApZPNLDtCtFvw7bjROJaJXFpIDzzWUXOTFHAS9N7XfW7izFiEDLpoZu7kghVL-XP_cHJAWWQvR9Cdhq1FFsasbaKoqq-Tz7lM8hyo0Iz4TbwUJS6n4AJq08--bx5nOTClD-zJqLVuTETpuoTnt-sP7-BS_VLoIJPA7EjWJEVUOUCoypjJg0eQyCzf9VZ9pKZTBzZEBw71

The zip file contains 332 comma-separated-value (CSV) files containing pollution monitoring data for fine particulate matter (PM) air pollution at 332 locations in the United States. Each file contains data from a single monitor and the ID number for each monitor is contained in the file name. For example, data for monitor 200 is contained in the file "200.csv". Each file contains three variables:
Date: the date of the observation in YYYY-MM-DD format (year-month-day)
sulfate: the level of sulfate PM in the air on that date (measured in micrograms per cubic meter)
nitrate: the level of nitrate PM in the air on that date (measured in micrograms per cubic meter)

### Part 1

Write a function named 'pollutantmean' that calculates the mean of a pollutant (sulfate or nitrate) across a specified list of monitors. The function 'pollutantmean' takes three arguments: 'directory', 'pollutant', and 'id'. Given a vector monitor ID numbers, 'pollutantmean' reads that monitors' particulate matter data from the directory specified in the 'directory' argument and returns the mean of the pollutant across all of the monitors, ignoring any missing values coded as NA.

[ See pollutantmean.R]


### Part 2

Write a function that reads a directory full of files and reports the number of completely observed cases in each data file. The function should return a data frame where the first column is the name of the file and the second column is the number of complete cases.

[ See complete.R]


### Part 3

Write a function that takes a directory of data files and a threshold for complete cases and calculates the correlation between sulfate and nitrate for monitor locations where the number of completely observed cases (on all variables) is greater than the threshold. The function should return a vector of correlations for the monitors that meet the threshold requirement. If no monitors meet the threshold requirement, then the function should return a numeric vector of length 0. 

[ See corr.R]




