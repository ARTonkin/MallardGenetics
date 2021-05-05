## Mallard Genetics Consulting Project

See my website www.tonkinprojects.com for a more indepth description and some sample outputs.

I was hired as a statistical consultant for a researcher who was interested in the links between duck genetics and their biometrics (physical traits). This repository includes the partial code from this project. I have permission to use this data and code for personal projects.

### Part 1 July - November 2019

Part 1 required me to combine four different datasets into one. Due to the duck ids not matching up between all datasets, the joins between the data would inevitably lead to dataloss. I investigated the best way to combine them by analysing the amount of data lost.

I first investigated how effective the scientists were at guessing the species of duck based on their biometric data. I created two way tables and alluvial graphs.

Next, I focused on attempting to find a way to split up the ducks. I tried a multivariate approach with PCA, aimed at seeing if there were any patterns within subgroups of variables between species. 

After PCA, I decided to see if a machine learning approach could prove useful. I created decision trees and tried random forests. Using the decision tree results, I created decision surfaces for visualisation purposes.

The last task was a simple data visualisation of duck reproduction measures.

The last section of code is my first foray into dasboarding, using shiny and flexdashboard to create interactive visualisations.

### Part 2 January - August 2020

In part 2, I had a new dataset, a single one that was combined and well cleaned.

Part 2 involved the creation of conditional decision trees. The endeavor was to create a tree which would split the genetic data as best as possible, thereby creating a decision tree in which you could use the biometric measurements of a duck to give a guess as to its genetics.

The trees were graphed with the ggparty library. I created a complex graphical layout which shows the number of ducks at each split, as well as the proportion of each genetic type. 














