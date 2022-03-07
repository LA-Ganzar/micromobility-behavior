# Micromobility Behavior
## Overview
This project contains the analysis and visualization code for the article [Who travels where: Behavior of pedestrians and micromobility users on transportation infrastructure](https://www.sciencedirect.com/science/article/pii/S0966692321003227).

In this study, we observed sites with a two-way cycle track, sidewalk, and street to determine:
1. The percentage of individuals traveling on each type of transportation infrastructure by mode; 
2. the likelihood of travelers crossing from one type of infrastructure to another by mode; and 
3. the likelihood of individuals traveling on “not recommended” transportation infrastructure by mode. 

## Contents
This project provides code for the following: 
- To assess interrater reliability of the Mobility Behavior Tool when a pair of staff members conducted observations using Cohen's Kappa statistic, which measures the ratio between the proportion of agreements between two raters and the maximum possible proportion of agreements between them. 
- To calculate the percentage of individuals on each type of transportation infrastructure by mode, and use chi-square tests of independence to examine differences in characteristics by traveler type. 
- To perform unadjusted binomial logistic regression modeling with dummy variables for type of traveler to determine the likelihood of individuals crossing from one type of infrastructure to another (1 = crossed; 0 = not crossed) by mode and the likelihood of these individuals traveling on not recommended infrastructure (1 = not recommended; 0 = recommended) during any moment while on the segment. 
- To perform binomial logisitic regression models adjusted for study site, estimated sex of traveler, age of traveler, and group travel.
- To develop a forest plot to visualize model output.
 
Questions about this code can be directed to [Dr. Leigh Ann Ganzar](mailto:leigh.a.ganzar@uth.tmc.edu)
Thanks to Dr. Kevin Lanza and Katie Burford for collaboration on this project. 
