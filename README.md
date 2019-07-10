# EEML2019 Data Analysis Challenge for Artificial Intelligence for Social Good 

The **East European Machine Learning (EEML) Summer School 2019** was held in Bucharest, Romania. As a part of the summer school, there was a **Kaggle Competition on Artificial Intelligence for Social Good (AI4SG)** for EEML 2019 participants. The goal was to predict and analyse electricity consumption.

## Motivation
AI4SG attempts to highlight social problems that can be addressed using Artificial Intelligence, particularly modern Machine Learning (ML) techniques. The goal of EEML summer school is not only to spread the knowledge about modern ML, but also to raise awareness among ML practitioners and the general public about the potential of looking at AI4SG problems such as sustainable agriculture, healthcare, energy etc.

This competition is open to EEML2019 participants and proposes a task around data analysis and prediction of energy consumption. Making accurate predictions about energy consumption not only has significant economical implications, but also leads to better informed decisions regarding energy production and storage, with a huge positive impact on the environment.

## Task: Data Analysis and Visualisation
AI4SG tasks address real problems using real data, which can be noisy, incomplete (due to expensive data collection or privacy concerns), and generally very complex. Training ML models to get accurate predictions in these conditions is far from trivial, and requires generally a tedious inspection of the data and a deep understanding of the problem that is being addressed.

In this context, besides the standard prediction task, there is huge value in finding original ways to interpret and visualise the data, establish correlations with external factors, understand biases in the data.

It was encouraged to use creativity and data analysis skills to create kernels that analyse and visualise the data in original ways, revealing new insights about the data. 

## Data
The data consists of the energy production of different types (wind, gas, coal etc.) and energy consumption from a national grid, over 8 years. The data is a long time series with periodic measurements of the instantaneous production power broken down on all sources: coal, gas, nuclear, wind, solar etc. Consumption represents the target value. All columns except the date are measured in MegaWatts.

The training set consists of continuous measurements starting from January 2010 until January 2018. The test data spawns across the subsequent year (Jan 2018 - Jan 2019).

*Note: The data describes the energy production and consumption of Romania.*

## Kernel
The kernel is for the Data Analysis of Electricity Production and Consumption data of Romania over 9 years. It has been written in **R**. 



