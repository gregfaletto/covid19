These repository contains my contributions to Team DSO's entry in the [2020 COVID-19 Computational Challenge]{https://grmds.org/2020challenge#solutions} hosted by the City of Los Angeles and RMDS. My teammate Mohammad Mehrabi and I tied for second place in the competition. The goal of the competition was to develop location-based risk scores around the city of Los Angeles. The exact predicdtion task was not defined besides that; it was up to individual teams to interpret the prompt and come up with an original solution.

With the available data, we decided on our own approach. We had access to daily COVID-19 test results in each neighborhood in Los Angeles, as well as covariates for each neighborhood. We decided on a three-step approach:

1. We used the test results data (along with some assumptions) to estimate the number of [susceptible, infected, and recovered]{https://en.wikipedia.org/wiki/Compartmental_models_in_epidemiology#The_SIR_model} citizens in each neighborhood on each day.
2. Next, we forecasted new infections in each neighborhood a day ahead based on currently available information using a GLM panel data model (Poisson autoregressive count model with time- and neighborhood-based covariates). We used the [R pglm package]{https://cran.r-project.org/web/packages/pglm/index.html} to fit the model via maximum likelihood.
3. Finally, we calculated per capita predicted new infections in each neighborhood. We converted per capita risk infections into risk categories ("high," "medium," and "low" risk) for each neighborhood on each day.

Our code and solution is also available on [the website for the Challenge.]{https://grmds.org/2020challenge#solutions} 

This repository contains the following files:

* The **Data** folder contains the data we used in our model, along with the sources.
* **2020_COVID_19_Computational_Challenge_Technical_Report.pdf:** This is the technical report we submitted. It describes our approach in detail.
* **README.txt:** This is the README file we submitted with our code with details on how to run the code.
* **poisson_panel.R:** This is the code I wrote. It takes in the data, estimates susceptible, infected, and recovered populations in each neighborhood on each day, and fits a Poisson panel data generalized linear model. (It also does a few other tasks, which you can see in the code and are also described in the report. The main thing in the report that my code doesn't do is generate the visualization of risk scores; Mohammad wrote that code separately.)
* **risk_scores_june_8_2020.csv:** Our generated risk scores for June 8 2020 (based on data available June 7 2020).
