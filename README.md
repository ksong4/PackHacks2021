# TITLE IV Program Dashboard : Helping Students Navigate College Finances

> *For PackHacks 2021 by Kuncheng Song, Caizhi Huang and Preethi Thunga*

Link to our GUI: https://skcapp.shinyapps.io/PackHacks2021/ 
Link to our demo: https://www.youtube.com/watch?v=X9PegGe0pDc
## Inspiration

Whether you’re looking to avoid debt, give your parents a break or get better returns on your investment, choosing college based on price is important. Currently, there is no comprehensive tool to help guide students in their decision making process. Our goal is to present all this information to students and families in a user-friendly way and help navigate college finances. 

## What it does

If you’re looking to graduate from college with little to no debt, we are here to help (you don’t have to repay us;) ). Our interactive Shiny dashboard will help you carefully consider and compare financial aid packages offered by different institutions in the last several years. We modeled the rate of change of each loan type granted over the years for every institution to give the students an idea of how their favorite universities have been performing.

## How we built it

R Shiny is a great tool to build interactive web apps with strong data visualization techniques and statistical modeling functions. We built our dashboard on R Shiny using student loan data from the Federal Student Aid (https://studentaid.gov/) and US Department of Education (https://collegecost.ed.gov/affordability) websites. We present this information at multiple levels (university, state and national ) for the last five years. 

We use a linear model to capture the rate of change of each loan type across all years. We reported the OLS estimate of the change rate coefficient, the standard error of the estimate, p-value of the estimate, and the R-square. This data can be used to make predictions about upcoming years. 

## What it looks like

![titleIV](https://github.com/ksong4/PackHacks2021/blob/main/titleIV.jpeg) 

## Challenges we ran into

College financial datasets are large and scattered all over the place. Moreover, these files have been formatted differently over time. Consolidating this information from several sources to obtain meaningful insights and trends in such a limited time was our biggest challenge. 

## Accomplishments that we're proud of

We analysed extensive datasets and built a powerful, easy-to-use interactive tool in less than 24 hours! This GUI allows you to dynamically view and download/save plots as you wish

## What we learned
* Group work and efficient communication 
* How Shiny works between the UI and Server  
* Interactive Plotly Graphs  

## What's next for Student Loan Dashboard

* We will work on using our linear model to make predictions on financial aid packages offered by institutions in the coming years. Given some of the linear models did not fit the data well, more complex models need to be considered.  
* Given we have data from 3000+ universities, it might be worth classifying them based on ownership (Public vs Private) prior to running models as this parameter is a key driver of the number of financial aid packages offered by the universities. 
 
