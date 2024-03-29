# The Pandemic Backsliding Project (PanDem), Version 6.1

This repository contains the data, comments and sources for the PanDem dataset V6.1. It covers the period from March 2020 to June 2021. Download the repository including the codebook and data by clicking the green clone or download button.



## Background

Emergency provisions - such as the ones that many countries put in place during the Covid-19 crisis - enable states to temporarily limit personal freedoms and checks and balances to react effectively in situations of crisis. Previous research shows that some leaders abuse such tools to foster more permanent autocratization by (a) imposing measures that are disproportionate to the severity of crises and (b) keeping emergency provisions in place once the factual situation improves (see [Lührmann and Rooney, 2021](https://doi.org/10.5129/001041521X16004520146485)). How can states effectively respond to crises without undermining democratic standards, freedom and human rights? 

The Pandemic Backsliding Project (PanDem) tracks state responses to the Covid-19 pandemic as illustrative of the varieties of emergency measures and their execution, addressing how these decisions affect short- and long-term prospects for democracy. To learn more about the project and its main findings, see our peer-reviewed article in [Social Science and Medicine](https://doi.org/10.1016/j.socscimed.2021.114244). 

This repository contains the data and sources for the PanDem project. We provide the data in two formats. The *time-series format (TS)* contains observations from 11 March 2020 to 30 June 2021 arranged within five coding periods that roughly coincide to financial quarters. This data may be useful for analyzing trends over time. It can be accessed [here](https://github.com/vdeminstitute/pandem/blob/master/datasets/pandem_TS_v6.1.xlsx). The *cross-sectional format (CS)* contains a single row for each country. It reports the maximum value on the PanDem index and each of the types of violations. It also reports the average value for the PanBack index for the period from 11 March 2020 to 30 June 2021. The cross-sectional version of the dataset can be accessed [here](https://github.com/vdeminstitute/pandem/blob/master/datasets/pandem_CS_v6.1.xlsx).

The R script that produces the indices and the two datasets from the raw data is available [here](https://github.com/vdeminstitute/pandem/blob/master/code/create_indices.R).

The full **codebook** for this project is available [here](https://github.com/vdeminstitute/pandem/blob/master/codebook/pandem_codebook.pdf).  

Click [here](https://github.com/vdeminstitute/pandem/tree/master/by_country) to access all coding sources and justifications organized **by country**, and [here](https://github.com/vdeminstitute/pandem/tree/master/by_question) to access all sources and coding justifications organized **by question**. 

Check out our **dashboard** [here](https://www.v-dem.net/pandem.html) to explore the most recent PanDem data and indices. Read our **policy briefs** [here](https://www.v-dem.net/pb.html).

## What's new in this version

V5 provided a time series for the three last quarters of 2020 (Q2, Q3, and Q4). V6 adds the first two quarters of 2021. In v6, we build on prior coding and sources of information from v3, v4, and v5 instructing coders to look for any new information that updates our prior knowledge for previous coding periods. If they find such information, values  are updated to reflect the new information and new sources/comments are added. For this reason, values may deviate from v5 coding.  The coding for the fourth quarter of 2020 may also deviate from v5 if new information was found concerning events occurring between December 10th (the last date covered by v5) and December 31st, 2020. V6.1 replaced urls with permanent links and corrected an error in the “discrim” coding for Singapore from v6.
