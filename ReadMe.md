---
title: "ReadMe"
author: "David Hood"
date: "2023-02-16"
output: 
  html_document: 
    keep_md: yes
---



(technical note: the ReadMe.Rmd version of this file contains code for generating graphs etc not found in the general reading .md version of the file)

## Background and key data files

At the end of November 2022, the New Zealand government released the October Public Health Risk Assessment of Covid-19 report as a result of Official Information Act request.

<https://fyi.org.nz/request/20877-covid-19-monthly-public-health-risk-assessments>

In that OIA response was a graph of COVID-19 case rates by ANZSCO L3 occupational group (crown copyright)

![Graph of Covid cases by ANZSCO occupational category](static_images/OIAanzsco.png)

As 98.6% of reported infections to mid-August 2022 were since mid-February, so I find it more useful to think of the graph as "Percent of occupations reporting covid infections in the first 6 months of Omicron"- As a rate per time period.

I digitised this file so that the numbers could be used. As this involved marking and measuring the bars on the graph I think most entries are more accurate than the rounded numbers, but some may be out from the full value by more than the rounded amount. If you came here for that file it is [bars.csv](bars.csv "bars.csv")

I also recalled that in April 2020 Figure.nz did an analysis of potential job effects of covid by linking O\*Net US pre-covid occupational surveys about the context of work to ANZSCO NZ occupational classifications at <https://www.notion.so/COVID-19-Job-Impacts-930f46b6cb934dd282f99007e310c010>

I thought it would be useful to redo the data linkage from the O\*net (CC-by 4.0) work context through the Figure.nz concordence (CC-by 4.0) to the occupational group infection rates. And as I think I am the first person to do this, if you want some comments around occupaitonal covid rates and job contexts, this is the right place.

If you just want this file, it is CC-BY-4.0 acknowledging O\*net, FigureNZ, StatsNZ, The New Zealand Government, and David Hood as [combined_data.csv](combined_data.csv "combined_data.csv")

Note that is file has the overview of the O\*net category result (generally as a column with a 5 point scale) followed by the subcategories of each answer choice for that category (as a percentage scale of the frequency of the answer being chosen). The subcategory columns are of the form Main category \_ answer option, while the Main category columns have no underscore in the column names.



## Talk slides

After linking all the data, one of the strongest relationship was the general category of "Close Contact". The more important close contact is as part of the job, the more of the workforce caugt Omicron in the first 6 months.

![](ReadMe_files/figure-html/closeContact-1.png)<!-- -->

I am deliberating labeling the amount of covid "none", "some", and "twice some" since there is under-reporting of cases, and it is the same angle of rise of covid if you multiply out the values to high estimates so the exact number is not too important. But for those who really want a number "some" is 20% and "twice some" is 40% (which should be read as "at least 20%" and "at least 40%"). And I think that variation makes an important point, we can get a doubling (or, more importantly by reading the graph in hte reverse direction, a halving) in the rate of cases not from fate or the genetics of the virus, but from the ways work is structured in different occupations.

Because it is more about the slope of the lines- how the number of infections change as the circumstances of work differ - we can contrast how different measures from the O\*net data affect reported cases.

![](ReadMe_files/figure-html/differCircumstance-1.png)<!-- -->

The "wearing common safety gear every day" is a slightly complex category as the pre-covid question includes a lot of safety gear such as gloves, eye protection, and ear protection, that does not protect against air spread respiratory infection. It also suggests an occupation with fewer causual bystanders in the immediate vicinity, and occupations used to safety requirements.

As well as direct comparisons of job aspects, we can also ask questions like "given the relationship between Constant contact with others and Covid, what are important variables in explaining the remainder of the variation in the data?", In which case important variables become things like:


| slope|activity                                                                             |
|-----:|:------------------------------------------------------------------------------------|
| -0.39|Face-to-Face Discussions_Never                                                       |
|  0.35|Importance of Being Exact or Accurate_Not important at all                           |
| -0.27|Exposed to Whole Body Vibration_Once a week or more but not every day                |
| -0.22|Contact With Others_No contact with others                                           |
|  0.19|Responsibility for Outcomes and Results_Moderate responsibility                      |
| -0.18|Exposed to Minor Burns, Cuts, Bites, or Stings_Once a week or more but not every day |
| -0.18|In an Open Vehicle or Equipment_Once a week or more but not every day                |
| -0.17|Coordinate or Lead Others_Not important at all                                       |
| -0.17|Physical Proximity_I don't work near other people (beyond 100 ft.)                   |
| -0.17|Exposed to Whole Body Vibration_Once a month or more but not every week              |

While many of the variables relate to how many other people are around (including how hostile the environment is to casual by-standers) an interesting one (the 2nd) is importance of being accurate- the lack of meticulousness matching higher levels of covid. Together with a variable not appearing in the top ten, occupational exposure to infectious disease, where the more exposure to occasional but not frequent disease the higher the cases of covid, I think it tells a story where a culture of occupational risk and non-optional health and safety policies have a noticeable effect.


| slope|activity                                                                 |
|-----:|:------------------------------------------------------------------------|
|  0.16|Exposed to Disease or Infections_Once a month or more but not every week |
|  0.16|Exposed to Disease or Infections_Once a week or more but not every day   |
|  0.05|Exposed to Disease or Infections_Once a year or more but not every month |
|  0.02|Exposed to Disease or Infections_Every day                               |
| -0.03|Exposed to Disease or Infections_Never                                   |

You can also take similar appearing jobs and use the contrasts between them to think about how, for those particular occupations, what might be interventions that change infection rates.

-   School Teachears 41%
-   Tertiary teachers 26%

Differences in contact time and the student body (and in 2022, the expectations on the student body to keep up from video recordings of lectures if at all sick)

-   Insurance Agents and Sales Representatives 32%
-   Real Estate Sales Agents 24%

Getting into the fresh air more, and doing so by private vehicle.

-   Hospitality Workers 38%
-   Sales Assistants and Salespersons 31%

Crowd density

-   Retail Managers 28%

-   Sales Assistants and Salespersons 31%

-   Accommodation and Hospitality Managers 27%

-   Hospitality Workers 38%

Private offices and vehicles

In this talk I also want to touch on what to expect from outbreaks at the moment.

From news reports we know that in November the Majestic Princess cruise ship had cases double with an average period of 18 hours over a 12 day cruise of Australia to New Zealand and back. This constrained enviroment can be the fast end of environmental spread.

We also have the Ministry of Health held data covering new cases in Tairawhiti over the past New Year's period (the data used- current daily cases and HSU region population estimates in use at the time of the outbreak (available from https://github.com/minhealthnz/nz-covid-data/raw/main/cases/covid-case-counts.csv and https://raw.githubusercontent.com/minhealthnz/nz-covid-data/main/vaccine-data/2023-01-11/hsu_population.csv )) is NZ Ministry of Health Crown Copyright CC-by 4.0)

![](ReadMe_files/figure-html/newYear-1.png)<!-- -->

From a heavily socialising outside 3 day event for 20,000 18 to 25 year olds, at event infections were on average 4 per seed case, which then spread even faster home and workplaces. But after the first generation of post event infections, cases declined, with an overall outbreak total of around 11 times as many as initial infections. And that deline, that threshold between growth and decline in cases given the current context, looks like an acheivable goal.

So given those two examples we are, broadly, in a period where an outbreak can infect a lot of people at once, and can mean mass sickness at a location such as a workplace, but regular behaviour prevents spreading between locations enough to prevent outbreak growth.

## Preparation code

For those who wish to link the data sources themselves, or investigate the details of the linking, I am providing the relevant data files and code here.

When I came to do my linking, I found that many of the O\*net codes had changed from when Figure.net did their linking in 2020. So I created a April 2020 to February 2023 (current) lookup table. This is available as [onet_concordance_2020_2023.csv](onet_concordance_2020_2023.csv "onet_concordance_2020_2023.csv") and should be treated as being made available with a Creative Commons CC-By 4.0 licence (the same as the source materials it is made from).

As mentioned earlier, [bars.csv](bars.csv "bars.csv") is the government occupational infection data, so is publically usable under crown copyright. Original source <https://fyi.org.nz/request/20877-covid-19-monthly-public-health-risk-assessments>

The file [ANZSCOMapping.xlsx](ANZSCOMapping.xlsx "ANZSCOMapping.xlsx") is the CC-by 4.0 Figure.nz/StatsNZ match of ANZSCO to O\*net occupations, the file (under the same filename) is on the webpage of <https://www.notion.so/COVID-19-Job-Impacts-930f46b6cb934dd282f99007e310c010>

The file "fignz_Table_Age_final.csv" is too big to put on Github, but is available from the same webpage <https://www.notion.so/COVID-19-Job-Impacts-930f46b6cb934dd282f99007e310c010> as a file called Table_Age_final.csv with the same copyright. It is used because it has ANZSCO counts of the finer level of groups, which let me weight the means by the number of people in those occupations when calculating the overall broader occupational categories used in the OIA response.

From the O\*net site <https://www.onetcenter.org/database.html#act> I have downloaded and cached the CC-by 4.0 current versions of the files [Work Context Categories.xlsx](Work Context Categories.xlsx "Work Context Categories.xlsx") (used for obtaining the exact question names for O\*net survey respones) and [Work Context.xlsx](Work Context.xlsx "Work Context.xlsx") for the work context survey responses for each occupation. I have cached this data so people can repeat this data matching with a stable set of occupational labels.




```r
library(readxl)
library(dplyr)
library(tidyr)
library(stringr)

# anzsco reference, but not data used:
# https://www.abs.gov.au/AUSSTATS/abs@.nsf/DetailsPage/1220.0First%20Edition,%20Revision%201?OpenDocument

# bars.csv is the percentages of occupation cases merged w. ANZSCO L3 digit #
cases <- read.csv("bars.csv") |>
  mutate(ANZSCO_three = formatC(anzscode, width=3, format="d"))
# the conversion from Figure.nz of ANZSCO minor to O*NET ID is in
# ANZSCOMapping.xlsx
anzsco <- read_excel("ANZSCOMapping.xlsx", sheet=4) |> 
  rename(code20 = 1)
# my own ANZSCO ONET 2020 to ONET 2023 concordance
onet_concordance <- read.csv("onet_concordance_2020_2023.csv")

## O*net Category Names
category <- read_excel("Work Context Categories.xlsx")

# the O*NET context figures for the 57 (each with 6 subcats) 
# categories are in Work Context.xlsx
context <- read_excel("Work Context.xlsx") |> 
  rename(code23 = 1) |>
  left_join(category, by = c("Element ID", "Scale ID", "Category")) |>
  mutate(Element_Context=paste(`Element Name.x`, `Category Description`, sep="_"),
         Element_Context = gsub("_NA$","",Element_Context)) |>
  select(1,20,8)
# NZ occupational demographics to weight the data
demog <- read.csv("fignz_Table_Age_final.csv") |>
  filter(Location == "Total New Zealand", Age.group == "Total") %>%
  mutate(ANZSCO_Code = str_extract(Stats.NZ.Occupation, "^[01234567890][01234567890][01234567890][01234567890][01234567890][01234567890]")) %>%
           select(Stats.NZ.Occupation, ANZSCO_Code, NZ_Population=Total)

# this is effectively the mean of the O*net values of all ANZSCO detailed occupations that match,
# then aggregated into a weighted mean of all the broad ANZSCO categories
combined <- demog |> 
  inner_join(anzsco, by = "ANZSCO_Code") |> 
  inner_join(onet_concordance, by = "code20") |>
  inner_join(context, by = "code23") |>
  group_by(ANZSCO_Title,ANZSCO_Code, Element_Context, NZ_Population) |> 
  summarise(Average_rating = mean(`Data Value`), .groups="drop") |>
  mutate(ANZSCO_three = str_extract(ANZSCO_Code, "^[01234567890][01234567890][01234567890]")) |> 
  group_by(ANZSCO_three, Element_Context) |> 
  summarise(weighted = sum(Average_rating * as.numeric(NZ_Population))/sum(as.numeric(NZ_Population)),
            .groups="drop") |>
  inner_join(cases, by = "ANZSCO_three") |> 
  select(ANZSCO_minor,Percentage, Element_Context, weighted) |> 
  spread(key=Element_Context, value=weighted) |>
  rename(Percentage_Covid = 2)
write.csv(combined, file = "combined_data.csv", row.names = FALSE)
```

### Check linear model summaries for all columns

I thought this might be useful to others as the code for getting simple linear model summary values for each variable. Keep in mind that the aggregate variables are often composed of a series of variables that do not have a directly comparable scale (for example, constant/daily/weekly/monthly/never) so I suggest with this exploration read it as strength of differences across the data rather than focusing too much on exact output values.


```r
colN <- ncol(combined)

checkmodel <- function(x, dset=combined){
  y <- dset[[2]]
  xvar <- dset[[x]]
  modelm <- lm(y ~ xvar)
  c1 <- modelm$coefficients[[1]]
  c2 <- modelm$coefficients[[2]]
  modelsum <- summary(modelm)
  rsq <- modelsum$r.squared
  return(data.frame(x,c1,c2,rsq))
}
modellist <- lapply(3:colN, checkmodel)
modeldf <- bind_rows(modellist)
modeldf$comboname <- colnames(combined)[3:length(colnames(combined))]
```


