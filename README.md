# BRAVE Toolkit

The Benefit Risk Assessment of VaccinEs (BRAVE) Toolkit is a comprehensive set of tools, resources, and methodologies designed to support the benefit risk assessment and contextualization of vaccines. This toolkit includes datasets, a user manual, detailed methodology, a set of functions and code derived from the applied methods, and a graphical user interface (GUI) for interacting with the results through a dashboard.

## Overview

The interactive toolkit was developed using an R Shiny dashboard, allowing users to input data on COVID-19 cases, hospitalizations, ICU admissions, and deaths. These inputs inform assessments of the benefits and risks of vaccination. The analysis considers COVID-19 incidence data, information on the emergence of SARS-CoV-2 variants of concern (VoC), and COVID-19 incidence by population of interest (e.g., country, age group, vaccine brand). Vaccine effectiveness estimates for different vaccine doses are also required as input parameters.

## Usage

To use the BRAVE Toolkit, follow these steps:

1.  Clone this repository.
2.  Run the Shiny app (`app.R`) using RStudio or the R command line.

## Datasets

To effectively utilize the BRAVE Toolkit for benefit risk contextualization of vaccination, certain datasets are required. The toolkit uses a minimum set of input, default, data sets covering the period from 13 December 2020 to 31 December 2021.

### Benefit calculation

1. **EU/EEA Country Demographics:**
   - Population per country and 10-year age group categories from EUROSTAT, as reported in December 2021.

2. **Granular COVID-19 Incidence:**
   - Data on COVID-19 cases obtained from EMA and ECDC datasets, covering 20 EU/EEA countries.
   - Information includes confirmed COVID-19 cases, hospitalizations, ICU admissions, and COVID-19-related deaths by 10-year age categories.

3. **Aggregated COVID-19 Incidence:**
   - Granular COVID-19 incidence data aggregated by country per day for cases, ICU admissions, hospitalizations, and mortality from 26 February 2020 to 9 February 2022.

4. **Variants of Concern (VoCs):**
   - Proportion of each VoC in sequenced SARS-CoV-2 positive samples, by day and by country, retrieved from GISAID EpiCOV genomic surveillance database and TESSy dd. 11 May 2020.

5. **Detailed Vaccination Coverage:**
   - Data sourced from ECDC Vaccine Tracker submissions to TESSy dd. 10 February 2021.
   - Information includes COVID-19 vaccine roll-out within the EU/EEA by day, vaccine brand, country, 10-year age categories, and number of doses received.

### Risk calculations

1. **Observed Events:**
   - Events after vaccination obtained from spontaneous case reports collected in EudraVigilance dd. 25 July 2021, stratified by vaccine, age, and gender.

2. **Background Incidence Rates:**
   - Background incidence rates of adverse events of special interest identified prior to vaccination roll-out.

3. **Aggregated Vaccination Coverage:**
   - Aggregated vaccination coverage data obtained from detailed vaccination coverage, imputed from gender proportions and redistributed over age categories.

## Modifying Input Parameters

Input parameters can be modified in the tool dashboard, providing user flexibility in terms of default settings. These default settings, including the time window of interest and the risk event of interest, can be adjusted by selecting input databases and attributing weights to background incidence rate estimates.

Additionally, published literature on the effectiveness of different authorized COVID-19 vaccines based on real-world evidence (RWE) studies was used to obtain effectiveness estimates. A sensitivity analysis of these input parameters can be performed to quantify uncertainty with regard to the endpoints of interest.

------------------------------------------------------------------------

*Please refer to the [user manual](https://dsi-uhasselt.shinyapps.io/covid_vaccine_risks_and_benefits_develop/_w_88212885/user%20manual.pdf) for detailed instructions on using the BRAVE Toolkit.*
