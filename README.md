# README

## Abstract

**"Wir sind keine Zweck-WG!" - Analyse von WG-Anzeigen mit Topic Modeling**

This project examines the thematic structure of German shared apartment (WG) advertisements on WG-gesucht, with a particular focus on identifying recurring topics and potential differences between FLINTA* and Non-FLINTA* WGs. Using topic modeling, four dominant themes were identified: Everyday Life, Leisure & WG Living, Application Process, WG Facilities, and Additional Costs. These themes largely align with the platform’s suggested content structure, highlighting the emphasis on describing daily life, practical aspects, and applicant expectations. The findings also reveal the strong student-oriented nature of the advertisements, reflected in frequent references to studying and university life.

In comparing FLINTA* and Non-FLINTA* WG advertisements, no significant differences in thematic distribution were found. While FLINTA* WGs allocate slightly more text to WG Facilities and Additional Costs and less to the Application Process, these variations are minor. As a result, the study does not provide a conclusive explanation for why FLINTA* WGs are often perceived as more desirable. This suggests that factors beyond the textual content—such as implicit social dynamics, safety perceptions, or community expectations—may influence these preferences.

This research contributes to the understanding of linguistic conventions in digital housing advertisements and offers a foundation for further qualitative studies on WG culture and selection processes.

---

FLINTA* is an acronym for female, lesbian, inter, non-binary, trans and agender people. The * highlights all gender and non-gender. 

## Usage

### Prerequisites

- R installation (including packages listed in `src/analysis.R` and `src/preprocessing.R`)

### Run

1. Set your working directory in `src/analysis.R` and `src/preprocessing.R`
2. Run `analysis.R`.
3. (Optional) Rerun `src/preprocessing.R`

## Resources

List of German stop words: <https://github.com/solariz/german_stopwords/blob/master/german_stopwords_plain.txt>