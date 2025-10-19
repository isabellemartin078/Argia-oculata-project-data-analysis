# The impact of canopy structure and territoriality on habitat selection of male Argia oculata (Odonata: Coenagrionidae)

This repository contains R scripts and data used to analyze the **Argia oculata** (damselfly) and canopy cover and height datasets.

## Contents
- 'damselflystatics.R': Statistical analysis script (R Studio)
- 'damselfly_day_1.csv': Data for damselflies caught on Day 1
- 'damselfly_day_2.csv': Data for damselflies caught on Day 2
- 'damselfly_resighting.csv': Data for damselflies that were resighted. Contains average, range and min/max positions
- 'canopy_data.csv': Data for canopy cover and height. Canopy_CoverL and Canopy_CoverR were collected with densitometer, whilst canopy_coverL2 and canopy_coverR2 determined with coverR2 package (based on digital photo analysis)
- 'damselfly_complete.csv': Restructured and cleaned up data; d_distance is point at which damselfly was first caught. -

This project used the canopy_data and damselfly_complete datasheets in analysis coding. 

## Requirements
- R (version 4.4.3 was used)
- Packages: ggplot2, dplyr, readr
