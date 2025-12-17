# MKP-KAP Survey Codebook
## Mosquitoes, Knowledge, Attitudes, Practices in Metropolitan France
## Version 1.0 | December 2025

---

# Overview

This codebook documents the simulated MKP-KAP dataset (N = 4,000) designed to assess knowledge, attitudes, and practices regarding mosquitoes and mosquito-borne diseases in metropolitan France.

## Dataset Files
- `mkp_kap_simulated_data.csv`: Comma-separated values file
- `mkp_kap_simulated_data.rds`: R data file (preserves variable labels)

## Total Variables: 135
## Total Observations: 4,000

---

# Section A: Demographics (A1-A7)

## A1_sex
- **Description**: Respondent's sex
- **Type**: Character
- **Values**:
  - "Femme" = Female
  - "Homme" = Male

## A2_age
- **Description**: Age group
- **Type**: Character
- **Values**:
  - "18-29" = 18 to 29 years
  - "30-39" = 30 to 39 years
  - "40-49" = 40 to 49 years
  - "50-59" = 50 to 59 years
  - "60-69" = 60 to 69 years
  - "70-79" = 70 to 79 years

## A3_region
- **Description**: Region of residence in metropolitan France
- **Type**: Character
- **Values**: 13 metropolitan regions
  - "Île-de-France"
  - "Auvergne-Rhône-Alpes"
  - "Occitanie"
  - "Nouvelle-Aquitaine"
  - "Hauts-de-France"
  - "Grand Est"
  - "Provence-Alpes-Côte d'Azur"
  - "Pays de la Loire"
  - "Bretagne"
  - "Normandie"
  - "Bourgogne-Franche-Comté"
  - "Centre-Val de Loire"
  - "Corse"

## A4_education
- **Description**: Highest education level achieved
- **Type**: Character
- **Values** (ordered):
  - "Sans le bac" = No baccalauréat
  - "Bac" = Baccalauréat only
  - "Bac+2" = BTS, DUT, DEUG
  - "Bac+3/4" = Licence, Bachelor, Maîtrise
  - "Bac+5+" = Master, Doctorate, Post-doctorate

## A5_postal_code
- **Description**: 5-digit French postal code
- **Type**: Character
- **Format**: "XXXXX" (e.g., "75001", "13001")

## A5_departement
- **Description**: 2-digit département code
- **Type**: Character
- **Values**: 01-95, 2A, 2B

## A6_csp
- **Description**: Socio-professional category
- **Type**: Character
- **Values**:
  - "Agriculteur(trice)"
  - "Artisan(e), commerçant(e), chef d'entreprise"
  - "Cadre ou profession libérale"
  - "Profession intermédiaire"
  - "Employé(e)"
  - "Ouvrier(e)"
  - "Etudiant(e)"
  - "Sans activité professionnelle"
  - "Retraité(e)"

## A7_financial_difficulty
- **Description**: Difficulty paying for basic necessities this year
- **Type**: Integer
- **Values**:
  - 1 = Très difficile (Very difficult)
  - 2 = Difficile (Difficult)
  - 3 = Quelque peu difficile (Somewhat difficult)
  - 4 = Pas difficile du tout (Not difficult at all)

---

# Linked External Data Variables

## geo_zone
- **Description**: Geographic classification for North-South gradient
- **Type**: Character
- **Values**:
  - "South" = Southern France (PACA, Occitanie, Nouvelle-Aquitaine, Corse, Auvergne-Rhône-Alpes)
  - "Central" = Central France (Île-de-France, Grand Est, Centre-Val de Loire, Pays de la Loire, Bourgogne-Franche-Comté)
  - "North" = Northern France (Hauts-de-France, Normandie, Bretagne)
- **Source**: Geographic classification based on latitude and mosquito exposure

## aedes_colonized
- **Description**: Proportion of départements in region colonized by Aedes albopictus
- **Type**: Numeric
- **Range**: 0.40 - 1.00
- **Source**: Santé Publique France surveillance data (2024)

## aedes_present_dept
- **Description**: Aedes albopictus presence in respondent's département
- **Type**: Integer
- **Values**:
  - 0 = Not established
  - 1 = Established
- **Source**: Ministère de la Santé colonization maps (2024)

## summer_temp
- **Description**: Average July maximum temperature in region
- **Type**: Integer
- **Unit**: Degrees Celsius (°C)
- **Range**: 21-30
- **Source**: Météo-France normales climatiques 1991-2020

## climate_suitability
- **Description**: Climate suitability for mosquito proliferation
- **Type**: Integer
- **Values**:
  - 1 = High suitability (Mediterranean climate)
  - 2 = Moderate suitability (Temperate)
  - 3 = Low suitability (Oceanic/cool)

## median_income
- **Description**: Regional median annual income
- **Type**: Integer
- **Unit**: Euros (€)
- **Range**: 21,420 - 25,210
- **Source**: INSEE Filosofi 2021

## poverty_rate
- **Description**: Regional poverty rate
- **Type**: Numeric
- **Unit**: Percentage (%)
- **Range**: 11.0 - 18.1
- **Source**: INSEE Filosofi 2021

## urbanization
- **Description**: Level of urbanization
- **Type**: Character
- **Values**:
  - "Urbain dense" = Dense urban (grands centres)
  - "Urbain intermédiaire" = Intermediate urban
  - "Rural périurbain" = Periurban rural
  - "Rural isolé" = Isolated rural
- **Source**: INSEE Grille Communale de Densité 2020

---

# Section B: Outdoor Exposure (B1-B5)

## B1_private_outdoor, B2_shared_green, B3_public_commercial, B4_natural_areas
- **Description**: Hours per week spent in various outdoor spaces during typical summer week
- **Type**: Integer
- **Values**:
  - 1 = Pas d'accès / pas de temps (No access / no time)
  - 2 = 1 à 3 heures
  - 3 = 4 à 6 heures
  - 4 = 7 à 10 heures
  - 5 = 11 à 15 heures
  - 6 = Plus de 15 heures

## B5_reduced_space_1, B5_reduced_space_2, B5_reduced_space_3, B5_reduced_space_4
- **Description**: Spaces where time was reduced due to mosquitoes (multiple choice)
- **Type**: Integer (binary)
- **Values**:
  - 0 = Not selected
  - 1 = Selected

## B5_no_reduction
- **Description**: Did not reduce time in any space due to mosquitoes
- **Type**: Integer (binary)
- **Values**:
  - 0 = Reduced time somewhere
  - 1 = No reduction anywhere

---

# Section C: Bite Frequency (C1-C6)

## C1_bites_bedroom, C2_bites_indoor
- **Description**: Frequency of mosquito bites in indoor spaces
- **Type**: Integer
- **Values**:
  - 1 = Jamais ou presque jamais (Never or almost never)
  - 2 = Moins d'une fois par mois (Less than once per month)
  - 3 = 1 à 3 fois par mois (1-3 times per month)
  - 4 = 1 à 3 fois par semaine (1-3 times per week)
  - 5 = 4 à 6 fois par semaine (4-6 times per week)
  - 6 = 1 à 3 fois par jour (1-3 times per day)
  - 7 = Plus de 3 fois par jour (More than 3 times per day)
- **Skip Logic**: Always asked

## C3_bites_private_outdoor
- **Description**: Frequency of mosquito bites in private outdoor spaces
- **Type**: Integer
- **Skip Logic**: Shown only if B1 ≠ 1 (has access to private outdoor spaces)
- **Missing**: NA if B1 == 1

## C4_bites_shared_green
- **Description**: Frequency of mosquito bites in shared green spaces
- **Type**: Integer
- **Skip Logic**: Shown only if B2 ≠ 1
- **Missing**: NA if B2 == 1

## C5_bites_public
- **Description**: Frequency of mosquito bites in public/commercial outdoor spaces
- **Type**: Integer
- **Skip Logic**: Shown only if B3 ≠ 1
- **Missing**: NA if B3 == 1

## C6_bites_natural
- **Description**: Frequency of mosquito bites in natural areas
- **Type**: Integer
- **Skip Logic**: Shown only if B4 ≠ 1
- **Missing**: NA if B4 == 1

---

# Section D: Risk Perception and Protection (D1-D6)

## D1_many_mosquitoes, D2_bite_probability, D3_worried_disease, D4_infection_risk
- **Description**: Risk perception items (7-point Likert scale)
- **Type**: Integer
- **Values**:
  - 1 = Pas du tout d'accord (Strongly disagree)
  - 2 = Plutôt pas d'accord (Disagree)
  - 3 = Légèrement pas d'accord (Slightly disagree)
  - 4 = Indécis (Neutral/undecided)
  - 5 = Légèrement d'accord (Slightly agree)
  - 6 = Plutôt d'accord (Agree)
  - 7 = Tout à fait d'accord (Strongly agree)

## D5_protection_1 through D5_protection_12
- **Description**: Protection measures used (multiple choice)
- **Type**: Integer (binary)
- **Protection Measures**:
  - D5_protection_1: Long sleeves/pants
  - D5_protection_2: Repellents (creams, lotions, sprays)
  - D5_protection_3: Electric mosquito trap
  - D5_protection_4: Fan or air conditioning
  - D5_protection_5: Plug-in diffuser
  - D5_protection_6: Candles or coils
  - D5_protection_7: Window/door screens
  - D5_protection_8: Sleep with windows closed
  - D5_protection_9: Bed net
  - D5_protection_10: Natural products (garlic, herbs, essential oils)
  - D5_protection_11: No protection used (EXCLUSIVE)
  - D5_protection_12: Other
- **Values**:
  - 0 = Not used
  - 1 = Used

## D5_any_protection
- **Description**: Derived variable indicating any protection used
- **Type**: Integer (binary)
- **Derivation**: 1 if any of D5_protection_1 through D5_protection_10 or D5_protection_12 = 1

## D6_info_1 through D6_info_15
- **Description**: Information sources about mosquito-borne viruses (multiple choice)
- **Type**: Integer (binary)
- **Information Sources**:
  - D6_info_1: Health professionals
  - D6_info_2: Local authorities
  - D6_info_3: Regional authorities (ARS, prefectures)
  - D6_info_4: National authorities
  - D6_info_5: Social media
  - D6_info_6: Family and friends
  - D6_info_7: Educational institutions
  - D6_info_8: Institutional websites (WHO, ECDC, WOAH)
  - D6_info_9: TV and news channels
  - D6_info_10: Print press
  - D6_info_11: Radio
  - D6_info_12: Product advertisements
  - D6_info_13: Don't remember when/where
  - D6_info_14: None of these (EXCLUSIVE)
  - D6_info_15: Other

---

# Section E: Protection Frequency (E1-E10)

**SKIP LOGIC**: Section E is only shown if D5_protection_11 ≠ 1 (i.e., uses at least one protection measure)

## E1_freq_clothing
- **Description**: Frequency of wearing long clothing for protection
- **Type**: Integer
- **Skip Logic**: Shown only if D5_protection_1 == 1
- **Values**: 1-5 scale
  - 1 = Moins d'une fois par mois
  - 2 = 1 à 3 fois par mois
  - 3 = 1 à 3 fois par semaine
  - 4 = 4 à 6 fois par semaine
  - 5 = Tous les jours

## E2_freq_repellent, E3_freq_trap, E4_freq_fan_ac, E5_freq_diffuser, E6_freq_candles, E9_freq_natural, E10_freq_other
- **Description**: Frequency of using various protection measures
- **Type**: Integer
- **Skip Logic**: Each shown only if corresponding D5_protection_X == 1
- **Values**: 1-6 scale
  - 1 = Moins d'une fois par mois
  - 2 = 1 à 3 fois par mois
  - 3 = 1 à 3 fois par semaine
  - 4 = 4 à 6 fois par semaine
  - 5 = 1 à 3 fois par jour
  - 6 = Plus de 3 fois par jour

## E7_freq_windows, E8_freq_bednet
- **Description**: Frequency of sleeping with windows closed / using bed net
- **Type**: Integer
- **Skip Logic**: E7 shown if D5_protection_8 == 1; E8 shown if D5_protection_9 == 1
- **Values**: 1-5 scale
  - 1 = Moins d'une fois par mois
  - 2 = 1 à 3 fois par mois
  - 3 = 1 à 3 fois par semaine
  - 4 = 4 à 6 fois par semaine
  - 5 = Tous les jours ou presque tous les jours

---

# Section F: Severity and Self-efficacy (F1-F7)

## F1_severity_hospital, F2_severity_death, F3_severity_function
- **Description**: Perceived severity of mosquito-borne diseases
- **Type**: Integer
- **Scale**: 7-point Likert (1 = Strongly disagree to 7 = Strongly agree)

## F4_efficacy_knowledge, F5_efficacy_info, F6_efficacy_identify, F7_efficacy_eliminate
- **Description**: Self-efficacy for mosquito protection
- **Type**: Integer
- **Scale**: 7-point Likert (1 = Strongly disagree to 7 = Strongly agree)

---

# Section G: Scenario Risk Assessment (G1-G6)

## G1_scenario through G6_scenario
- **Description**: Perceived probability of being bitten in various illustrated scenarios
- **Type**: Integer
- **Scale**: 7-point Likert
  - 1 = Extrêmement improbable (Extremely unlikely)
  - 2 = Très improbable (Very unlikely)
  - 3 = Peu probable (Unlikely)
  - 4 = Neutre (Neutral)
  - 5 = Probable (Likely)
  - 6 = Très probable (Very likely)
  - 7 = Extrêmement probable (Extremely likely)

---

# Section H: Cues and Beliefs (H1-H8)

## H1_cue_outdoor, H2_cue_indoor, H3_cue_alerts
- **Description**: Cues to action for protection
- **Type**: Integer
- **Scale**: 7-point Likert

## H4_belief_repellent_effective, H5_belief_repellent_safe, H6_belief_protection_works
- **Description**: Beliefs about protection measure effectiveness and safety
- **Type**: Integer
- **Scale**: 7-point Likert

## H7_barrier_annoyance, H8_barrier_time
- **Description**: Perceived barriers to protection
- **Type**: Integer
- **Scale**: 7-point Likert

---

# Section I: Repellent Beliefs (I1-I10)

**SKIP LOGIC**: Section I is only shown if D5_protection_2 == 1 (uses repellents)

## I1_concern_health through I10_knowledge
- **Description**: Beliefs and attitudes specifically about repellent use
- **Type**: Integer
- **Scale**: 7-point Likert
- **Missing**: NA if D5_protection_2 == 0

---

# Section J: Knowledge (J1-J12)

## J1_west_nile, J1_chikungunya, J1_zika, J1_dengue (Correct answers)
- **Description**: Correctly identified mosquito-borne diseases in metropolitan France
- **Type**: Integer (binary)
- **Values**: 0 = Not selected, 1 = Selected

## J1_malaria, J1_flu, J1_hiv, J1_measles, J1_lyme (Incorrect answers)
- **Description**: Incorrectly selected diseases
- **Type**: Integer (binary)

## J1_none, J1_dont_know (Exclusive options)
- **Description**: None selected / Don't know
- **Type**: Integer (binary)

## J2_arbovirus_experience
- **Description**: Personal or acquaintance experience with dengue/chikungunya/Zika
- **Type**: Integer
- **Values**:
  - 1 = Personally contracted
  - 2 = Someone I know contracted
  - 3 = Neither myself nor anyone I know
  - 4 = Don't know

## J3_outbreak_cause
- **Description**: Understanding of how autochthonous outbreak begins
- **Type**: Integer
- **Values**:
  - 1 = Infected traveler bitten by local mosquito (CORRECT)
  - 2 = Migration of infected mosquitoes
  - 3 = Direct human-to-human transmission
  - 4 = Virus present in environment
  - 5 = Only travelers can contract disease
  - 6 = Not sure / Prefer not to answer

## J4_photo through J8_photo
- **Description**: Tiger mosquito identification from photos
- **Type**: Integer
- **Values**:
  - 1 = Oui (Yes)
  - 2 = Non (No)
  - 3 = Je ne suis pas sûr(e) (Not sure)
  - 4 = Je ne sais pas (Don't know)
- **Correct Answers**:
  - J4, J6: Correct answer is 1 (Yes)
  - J5, J7, J8: Correct answer is 2 (No)

## J9_endemic_residence
- **Description**: Lived in endemic territory
- **Type**: Integer
- **Values**:
  - 1 = Non, jamais (No, never)
  - 2 = Oui, moins de 6 mois
  - 3 = Oui, entre 6 mois et 1 an
  - 4 = Oui, plus d'1 an
  - 5 = Je ne préfère pas répondre / Je ne sais pas

## J10_priority_1 through J10_priority_5, J10_dont_know
- **Description**: Priority measures if acquaintance infected
- **Type**: Integer (binary)
- **Correct Answer**: J10_priority_2 (protect infected person from mosquito bites)

## J11_case_awareness
- **Description**: Awareness of reported cases in department
- **Type**: Integer
- **Values**: 1-6 (various awareness levels)

## J12_fumigation_awareness
- **Description**: Awareness of fumigation operations in neighborhood
- **Type**: Integer
- **Values**:
  - 1 = Oui, récemment (≤ 3 mois)
  - 2 = Oui, mais pas récemment (> 3 mois)
  - 3 = Non, jamais observé
  - 4 = Je ne sais pas / Je ne me souviens pas

---

# Composite Scores (Derived Variables)

## knowledge_diseases
- **Description**: Count of correctly identified diseases
- **Derivation**: J1_west_nile + J1_chikungunya + J1_zika + J1_dengue
- **Range**: 0-4

## knowledge_diseases_errors
- **Description**: Count of incorrectly selected diseases
- **Derivation**: J1_malaria + J1_flu + J1_hiv + J1_measles + J1_lyme
- **Range**: 0-5

## knowledge_photos
- **Description**: Count of correctly identified photos
- **Derivation**: Sum of correct identifications for J4-J8
- **Range**: 0-5

## knowledge_total
- **Description**: Overall knowledge score
- **Derivation**: knowledge_diseases + knowledge_photos + (J3_outbreak_cause == 1)
- **Range**: 0-10

## risk_perception_score
- **Description**: Mean risk perception across D1-D4
- **Derivation**: (D1 + D2 + D3 + D4) / 4
- **Range**: 1-7

## severity_score
- **Description**: Mean perceived severity across F1-F3
- **Derivation**: (F1 + F2 + F3) / 3
- **Range**: 1-7

## efficacy_score
- **Description**: Mean self-efficacy across F4-F7
- **Derivation**: (F4 + F5 + F6 + F7) / 4
- **Range**: 1-7

## protection_count
- **Description**: Number of protection measures used
- **Derivation**: Sum of D5_protection_1 through D5_protection_10 + D5_protection_12
- **Range**: 0-11

## bite_frequency_mean
- **Description**: Mean bite frequency across all locations
- **Derivation**: Row mean of C1-C6 (excluding NA)
- **Range**: 1-7

## scenario_risk_mean
- **Description**: Mean scenario risk assessment
- **Derivation**: (G1 + G2 + G3 + G4 + G5 + G6) / 6
- **Range**: 1-7

## barrier_score
- **Description**: Mean perceived barriers
- **Derivation**: (H7 + H8) / 2
- **Range**: 1-7

---

# Simulation Parameters

## Geographic Gradient
- **Strength**: Strong (OR 2.5-3.5 for South vs. North)
- **Multipliers Applied**:
  - South: bite_multiplier = 3.0, risk_multiplier = 2.8, protection_multiplier = 2.5
  - Central: bite_multiplier = 1.5, risk_multiplier = 1.4, protection_multiplier = 1.3
  - North: bite_multiplier = 1.0 (reference)

## Population Weights
- **Regional**: Proportional to INSEE 2025 population estimates
- **Age**: Based on INSEE adult population distribution (18-79)
- **Sex**: 51.6% Female, 48.4% Male
- **Education**: Weighted by age group (younger cohorts more educated)

## External Data Sources
1. **Climate**: Météo-France normales climatiques 1991-2020
2. **Socioeconomic**: INSEE Filosofi 2021
3. **Urbanization**: INSEE Grille Communale de Densité 2020
4. **Aedes Surveillance**: Santé Publique France / Ministère de la Santé 2024

---

# Missing Data

By design, this simulated dataset contains **no missing data** except for:

1. **Skip logic NA values**: Variables skipped due to branching logic (e.g., C3-C6 if corresponding B item = 1, Section E if no protection used, Section I if repellents not used)

These NA values are **structurally missing** (Missing By Design) and should be handled accordingly in analysis.

---

*Codebook Version 1.0 | Generated December 2025*
