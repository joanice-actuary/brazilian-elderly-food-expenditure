# brazilian-elderly-food-expenditure
An R-based analysis of Brazilian household food expenditure, examining the impact of elderly residents using updated IBGE POF microdata.
markdown# Brazilian Elderly Food Expenditure Analysis

An R-based statistical analysis investigating the average monthly food expenditure of Brazilian households, focusing on how consumption patterns change with the presence of elderly members (aged 60+).

This study uses microdata from the **Consumer Expenditure Survey (POF 2017-2018)** conducted by the **Brazilian Institute of Geography and Statistics (IBGE)**. To mitigate currency lag, all monetary values have been updated to **September 2026** using the **National Consumer Price Index (INPC/IBGE)**.

## 📊 Project Structure & Methodology

The data pipeline processes direct household survey samples using cluster probability sampling. The data is integrated using a composite primary key (`COD_UPA`, `NUM_DOM`, and `NUM_UC`) across three core relational microdata tables:

1. **`Morador`**: Sociodemographic characteristics of household members (178,431 rows, 56 columns).
2. **`Caderneta Coletiva`**: Daily in-home food and cleaning expenditures (789,995 rows, 23 columns).
3. **`Despesa Individual`**: Out-of-home personal expenditures including food, transit, and health (1,836,032 rows, 25 columns).

## 🛠️ Data Dictionary (Core Variables)

| Variable Code | Parameter Description |
| :--- | :--- |
| **`COD_UPA`** | Primary Sampling Unit code identifying the geographic cluster block. |
| **`COD_INFORMANTE`**| Unique identifier for each individual within the consumption unit. |
| **`ESTRATO_POF`** | Stratification variable dividing the population into homogeneous geographic/socioeconomic subgroups. |
| **`FATOR_ANUALIZACAO`**| Multiplier operator to standardize household expenditures into annual estimates. |
| **`NUM_DOM`** | Household number within a specific UPA to group individuals under the same roof. |
| **`NUM_UC`** | Consumption Unit number grouping individuals sharing the same budget/income. |
| **`PESO_FINAL`** | Sample expansion weight providing statistical representation for the entire country. |
| **`V8000_DEFLA`** | Deflated monetary value corrected for historical price changes (base: Jan 15, 2018). |
| **`V0403`** | Resident age in completed years on the interview date. |
| **`V9001`** | Numerical item code for the specific food item purchased (standardized to 5 digits). |
| **`V9011`** | Frequency or number of months the expenditure occurred within the recall period. |

## 🚀 Built With

* **R (v4.6.x)** - Main processing engine for data cleaning, transformation, and graphics.
* **`survey`** - Official R library required to declare complex sample designs (strata, UPAs, and weights).
* **`skimr`** - Used for comprehensive exploratory data diagnosis and missing value checks.
* **Generative AI** - Used for script optimization, refactoring, and editorial grammar review.

## 📉 Explored Insights

* **Validation:** The processed pipeline calculated a baseline average food expenditure of **R\$ 658.79** for January 2018, matching official IBGE records (**R\$ 658.23**).
* **Inflation Adjustment:** Adjusted to **September 2026** by the INPC index, the equivalent monthly average household food expenditure is **R\$ 1,020.26** (67.2% in-home consumption, 32.8% out-of-home consumption).
* **Geographic Variations:** Spatial analysis reveals Amapá as the state with the highest food expenditure due to heavy logistical import dependency, while Tocantins registers the lowest due to active rural self-consumption.
* **Non-Linear Elderly Trends:** Household food expenditure slightly decreases until age 79 but spikes upward between **80-89 years old** due to clinical supplementation and specialized healthcare diets.

## 📄 License & Attribution

* Data Source: **IBGE POF 2017-2018 Microdata**
* Project developed as part of course material for *STC878 – Introdução à Ciência de Dados* at **UFSM** (2026).
