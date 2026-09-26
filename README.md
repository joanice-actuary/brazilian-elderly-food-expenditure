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
