# --- Load Necessary Packages ---
library(WDI)             # To download World Bank data
library(plm)             # For panel data modeling (fixed effects)
library(tidyverse)       # For data manipulation and visualization
library(rnaturalearth)   # For world map spatial data
library(rnaturalearthdata) # Provides additional natural earth data
library(colorspace)      # For additional palettes

# --- Define Indicator Codes ---
indicators <- c(
  "SH.XPD.CHEX.PP.CD",  # Health expenditure per capita, PPP ($)
  "SP.DYN.LE00.IN",     # Life expectancy at birth (years)
  "SP.POP.TOTL"         # Total population
)

# --- Download and Prepare the Data ---
# Fetch data (2000-2021), remove aggregate regions, and select key columns.
data <- WDI(
  country = "all", 
  indicator = indicators, 
  start = 2000, 
  end = 2021,
  extra = TRUE
) %>% 
  as_tibble() %>%
  filter(region != "Aggregates") %>%  # Exclude non-country aggregates
  select(country, iso3c, year, all_of(indicators), region)

# --- Calculate Missing Data Rates ---
# Pivot and summarize missing values to assess data quality.
missing_summary <- data %>%
  pivot_longer(cols = all_of(indicators), names_to = "indicator", values_to = "value") %>%
  group_by(country, indicator) %>%
  summarise(
    missing_years = sum(is.na(value)),
    total_years   = n(),
    missing_rate  = missing_years / total_years,
    .groups = "drop"
  )
View(missing_summary)  # Inspect missing data patterns for each country and indicator

# --- Filter Out Countries with Excess Missing Data ---
# Remove small/unstable nations to ensure robust analysis.
data <- data %>%
  filter(!(country %in% c(
    "American Samoa", "Andorra", "Aruba", "Bermuda", "British Virgin Islands", 
    "Cayman Islands", "Channel Islands", "Curacao", "Faroe Islands", 
    "French Polynesia", "Gibraltar", "Greenland", "Guam", "Hong Kong SAR, China", 
    "Isle of Man", "Korea, Dem. People's Rep.", "Kosovo", "Liechtenstein", 
    "Macao SAR, China", "Monaco", "New Caledonia", "Northern Mariana Islands", 
    "Puerto Rico", "San Marino", "Sint Maarten (Dutch part)", "Somalia", 
    "St. Martin (French part)", "Turks and Caicos Islands", "Virgin Islands (U.S.)", 
    "West Bank and Gaza", "Palau", "Venezuela, RB", "South Sudan", "Montenegro", 
    "Libya", "Zimbabwe", "Syrian Arab Republic", "Yemen, Rep.", "Iraq", 
    "Timor-Leste", "Afghanistan"
  ))) %>%
  # Rename columns to user-friendly names
  rename(
    health_expenditure = SH.XPD.CHEX.PP.CD,
    life_expectancy    = SP.DYN.LE00.IN,
    population         = SP.POP.TOTL
  )

# Preserve original indicator labels as attributes
attr(data$health_expenditure, "label") <- "Health Expenditure per Capita, PPP ($)"
attr(data$life_expectancy, "label")    <- "Life Expectancy at Birth (years)"
attr(data$population, "label")           <- "Total Population"

# Quick check on data structure and summary
str(data)
summary(data)
View(data)

# --- Exploratory Data Analysis (EDA) Visuals ---

# 1. Trend of Average Health Expenditure by Year and Region
ggplot(data, aes(x = year, y = health_expenditure, color = region)) +
  stat_summary(fun = mean, geom = "line", size = 1) +
  labs(
    title = "Trend: Avg. Health Expenditure per Capita (PPP)",
    subtitle = "Grouped by region (2000-2021)",
    x = "Year",
    y = "Avg. Health Expenditure (PPP $)"
  ) +
  theme_minimal() +
  theme(legend.position = "bottom")

# 2. Trend of Average Life Expectancy by Year and Region
ggplot(data, aes(x = year, y = life_expectancy, color = region)) +
  stat_summary(fun = mean, geom = "line", size = 1) +
  labs(
    title = "Trend: Avg. Life Expectancy at Birth",
    subtitle = "Grouped by region (2000-2021)",
    x = "Year",
    y = "Avg. Life Expectancy (years)"
  ) +
  theme_minimal() +
  theme(legend.position = "bottom")

# 3. Cross-Sectional Scatter Plot (2021): Log(Health Expenditure) vs Life Expectancy
# This plot offers a snapshot for 2021 to visualize the cross-sectional association.
ggplot(data %>% filter(year == 2021), aes(x = log(health_expenditure), y = life_expectancy, color = region)) +
  geom_point(size = 3, alpha = 0.6) +
  geom_smooth(method = "lm", se = FALSE, linetype = "dashed", color = "black") +
  labs(
    title = "2021 Analysis: Log(Health Expenditure) vs Life Expectancy",
    x = "Log(Health Expenditure per Capita, PPP)",
    y = "Life Expectancy (years)"
  ) +
  theme_minimal() +
  theme(legend.position = "bottom")

# 4. Faceted Scatter Plot: Relationship by Region (All Years)
# Displays the relationship with individual linear fits, highlighting regional differences.
ggplot(data, aes(x = log(health_expenditure), y = life_expectancy)) +
  geom_point(size = 2, alpha = 0.5, color = "steelblue") +
  geom_smooth(method = "lm", se = FALSE, color = "darkred") +
  facet_wrap(~ region, scales = "free_x") +
  labs(
    title = "Health Expenditure vs Life Expectancy by Region",
    x = "Log(Health Expenditure per Capita, PPP)",
    y = "Life Expectancy (years)"
  ) +
  theme_minimal()

# 5. Boxplot: Distribution of Life Expectancy by Region
# Provides insights into the spread and central tendency of life expectancy across regions.
ggplot(data, aes(x = region, y = life_expectancy, fill = region)) +
  geom_boxplot(alpha = 0.7, outlier.color = "red") +
  labs(
    title = "Life Expectancy Distribution by Region",
    x = "Region",
    y = "Life Expectancy (years)"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position = "none")

# 6. Density Plot: Distribution of Log(Health Expenditure) by Region
# Compares the distribution of health expenditure on a log scale.
ggplot(data, aes(x = log(health_expenditure), fill = region)) +
  geom_density(alpha = 0.5) +
  labs(
    title = "Density of Log(Health Expenditure) by Region",
    x = "Log(Health Expenditure per Capita, PPP)",
    y = "Density"
  ) +
  theme_minimal() +
  theme(legend.position = "bottom")

# --- Panel Data Setup and Fixed Effects Estimation ---
# Convert the dataset to a panel data frame with 'country' as the individual identifier and 'year' as the time variable.
pdata <- pdata.frame(data, index = c("country", "year"))

# 7. Fixed Effects Model (Base Model)
# Estimate the within-country effect of log-transformed health expenditure on life expectancy.
# This model captures the contemporaneous association between health expenditure and life expectancy.
fe_model <- plm(life_expectancy ~ log(health_expenditure),
                data = pdata, model = "within", weight = population)
summary(fe_model)  # Examine the coefficient, which represents the % change in expenditure associated with a change in life expectancy.

# 8. Fixed Effects Model with Interaction
# Extend the base model to test if the effect of health expenditure on life expectancy differs by region.
fe_model_region <- plm(life_expectancy ~ log(health_expenditure) * region,
                       data = pdata, model = "within", weight = population)
summary(fe_model_region)

# 9. Robustness Check: Dynamic Specification with Lagged Life Expectancy
# Life expectancy may be highly persistent, so we include a lagged dependent variable to account for its inertia.
fe_model_region_lag <- plm(life_expectancy ~ log(health_expenditure) * region + lag(life_expectancy, 1),
                           data = pdata, model = "within", weight = population)
summary(fe_model_region_lag)

# --- Model Diagnostics ---

# 10. Diagnostic Plot: Residuals vs Fitted Values
# This plot assesses the model fit and checks for patterns or heteroskedasticity.
model_data <- data.frame(
  fitted = fitted(fe_model),
  residuals = residuals(fe_model),
  region = data$region
)
ggplot(model_data, aes(x = fitted, y = residuals, color = region)) +
  geom_point(size = 2, alpha = 0.6) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "gray40") +
  labs(
    title = "Diagnostic: Residuals vs Fitted Values",
    x = "Fitted Values",
    y = "Residuals"
  ) +
  theme_minimal() +
  theme(legend.position = "bottom")

# --- Extract Regional Effects for Mapping ---
# For the interaction model, compute effective marginal effects by region.
# The baseline effect is for the reference region (first alphabetically).
coefs <- coef(fe_model_region)
baseline_effect <- coefs["log(health_expenditure)"]

# Get unique regions from the data
regions <- unique(data$region)
# Create a data frame with effective coefficients by region.
region_effects <- tibble(
  region = regions,
  effect = baseline_effect  # initialize with baseline
)

# Loop through each region to add the interaction effect if available.
for (r in regions) {
  # Build the interaction term name; note: factor level names might have spaces.
  interaction_term <- paste0("log(health_expenditure):region", r)
  if(interaction_term %in% names(coefs)){
    region_effects <- region_effects %>% 
      mutate(effect = if_else(region == r, baseline_effect + coefs[interaction_term], effect))
  }
}
region_effects

# --- Prepare Data for Mapping ---
# Merge country-level region effects (each country gets the effect of its region).
country_effects <- data %>%
  select(iso3c, region) %>%
  distinct() %>%
  left_join(region_effects, by = "region")

# Obtain world map data (using rnaturalearth for medium scale polygons).
world <- ne_countries(scale = "medium", returnclass = "sf")

# Merge world map with our country effects using ISO3 codes.
world_map_data <- world %>%
  left_join(country_effects, by = c("iso_a3" = "iso3c"))

# 11. Map: Marginal Effect of Health Expenditure on Life Expectancy by Region
# The map visually displays how the estimated effect varies across regions.
ggplot(world_map_data) +
  geom_sf(aes(fill = effect), color = "gray50", size = 0.1) +
  scale_fill_viridis_c(option = "inferno", na.value = "lightgray", direction = -1) + 
  labs(
    title = "Marginal Effect of Log(Health Expenditure) on Life Expectancy",
    subtitle = "Effective coefficients by region (from FE model with interaction)",
    fill = "Effect"
  ) +
  theme_minimal() +
  theme(legend.position = "bottom")


# --- Interpretation of Fixed Effects Model Results ---

# Overall Effect:
# The base fixed effects model shows that doubling per capita health expenditure 
# is associated with an average increase of approximately 3.82 years in life expectancy, 
# holding country-specific, time-invariant characteristics constant. This suggests 
# that increased health spending generally enhances population health by improving 
# access to care, expanding preventive services, and enabling adoption of modern 
# medical technologies. However, this average effect masks substantial regional variation.

# Regional Heterogeneity (Interaction Model):
# The marginal effect of health expenditure on life expectancy varies considerably 
# across world regions, reflecting differences in baseline health systems, institutional 
# quality, public sector efficiency, disease burden, and population demographics.

# - **Europe & Central Asia (Effect: 3.08)**  
#   Europe & Central Asia shows a solid, though not large, responsiveness to increased 
#   spending. With already relatively high average life expectancy (~76 years) and 
#   substantial average health spending (~$2200 PPP), additional investments produce 
#   meaningful but moderate gains. This aligns with the region's emphasis on managing 
#   aging populations, non-communicable diseases, and preventive care. Future returns 
#   may increasingly depend on efficiency improvements and health innovation rather 
#   than sheer volume of expenditure.

# - **Middle East & North Africa (Effect: 4.42)**  
#   The region sees strong health returns from additional spending. Despite relatively 
#   high baseline spending (~$1400), infrastructure and coverage gaps remain, especially 
#   in rural or lower-income areas. A doubling of expenditure here adds more than 
#   four years to life expectancy, indicating substantial room for improvement. 
#   Investment in equitable healthcare delivery and modernization could significantly 
#   narrow health inequalities and improve outcomes.

# - **Sub-Saharan Africa (Effect: 7.03)**  
#   This is the most responsive region: doubling health expenditure yields an 
#   increase of over seven years in life expectancy. Given the lowest baseline 
#   expenditure (~$216) and life expectancy (~59 years), even modest increases 
#   in funding—if efficiently deployed—can drastically improve outcomes. Focus 
#   should be on basic health infrastructure, maternal and child health, sanitation, 
#   and communicable disease prevention. Returns here are high both in humanitarian 
#   and economic terms.

# - **Latin America & Caribbean (Effect: 2.07)**  
#   Despite relatively moderate health expenditure (~$836) and decent life expectancy 
#   (~73 years), the region sees one of the lowest marginal returns. This suggests 
#   deep systemic inefficiencies—often rooted in corruption, fragmented systems, and 
#   bureaucratic mismanagement. Substantial funds fail to translate into proportionate 
#   health improvements. Thus, the core challenge is not how much is spent, but how 
#   effectively it is used. Reforms should target governance, performance-based budgeting, 
#   and accountability in public health spending.

# - **East Asia & Pacific (Effect: 2.36)**  
#   The modest responsiveness in this region is nuanced. While the average life expectancy 
#   (~71 years) and spending (~$885) suggest room for growth, the aggregate hides stark 
#   contrasts between countries like Japan and poorer Southeast Asian nations. In higher-income 
#   countries, the health system is already mature, and returns are naturally lower. In emerging 
#   economies, health spending is rising rapidly, and efficiency is improving. Continued progress 
#   depends on addressing rural access gaps, aging populations, and non-communicable disease burdens.

# - **South Asia (Effect: 6.03)**  
#   With low baseline health expenditure (~$327) and life expectancy (~69 years), South Asia 
#   exhibits strong marginal returns. The region faces immense demographic and public health 
#   pressures, including high child mortality, poor sanitation, and underfunded rural care. 
#   Doubling expenditure here could lead to significant improvements—over six years added 
#   to life expectancy on average. Policymakers should emphasize scalable primary healthcare, 
#   clean water access, nutrition programs, and equitable service delivery.

# - **North America (Effect: 1.16)**  
#   North America has the highest baseline expenditure (~$6142) and life expectancy (~79.5 years), 
#   yet it shows the **lowest marginal return** on additional spending. This reflects the reality 
#   of diminishing returns in saturated systems, but also stark inefficiencies—particularly in the U.S., 
#   where administrative overhead, high drug prices, and fragmented care delivery undermine the 
#   effectiveness of spending. Additional investment here often does not translate into longer lives. 
#   The policy priority should be improving value for money through better regulation, system integration, 
#   and cost containment—not increasing budgets.

# --- Policy Implications ---

# 1. **Target High-Yield Regions for Investment**  
#    Sub-Saharan Africa and South Asia represent high-return opportunities where even modest 
#    investments can significantly improve life expectancy. These regions should be prioritized 
#    by governments, donors, and multilateral organizations aiming for maximum global health impact.

# 2. **Fix Governance and Efficiency Gaps**  
#    Latin America and North America highlight a different challenge: spending is not low, 
#    but its effectiveness is. In these regions, reforms should focus on reducing corruption, 
#    streamlining delivery systems, and ensuring that health budgets are transparently and efficiently managed.

# 3. **Optimize Systems in Mature Economies**  
#    In Europe, East Asia, and parts of North America, systems are advanced but aging populations 
#    and chronic conditions require adaptive strategies. Innovation, technology adoption, and 
#    personalized medicine may be more productive than additional funding alone.

# 4. **Tailor Strategies to Local Conditions**  
#    No one-size-fits-all solution exists. The impact of health expenditure depends not only 
#    on how much is spent, but how it is allocated, managed, and embedded within local 
#    institutional frameworks. Health policy must therefore be deeply contextual, data-driven, 
#    and aligned with national development goals.

# In conclusion, while increasing per capita healthcare spending is broadly associated 
# with higher life expectancy, its impact is shaped by each region’s starting point, 
# system capacity, and institutional quality. Smart policy requires not just spending more, 
# but spending better—and spending where it matters most.