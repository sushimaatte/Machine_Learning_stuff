
library(fpp3)

# https://rpubs.com/OrliKhaim/DATA624_HW1
# https://rpubs.com/Vanessasupit/EconometricWeek10
# https://rstudio-pubs-static.s3.amazonaws.com/1012118_1c07814c0b0b4c94bda30fcf31a54055.html



# 1. Explore the following four time series: Bricks from aus_production, Lynx from pelt, Close from gafa_stock, Demand from vic_elec.

# Bricks from aus_production - Quarterly - Clay brick production in millions of bricks.
bricks_data <- aus_production %>% select(Bricks) 
# plot 
aus_production %>% autoplot(Bricks)

# Lynx from pelt - Yearly - Lynx:	The number of Canadian Lynx pelts traded
lynx_data <- pelt %>% select(Lynx) 
# plot 
pelt %>% autoplot(Lynx)

# Close from gafa_stock - varying time interval - Close:	The closing price for the stock.
stock_data <- gafa_stock %>% select(Close)
gafa_stock  %>%  autoplot(Close)

# Demand from vic_elec - half hourly - Total electricity demand in MWh.
vic_data <- vic_elec %>% select(Demand)  
vic_elec %>% autoplot(Demand) + labs(y = "demand", title ="Total electricity demand in MWh")


# 2. Use filter() to find what days corresponded to the peak closing price for each of the four stocks in gafa_stock.

# types of stocks
unique(gafa_stock$Symbol)

# peak closing price for each of the four stocks
gafa_stock %>%
  select(Symbol, Date, Close) %>%
  group_by(Symbol) %>%
  filter(Close == max(Close))



# 3. Download the file tute1.csv

# read the file 
tute1 <- readr::read_csv("tute1.csv")
View(tute1)

# Convert the data to time series
mytimeseries <- tute1 %>%
  mutate(Quarter = yearquarter(Quarter)) %>% # 1981-03-01 > 1981 Q1
  as_tsibble(index = Quarter)

# Construct time series plots of each of the three series
mytimeseries |>
  pivot_longer(-Quarter) |>
  ggplot(aes(x = Quarter, y = value, colour = name)) +
  geom_line() +
  facet_grid(name ~ ., scales = "free_y")



# 4. USgas

# Create a tsibble from us_total with year as the index and state as the key.

us_total_ts <- us_total %>%
  as_tibble(key = state,
            index = year)

head(us_total_ts)

# Plot the annual natural gas consumption by state for the New England area 
  #(comprising the states of Maine, Vermont, New Hampshire, Massachusetts, Connecticut and Rhode Island).
us_total_ts %>%
  filter(state %in% c('Maine', 'Vermont', 'New Hampshire', 'Massachusetts', 'Connecticut', 'Rhode Island')) %>%
  ggplot(aes(x = year, y = y/1e6, colour = state)) +
  geom_line() +
  labs(title = "Annual Natural Gas Consumption in New England",
       y = "Consumption")  
 #  facet_grid(state ~., scales = "free_y") 


# 5. Download tourism.xlsx

# read data
tourism_data <- readxl::read_excel("tourism.xlsx")

# Create a tsibble which is identical to the tourism tsibble from the tsibble package.
tourism_data <- tourism_data %>%
  mutate(Quarter = yearquarter(Quarter)) %>%
  as_tsibble(key = c(Region, State, Purpose))

# Find what combination of Region and Purpose had the maximum number of overnight trips on average

tourism_data %>% 
  group_by(Region, Purpose) %>%
  summarise(avg_trips = mean(Trips)) %>%
  filter(avg_trips == max(avg_trips))  %>%
  arrange(desc(avg_trips))
  
tourism_data %>% 
  group_by(Region, Purpose) %>%
  summarise(avg_trips = mean(Trips)) %>%
  ungroup() %>%
  filter(avg_trips == max(avg_trips))

# ungroup() is used here to make sure that the filtering operation (filter) considers the entire data frame without group-specific constraints


# 6. The aus_arrivals data set comprises quarterly international arrivals to Australia from Japan, New Zealand, UK and the US.


# LOOOOOOOOL



