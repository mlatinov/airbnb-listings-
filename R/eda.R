
#### Source utilis function ####
source("R/utilis.R")

#### Look at distributions of all numerical features ####
viz_dist(model_data, feature = "price")
viz_dist(model_data, feature = "host_months_host")
viz_dist(model_data, feature = "host_months_user")
viz_dist(model_data, feature = "accommodates")

#### Look at the distribution condition by variable ###
## ACC x RT
viz_dist_cond(model_data = model_data, dist_feature = "accommodates", adjust = "room_type")

## Price x RT ## 
ggplot(log_price_model_data, aes(x = log_price, fill = room_type))+
  geom_density(alpha = 0.5) +
  theme_minimal() +
  scale_color_viridis_d(option = "inferno")+
  labs(
    x = "Price",
    y = "Density",
    title = "Log Price adjusting for Room Type"
  )

## SH x RT ##
model_data %>%
  group_by(room_type) %>%
  summarise(
    superhost = mean(superhost)
  )%>%
  ggplot(aes(x = superhost, y = room_type, fill = room_type))+
  geom_col() +
  theme_minimal() +
  scale_fill_viridis_d(option = "A") +
  labs(
    x = "Super Host",
    y = "Room Type",
    title = "Room Type x Super Host"
  ) +
  theme(
    title = element_text(size = 15)
  )

## SH x log Price ##
ggplot(data = log_price_model_data ,aes(x = log_price, fill = as.factor(superhost)))+
  geom_boxplot() +
  scale_fill_viridis_d(option = "A", begin = 0.3, end = 0.8)+
  coord_flip()+
  theme_minimal()+
  labs(
    x = "Log Price",
    fill = "Super Host",
    title = "Log Price by Super Host"
  )

### Log price vs Accommodates ##
viz_relan(model_data = model_data, x_var = "accommodates", y_var = "log_price")

### Log price vs Minimum Nights ##
viz_relan(model_data = model_data, x_var = "minimum_nights", y_var = "log_price")

### Log price vs Guest Lead Months ## 
viz_relan(model_data = model_data, x_var = "guest_lead_months", y_var = "log_price")

### Log price x Super Host x ACC ### 
viz_relan_z(model_data = model_data, x_var = "accommodates", y_var = "log_price",adjust = "superhost")

### Log price x Super Host x Minimum Nights ### 
viz_relan_z(model_data = model_data, x_var = "minimum_nights", y_var = "log_price",adjust = "superhost")

### Log price x Super Host x Guest Lead Months ### 
viz_relan_z(model_data = model_data, x_var = "guest_lead_months", y_var = "log_price",adjust = "superhost")

#### Comparison between the Simulation Data and Sample ####
sim_data <- simulate_airbnb_v1()

## Compare Distributions ##
comapare_dist(sim_data = sim_data, model_data = model_data, x_var = "log_price")
comapare_dist(sim_data = sim_data, model_data = model_data, x_var = "accommodates")
comapare_dist(sim_data = sim_data, model_data = model_data, x_var = "minimum_nights")
comapare_dist(sim_data = sim_data, model_data = model_data, x_var = "guest_lead_months")

## Compare Relantionships ##
compare_relan(sim_data = sim_data, model_data = model_data, x_var = "accommodates", y_var = "log_price")
compare_relan(sim_data = sim_data, model_data = model_data, x_var = "minimum_nights", y_var = "log_price")
compare_relan(sim_data = sim_data, model_data = model_data, x_var = "guest_lead_months", y_var = "log_price")

## Compare Relantionships between 3 variables 
compare_relan_z(sim_data = sim_data, model_data = model_data, x_var = "accommodates", y_var = "log_price", z_var = "room_type")
compare_relan_z(sim_data = sim_data, model_data = model_data, x_var = "accommodates", y_var = "log_price", z_var = "superhost")
compare_relan_z(sim_data = sim_data, model_data = model_data, x_var = "minimum_nights", y_var = "log_price", z_var = "room_type")
compare_relan_z(sim_data = sim_data, model_data = model_data, x_var = "minimum_nights", y_var = "log_price", z_var = "superhost")
compare_relan_z(sim_data = sim_data, model_data = model_data, x_var = "guest_lead_months", y_var = "log_price", z_var = "room_type")
compare_relan_z(sim_data = sim_data, model_data = model_data, x_var = "guest_lead_months", y_var = "log_price", z_var = "superhost")
