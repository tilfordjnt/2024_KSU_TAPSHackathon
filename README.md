import streamlit as st
import pandas as pd
import matplotlib.pyplot as plt
import numpy as np
import requests
import math
import datetime
from datetime import timedelta

# Set the page layout and theme
st.set_page_config(page_title="Irrigation Advisory Tool", layout="centered")

# Define custom CSS for styling
st.markdown("""
    <style>
        h1, h2, h3, h4, h5, h6 {
            color: #2E8B57;
            font-family: 'Arial', sans-serif;
        }
        .stButton>button {
            background-color: #2E8B57;
            color: white;
        }
        .css-1aumxhk {
            text-align: center;
            color: #2E8B57;
            font-weight: bold;
            font-size: 18px;
        }
        .css-10trblm {
            color: #2E8B57;
            font-family: 'Arial', sans-serif;
        }
    </style>
""", unsafe_allow_html=True)

# Title of the dashboard
st.title("🌾 Irrigation Advisory Tool")
st.subheader("Helping You Make Data-Driven Irrigation Decisions")

# Load the NDVI and irrigation data from the specified Excel files
ndvi_data = pd.read_excel('/Users/jeremydavies/Downloads/Book 2 (2).xlsx', skiprows=2)
irrigation_data = pd.read_excel('/Users/jeremydavies/Downloads/Book 1 (1).xlsx', header=1)

# Clean up NDVI data by renaming columns to numerical IDs for merging consistency
ndvi_data = ndvi_data.rename(columns={"DATE": "Date"})
ndvi_data.columns = ["Date", 2, 4, 5, 13, 15, 16, 18, 21, 22]
ndvi_data_long = ndvi_data.melt(id_vars="Date", var_name="ID", value_name="NDVI")
ndvi_data_long.dropna(inplace=True)

# Remove "Total" column and convert date columns in irrigation data
irrigation_data = irrigation_data.loc[:, irrigation_data.columns != "Total"]
irrigation_data.columns = ["ID"] + [pd.to_datetime(col).date() for col in irrigation_data.columns[1:]]
irrigation_data_long = irrigation_data.melt(id_vars="ID", var_name="Date", value_name="Irrigation")
irrigation_data_long['Date'] = pd.to_datetime(irrigation_data_long['Date'], errors='coerce')
irrigation_data_long['Irrigation'].fillna(0, inplace=True)

# Merge cleaned data for plotting
merged_data = pd.merge(ndvi_data_long, irrigation_data_long, on=['Date', 'ID'], how='inner')

# Aggregate total irrigation and average NDVI by plot ID
aggregated_data = merged_data.groupby("ID").agg({"Irrigation": "sum", "NDVI": "mean"}).reset_index()

# Plot Aggregated Scatter Plot with Trend Line using matplotlib
st.subheader("Total Irrigation vs. Average NDVI per Plot")
fig, ax = plt.subplots(figsize=(10, 6))
ax.scatter(aggregated_data["Irrigation"], aggregated_data["NDVI"], s=100, alpha=0.7, color="#2E8B57", label="Data Points")
# Fit and plot a trend line
m, b = np.polyfit(aggregated_data["Irrigation"], aggregated_data["NDVI"], 1)
ax.plot(aggregated_data["Irrigation"], m * aggregated_data["Irrigation"] + b, color="red", label="Trend Line")
ax.set_title("Relationship between Total Irrigation and Average NDVI", fontsize=16)
ax.set_xlabel("Total Irrigation (inches)", fontsize=14)
ax.set_ylabel("Average NDVI", fontsize=14)
ax.legend()
ax.grid(True, linestyle='--', alpha=0.5)

# Display the plot
st.pyplot(fig)

# Sidebar: location and crop information
st.sidebar.header("🗺️ Location and Crop Details")
town = st.sidebar.text_input("Enter the town in Kansas", "Manhattan")

# Sidebar: planting date and crop type selection
planting_date = st.sidebar.date_input("🌱 Enter Planting Date", datetime.date.today())
crop_type = st.sidebar.selectbox("🌾 Select Crop Type", ["Corn", "Wheat", "Soybean"])

# Crop growth periods
growth_periods = {
    "Corn": {"Initial": 30, "Mid-Season": 60, "Late-Season": 30},
    "Wheat": {"Initial": 25, "Mid-Season": 70, "Late-Season": 35},
    "Soybean": {"Initial": 20, "Mid-Season": 50, "Late-Season": 30}
}

# Calculate crop stage
def get_crop_stage(planting_date, crop_type, current_date):
    days_since_planting = (current_date - planting_date).days
    growth_stages = growth_periods[crop_type]
    if days_since_planting <= growth_stages["Initial"]:
        return "Initial"
    elif days_since_planting <= (growth_stages["Initial"] + growth_stages["Mid-Season"]):
        return "Mid-Season"
    else:
        return "Late-Season"

# Display crop stage
current_date = datetime.date.today()
crop_stage = get_crop_stage(planting_date, crop_type, current_date)
st.sidebar.markdown(f"**🌱 Current Crop Stage**: {crop_stage}")

# Sidebar: irrigation entry
st.sidebar.header("💧 Irrigation Data")
st.sidebar.write("Enter irrigation events starting 21 days before planting:")

# Define irrigation date range starting 21 days before planting
start_irrigation_date = planting_date - timedelta(days=21)
end_irrigation_date = current_date
irrigation_dates_range = pd.date_range(start=start_irrigation_date, end=end_irrigation_date)

# Multi-date picker for irrigation days within range
selected_irrigation_dates = st.sidebar.multiselect(
    "Select Irrigation Days", irrigation_dates_range.to_list(), default=[]
)

# Dictionary for irrigation amounts
irrigation_events = {}

# Display water amount input box for each selected day
st.write("💧 **Enter Irrigation Amount for Each Selected Day (in inches):**")
for date in selected_irrigation_dates:
    amount_in = st.number_input(f"Irrigation amount on {date.strftime('%Y-%m-%d')}", min_value=0.0, step=0.1, key=f"irrigation_{date}")
    if amount_in > 0:
        irrigation_events[date] = amount_in  # Store the amount in inches

# Function to get weather data from OpenWeather API
def get_weather_data_with_solar(town):
    api_key = "e6139314284b647780f9e4e3f8e18ca2"
    base_url = f"http://api.openweathermap.org/data/2.5/forecast?q={town},US&appid={api_key}&units=imperial"
    
    response = requests.get(base_url)
    if response.status_code == 200:
        data = response.json()
        rainfall_inches = sum([data['list'][i]['rain'].get('3h', 0) if 'rain' in data['list'][i] else 0 for i in range(0, 8)])
        
        latitude = data['city']['coord']['lat']
        longitude = data['city']['coord']['lon']
        
        weather = {
            "temperature": data['list'][0]['main']['temp'],
            "humidity": data['list'][0]['main']['humidity'],
            "wind_speed": data['list'][0]['wind']['speed'],
            "precipitation_inches": rainfall_inches * 0.0393701,  # Convert mm to inches
            "latitude": latitude,
            "longitude": longitude
        }
        return weather
    else:
        return None

# Function to retrieve solar radiation from NASA POWER API
def get_nasa_solar_radiation(latitude, longitude):
    today = datetime.date.today()
    base_url = f"https://power.larc.nasa.gov/api/temporal/daily/point?parameters=ALLSKY_SFC_SW_DWN&community=AG&longitude={longitude}&latitude={latitude}&start={today.strftime('%Y%m%d')}&end={today.strftime('%Y%m%d')}&format=JSON"
    
    response = requests.get(base_url)
    if response.status_code == 200:
        data = response.json()
        solar_radiation = data['properties']['parameter']['ALLSKY_SFC_SW_DWN'][today.strftime('%Y%m%d')]
        return solar_radiation
    else:
        return None

# FAO Penman-Monteith model for ET₀ calculation with all parameters
def calculate_et0(temp, wind_speed, humidity, solar_radiation, latitude):
    T = (temp - 32) * 5/9  # Convert temperature to Celsius
    u2 = wind_speed * 0.44704  # Convert wind speed from mph to m/s
    e_s = 0.6108 * math.exp((17.27 * T) / (T + 237.3))  # Saturation vapor pressure in kPa
    e_a = e_s * (humidity / 100)  # Actual vapor pressure
    vpd = e_s - e_a  # Vapor pressure deficit
    
    # Constants
    gamma = 0.665 * 0.001 * 101.3  # Psychrometric constant in kPa/°C
    
    # Extraterrestrial radiation (Ra)
    J = datetime.datetime.now().timetuple().tm_yday  # Day of the year
    lat_rad = math.radians(latitude)
    dr = 1 + 0.033 * math.cos(2 * math.pi / 365 * J)
    delta = 0.409 * math.sin(2 * math.pi / 365 * J - 1.39)
    omega = math.acos(-math.tan(lat_rad) * math.tan(delta))
    Ra = (24 * 60 / math.pi) * 0.0820 * dr * (omega * math.sin(lat_rad) * math.sin(delta) + math.cos(lat_rad) * math.cos(delta) * math.sin(omega))

    # Net radiation (Rn)
    Rns = (1 - 0.23) * solar_radiation  # Net shortwave radiation
    Rnl = (4.903e-9 * ((T + 273.16)**4) * (0.34 - 0.14 * math.sqrt(e_a)) * ((1.35 * (solar_radiation / Ra)) - 0.35))  # Net longwave radiation
    Rn = Rns - Rnl  # Net radiation in MJ/m^2/day

    # Slope of the vapor pressure curve
    delta_slope = (4098 * e_s) / ((T + 237.3) ** 2)

    # FAO Penman-Monteith equation for ET₀
    ET0 = ((0.408 * delta_slope * (Rn - 0)) + (gamma * (900 / (T + 273)) * u2 * vpd)) / (delta_slope + gamma * (1 + 0.34 * u2))
    
    return ET0 * 0.0393701  # Convert from mm/day to inches/day

# Crop coefficient (Kc) adjustment based on growth stage
kc_values = {
    "Corn": {"Initial": 0.3, "Mid-Season": 1.2, "Late-Season": 0.5},
    "Wheat": {"Initial": 0.7, "Mid-Season": 1.15, "Late-Season": 0.3},
    "Soybean": {"Initial": 0.5, "Mid-Season": 1.15, "Late-Season": 0.5}
}

kc = kc_values[crop_type][crop_stage]

# Irrigation advice function
def irrigation_schedule(soil_moisture):
    if soil_moisture < 40:
        irrigation_needed_inches = 1.0  
        return f"Urgent: Apply {irrigation_needed_inches:.2f} inches of water to prevent crop stress."
    elif 40 <= soil_moisture < 60:
        irrigation_needed_inches = 0.6  
        return f"Apply {irrigation_needed_inches:.2f} inches of water. Monitor rainfall."
    else:
        return "Good: Soil moisture is sufficient, and no irrigation is needed."

# Display dashboard results in reserved bottom section
with st.container():
    st.subheader("📊 Dashboard Results")
    
    if st.sidebar.button("Get Weather Data and Irrigation Advice"):
        weather_data = get_weather_data_with_solar(town)
        if weather_data:
            st.write(f"Weather in {town}:")
            st.write(f"Temperature: {weather_data['temperature']} °F")
            st.write(f"Humidity: {weather_data['humidity']} %")
            st.write(f"Wind Speed: {weather_data['wind_speed']} mph")
            st.write(f"Precipitation forecast: {weather_data['precipitation_inches']:.2f} inches")

            # Fetch and display solar radiation, calculate ET₀ and provide irrigation advice
            solar_radiation = get_nasa_solar_radiation(weather_data["latitude"], weather_data["longitude"])
            if solar_radiation:
                et0 = calculate_et0(
                    weather_data["temperature"],
                    weather_data["wind_speed"],
                    weather_data["humidity"],
                    solar_radiation,
                    weather_data["latitude"]
                )
                etc = et0 * kc
                st.write(f"Calculated ETc: {etc:.2f} inches/day")

                # Example soil moisture calculation for advice
                initial_soil_moisture = 50
                soil_moisture = initial_soil_moisture - etc + weather_data["precipitation_inches"]
                soil_moisture = min(max(soil_moisture, 0), 100)

                irrigation_advice = irrigation_schedule(soil_moisture)
                st.write(f"Irrigation Advice: {irrigation_advice}")
            else:
                st.write("Could not retrieve solar radiation data.")
        else:
            st.write("Could not retrieve weather data.")
    
