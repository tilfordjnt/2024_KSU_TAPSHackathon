# Import required libraries
import streamlit as st
import pandas as pd
import requests
import math
import datetime
from datetime import timedelta

# Set up the page configuration
st.set_page_config(page_title="Irrigation Advisory Tool", layout="centered")

# Define custom CSS for styling
st.markdown("""
    <style>
        h1 {
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

# Sidebar: location and crop information
st.sidebar.header("Location and Crop Details")
town = st.sidebar.text_input("Town", "Manhattan")

# Sidebar: planting date and crop type selection
planting_date = st.sidebar.date_input("Planting Date", datetime.date.today())
crop_type = st.sidebar.selectbox("Crop Type", ["Corn", "Wheat", "Soybean"])

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
st.sidebar.markdown(f"**Current Crop Stage**: {crop_stage}")

# Sidebar for initial soil moisture
st.sidebar.header("Initial Soil Moisture and Irrigation Data")
st.sidebar.write("Choose the initial soil moisture date and amount:")

# Define irrigation date range starting 21 days before planting
start_irrigation_date = planting_date - timedelta(days=21)
initial_soil_moisture_date = st.sidebar.date_input("Initial Soil Moisture Date", start_irrigation_date)
initial_soil_moisture = st.sidebar.number_input("Initial Soil Moisture (%)", min_value=0.0, step=0.1)

# Sidebar: irrigation events
end_irrigation_date = current_date
irrigation_dates_range = pd.date_range(start=start_irrigation_date, end=end_irrigation_date)
selected_irrigation_dates = st.sidebar.multiselect(
    "Irrigation Days", irrigation_dates_range.to_list(), default=[]
)

# Dictionary for irrigation amounts
irrigation_events = {}
for date in selected_irrigation_dates:
    amount_in = st.number_input(f"Irrigation amount on {date.strftime('%Y-%m-%d')}", min_value=0.0, step=0.1, key=f"irrigation_{date}")
    if amount_in > 0:
        irrigation_events[date] = amount_in  # Store the amount in inches

# OpenWeather Historical Weather Data function
def get_openweather_historical_data(latitude, longitude, start_date, end_date):
    api_key = "YOUR_OPENWEATHER_API_KEY"  # Replace with your actual API key
    base_url = "https://api.openweathermap.org/data/2.5/onecall/timemachine"
    historical_data = []
    
    dates = pd.date_range(start=start_date, end=end_date)
    for date in dates:
        timestamp = int(date.timestamp())
        url = f"{base_url}?lat={latitude}&lon={longitude}&dt={timestamp}&appid={api_key}&units=imperial"
        response = requests.get(url)
        
        if response.status_code == 200:
            data = response.json()
            daily_data = {
                "date": date,
                "temperature": data['current']['temp'],
                "humidity": data['current']['humidity'],
                "wind_speed": data['current']['wind_speed'],
                "precipitation": data['current'].get('rain', {}).get('1h', 0) * 0.0393701,  # Convert mm to inches
                "solar_radiation": 10  # Assuming average solar radiation; OpenWeather lacks solar radiation data directly
            }
            historical_data.append(daily_data)
        else:
            st.write(f"⚠️ Could not retrieve data for {date}.")
    
    return historical_data

# ET₀ calculation using the FAO Penman-Monteith equation
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
    ET0 = ((0.408 * delta_slope * Rn) + (gamma * (900 / (T + 273)) * u2 * vpd)) / (delta_slope + gamma * (1 + 0.34 * u2))
    
    return ET0 * 0.0393701  # Convert from mm/day to inches/day

# Function to calculate estimated soil moisture with OpenWeather historical data
def calculate_soil_moisture_with_historical_data(initial_moisture, historical_data, latitude, decay_factor=0.995):
    current_moisture = initial_moisture
    for daily_data in historical_data:
        precipitation = daily_data["precipitation"]
        solar_radiation = daily_data["solar_radiation"]
        
        # Calculate ET₀ based on historical solar radiation
        daily_et0 = calculate_et0(
            temp=daily_data["temperature"],
            wind_speed=daily_data["wind_speed"],
            humidity=daily_data["humidity"],
            solar_radiation=solar_radiation,
            latitude=latitude
        )

        # Apply decay factor only to the added moisture
        added_moisture = precipitation * decay_factor

        # Adjust soil moisture
        current_moisture = max(0, min((current_moisture + added_moisture - daily_et0), 100))
    
    return current_moisture

# Button to fetch weather data, calculate soil moisture with historical data, and provide irrigation advice
if st.sidebar.button("Get Irrigation Advice"):
    weather_data = get_openweather_historical_data(town)  # Get town's coordinates here if required
    if weather_data:
        latitude, longitude = weather_data["latitude"], weather_data["longitude"]
        historical_data = get_openweather_historical_data(latitude, longitude, planting_date.strftime('%Y%m%d'), current_date.strftime('%Y%m%d'))
        
        if historical_data:
            estimated_soil_moisture = calculate_soil_moisture_with_historical_data(initial_soil_moisture, historical_data, latitude)
            
            # Irrigation advice
            if estimated_soil_moisture < soil_moisture_targets["Corn"][crop_stage][0]:
                advice = "High irrigation needed: Apply 1.0 inch of water."
            elif estimated_soil_moisture > soil_moisture_targets["Corn"][crop_stage][1]:
                advice = "Irrigation not required; soil moisture is sufficient."
            else:
                advice = "Moderate irrigation recommended: Apply 0.5 inches of water."
            
            st.write(f"**Estimated Soil Moisture**: {estimated_soil_moisture:.2f}%")
            st.write(f"**Irrigation Advice**: {advice}")
        else:
            st.write("⚠️ Could not retrieve historical weather data.")
    else:
        st.write("⚠️ Could not retrieve weather data for the specified location.")

