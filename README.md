import streamlit as st
import pandas as pd
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

# Sidebar for irrigation input
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
for date in selected_irrigation_dates:
    amount_in = st.number_input(f"Irrigation amount on {date.strftime('%Y-%m-%d')}", min_value=0.0, step=0.1, key=f"irrigation_{date}")
    if amount_in > 0:
        irrigation_events[date] = amount_in  # Store the amount in inches

# Only display results if irrigation data is entered
if irrigation_events:
    
    # Define helper functions
    def get_weather_data_with_solar(town):
        api_key = "your_openweather_api_key"
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

    # Calculate daily soil moisture and ET₀
    def update_soil_moisture(weather_data, kc, initial_moisture, irrigation_events):
        soil_moisture = initial_moisture
        for date, irrigation_amount in irrigation_events.items():
            solar_radiation = get_nasa_solar_radiation(weather_data["latitude"], weather_data["longitude"])
            if solar_radiation:
                et0_daily = calculate_et0(
                    weather_data["temperature"],
                    weather_data["wind_speed"],
                    weather_data["humidity"],
                    solar_radiation,
                    weather_data["latitude"]
                )
                etc = et0_daily * kc
                precipitation = weather_data["precipitation_inches"]
                soil_moisture = soil_moisture - etc + precipitation + irrigation_amount
                soil_moisture = min(max(soil_moisture, 0), 100)
        return soil_moist
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

    # Calculate daily soil moisture and ET₀
    def update_soil_moisture(weather_data, kc, initial_moisture, irrigation_events):
        soil_moisture = initial_moisture
        for date, irrigation_amount in irrigation_events.items():
            solar_radiation = get_nasa_solar_radiation(weather_data["latitude"], weather_data["longitude"])
            if solar_radiation:
                et0_daily = calculate_et0(
                    weather_data["temperature"],
                    weather_data["wind_speed"],
                    weather_data["humidity"],
                    solar_radiation,
                    weather_data["latitude"]
                )
                etc = et0_daily * kc
                precipitation = weather_data["precipitation_inches"]
                soil_moisture = soil_moisture - etc + precipitation + irrigation_amount
                soil_moisture = min(max(soil_moisture, 0), 100)
        return soil_moisture

    # Dashboard results and irrigation recommendation
    with st.container():
        st.subheader("📊 Dashboard Results")
        
        weather_data = get_weather_data_with_solar(town)
        if weather_data:
            initial_soil_moisture = 50
            soil_moisture = update_soil_moisture(weather_data, kc_values[crop_type][crop_stage], initial_soil_moisture, irrigation_events)
            
            st.write(f"Soil Moisture: {soil_moisture:.2f}%")
            
            if soil_moisture < 40:
                st.write("🚨 **Irrigation Needed:** Apply 1.0 inches of water.")
            elif 40 <= soil_moisture < 60:
                st.write("⚠️ **Irrigation Suggested:** Apply 0.6 inches of water.")
            else:
                st.write("✅ **Soil moisture is sufficient; no additional irrigation is needed.**")

        else:
            st.write("⚠️ Weather data unavailable for the specified location.")

        # Handle cases where solar radiation data is unavailable
        if weather_data and not get_nasa_solar_radiation(weather_data["latitude"], weather_data["longitude"]):
            st.write("⚠️ Could not retrieve solar radiation data; ET₀ calculation was adjusted based on available weather data only.")

# Clear cache button
if st.sidebar.button('Clear Cache'):
    st.cache_data.clear()

st.markdown("---")
st.write("ℹ️ **Note:** This model uses daily ET₀ calculations based on historical and current weather data, along with irrigation events to provide location-based recommendations for irrigation needs.")
