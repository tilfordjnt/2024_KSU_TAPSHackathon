# 2024_KSU_TAPSHackathon
2024 Kansas State University TAPS Hackathon competition for Water Wise Wildcats
import streamlit as st
import pandas as pd
import requests
import math
import datetime

# Clear cache button
if st.sidebar.button('Clear Cache'):
    st.cache_data.clear()

# Title of the dashboard
st.title("Irrigation Advisory Tool")

# Display current date
current_date_display = datetime.datetime.now().strftime("%B %d, %Y")
st.write(f"Current Date: {current_date_display}")

# Sidebar for user input
st.sidebar.header("Enter Your Location")
town = st.sidebar.text_input("Enter the town in Kansas", "Manhattan")

# Sidebar for planting date
st.sidebar.header("Crop Information")
planting_date = st.sidebar.date_input("Enter Planting Date", datetime.date.today())

# Sidebar for crop type and irrigation setup
crop_type = st.sidebar.selectbox("Select Crop Type", ["Corn", "Wheat", "Soybean"])

# Typical growth cycle periods for crops (in days)
growth_periods = {
    "Corn": {"Initial": 30, "Mid-Season": 60, "Late-Season": 30},
    "Wheat": {"Initial": 25, "Mid-Season": 70, "Late-Season": 35},
    "Soybean": {"Initial": 20, "Mid-Season": 50, "Late-Season": 30}
}

# Function to calculate current crop stage based on planting date
def get_crop_stage(planting_date, crop_type, current_date):
    days_since_planting = (current_date - planting_date).days
    growth_stages = growth_periods[crop_type]

    if days_since_planting <= growth_stages["Initial"]:
        return "Initial"
    elif days_since_planting <= (growth_stages["Initial"] + growth_stages["Mid-Season"]):
        return "Mid-Season"
    else:
        return "Late-Season"

# Calculate crop stage based on current date
current_date = datetime.date.today()
crop_stage = get_crop_stage(planting_date, crop_type, current_date)
st.sidebar.write(f"Current Crop Stage: {crop_stage}")

# Sidebar for irrigation entry
st.sidebar.header("Irrigation Data")
st.sidebar.write(f"Enter irrigation events for the past few days:")

# Dictionary to hold irrigation amounts
irrigation_events = {}

# Multi-date picker: Allow users to select irrigation days
selected_irrigation_dates = st.sidebar.multiselect(
    "Select Irrigation Days", pd.date_range(start=current_date - pd.Timedelta(days=7), end=current_date).to_list(), default=[]
)

# Display water amount input box for each selected day
st.write("Enter irrigation amount for each selected day (in inches):")
for date in selected_irrigation_dates:
    amount_in = st.number_input(f"Irrigation amount on {date.strftime('%Y-%m-%d')}", min_value=0.0, step=0.1, key=f"irrigation_{date}")
    if amount_in > 0:
        irrigation_events[date] = amount_in  # Store the amount in inches

# Function to get real-time weather data using OpenWeather API
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

# FAO Penman-Monteith model for ET₀ calculation
def calculate_et0(temp, wind_speed, humidity):
    T = temp  
    u2 = wind_speed  
    e_a = humidity / 100

    # Penman-Monteith parameters in imperial units
    e_s = 0.6108 * math.exp((17.27 * (T - 32) * 5/9) / ((T - 32) * 5/9 + 237.3))  
    e_s_minus_e_a = e_s - e_a
    gamma = 0.665 * 0.001 * 101.3
    delta = 4098 * (0.6108 * math.exp((17.27 * (T - 32) * 5/9) / ((T - 32) * 5/9 + 237.3))) / (((T - 32) * 5/9 + 237.3) ** 2)

    ET0 = (0.408 * delta * (10) + gamma * (900 / ((T - 32) * 5/9 + 273)) * u2 * e_s_minus_e_a) / (delta + gamma * (1 + 0.34 * u2))
    return ET0 * 0.0393701  # Convert from mm/day to inches/day

# Crop coefficient (Kc) adjustment based on growth stage
kc_values = {
    "Corn": {"Initial": 0.3, "Mid-Season": 1.2, "Late-Season": 0.5},
    "Wheat": {"Initial": 0.7, "Mid-Season": 1.15, "Late-Season": 0.3},
    "Soybean": {"Initial": 0.5, "Mid-Season": 1.15, "Late-Season": 0.5}
}

kc = kc_values[crop_type][crop_stage]

# Function to calculate ETc using ET₀ and Kc
def calculate_etc(et0, kc):
    return et0 * kc

# Decay model for past irrigation events (decay reduces irrigation impact over time)
def apply_decay_to_irrigation_events(irrigation_events, current_date):
    decayed_irrigation = 0
    decay_rate = 0.9  # Decay factor to reduce water effect over time

    for event_date, irrigation_amount in irrigation_events.items():
        days_since_irrigation = (current_date - event_date.date()).days
        if days_since_irrigation >= 0:
            decayed_irrigation += irrigation_amount * (decay_rate ** days_since_irrigation)
    
    return decayed_irrigation

# Water balance function for current data, using FAO equation and irrigation decay model
def update_soil_moisture_with_current_data(weather_data, irrigation_events, current_date):
    soil_moisture = 50  # Starting soil moisture (assumed 50%)
    
    # Get today's ET₀ from weather data
    et0 = calculate_et0(weather_data["temperature"], weather_data["wind_speed"], weather_data["humidity"])
    etc = calculate_etc(et0, kc)
    
    # Subtract ETc (water loss) and add any forecast precipitation
    soil_moisture -= etc
    soil_moisture += weather_data["precipitation_inches"]
    
    # Apply decayed irrigation amounts from recent irrigation events
    decayed_irrigation = apply_decay_to_irrigation_events(irrigation_events, current_date)
    soil_moisture += decayed_irrigation
    
    # Keep soil moisture within realistic bounds (0% to 100%)
    soil_moisture = min(max(soil_moisture, 0), 100)
    
    return soil_moisture

# Irrigation advice based on updated soil moisture
def irrigation_schedule(soil_moisture):
    if soil_moisture < 40:
        irrigation_needed_inches = 1.0  
        return f"Urgent: Apply {irrigation_needed_inches:.2f} inches of water to prevent crop stress."
    elif 40 <= soil_moisture < 60:
        irrigation_needed_inches = 0.6  
        return f"Apply {irrigation_needed_inches:.2f} inches of water. Monitor rainfall."
    else:
        return "Good: Soil moisture is sufficient, and no irrigation is needed."

# Main function to handle workflow
if st.sidebar.button("Get Weather Data and Irrigation Advice"):
    weather_data = get_weather_data_with_solar(town)
    if weather_data:
        st.write(f"Weather in {town}:")
        st.write(f"Temperature: {weather_data['temperature']} °F")
        st.write(f"Humidity: {weather_data['humidity']} %")
        st.write(f"Wind Speed: {weather_data['wind_speed']} mph")
        st.write(f"Precipitation forecast: {weather_data['precipitation_inches']:.2f} inches")

        # Use current weather data and recent irrigation events with decay model to update soil moisture
        soil_moisture = update_soil_moisture_with_current_data(weather_data, irrigation_events, current_date)
        
        st.write(f"Estimated Soil Moisture: {soil_moisture:.2f}%")

        # Generate irrigation advice based on updated soil moisture
        irrigation_advice = irrigation_schedule(soil_moisture)
        st.write(f"Irrigation Advice: {irrigation_advice}")
