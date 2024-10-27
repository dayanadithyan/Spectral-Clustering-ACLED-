import pandas as pd
import numpy as np
import matplotlib.pyplot as plt
import seaborn as sns
from datetime import datetime
import plotly.express as px
import plotly.graph_objects as go
from plotly.subplots import make_subplots
import streamlit as st

# Check if we're running in Streamlit
IN_STREAMLIT = True
try:
    st.session_state
except:
    IN_STREAMLIT = False

class EventsAnalyzer:
    def __init__(self):
        # Set styling - using a valid style name
        plt.style.use('seaborn-v0_8')  # Changed from 'seaborn' to 'seaborn-v0_8'
        sns.set_theme()  # This is the modern way to set seaborn's style
        
        # Configure pandas display options
        pd.set_option('display.max_columns', None)
        pd.set_option('display.max_rows', 100)
        pd.set_option('display.float_format', lambda x: '%.3f' % x)
        
    def load_and_preprocess_data(self, file_path):
        """Load and preprocess the data"""
        try:
            df = pd.read_csv(file_path)
            
            # Convert date columns
            df['event_date'] = pd.to_datetime(df['event_date'])
            df['timestamp'] = pd.to_datetime(df['timestamp'], unit='s')
            
            # Convert coordinates to numeric
            df['latitude'] = pd.to_numeric(df['latitude'])
            df['longitude'] = pd.to_numeric(df['longitude'])
            
            return df
        except FileNotFoundError:
            error_msg = f"Error: Could not find the file '{file_path}'. Please check the file path."
            if IN_STREAMLIT:
                st.error(error_msg)
            else:
                print(error_msg)
            return None
        except Exception as e:
            error_msg = f"Error loading data: {str(e)}"
            if IN_STREAMLIT:
                st.error(error_msg)
            else:
                print(error_msg)
            return None
    
    def display_or_plot(self, fig, title=None):
        """Helper function to handle both Streamlit and normal Python contexts"""
        if IN_STREAMLIT:
            if title:
                st.subheader(title)
            st.plotly_chart(fig, use_container_width=True)
        else:
            fig.show()
    
    def print_or_write(self, text, title=None):
        """Helper function to handle both Streamlit and normal Python contexts"""
        if IN_STREAMLIT:
            if title:
                st.subheader(title)
            st.write(text)
        else:
            if title:
                print(f"\n{title}")
                print("-" * len(title))
            print(text)
    
    def temporal_analysis(self, df):
        """Temporal analysis of events"""
        if df is None:
            return
            
        # Create time-based features
        df['year_month'] = df['event_date'].dt.to_period('M')
        df['day_of_week'] = df['event_date'].dt.day_name()
        
        # Events by year
        yearly_events = df['year'].value_counts().sort_index()
        self.print_or_write(yearly_events, "Events by Year")
        
        # Events by month
        monthly_fig = go.Figure()
        monthly_events = df['event_date'].dt.month.value_counts().sort_index()
        monthly_fig.add_trace(go.Bar(x=monthly_events.index, y=monthly_events.values))
        monthly_fig.update_layout(title="Events by Month",
                                xaxis_title="Month",
                                yaxis_title="Number of Events")
        self.display_or_plot(monthly_fig)
        
        # Events by day of week
        daily_fig = go.Figure()
        daily_events = df['day_of_week'].value_counts()
        daily_fig.add_trace(go.Bar(x=daily_events.index, y=daily_events.values))
        daily_fig.update_layout(title="Events by Day of Week",
                              xaxis_title="Day",
                              yaxis_title="Number of Events")
        self.display_or_plot(daily_fig)
    
    def spatial_analysis(self, df):
        """Spatial analysis of events"""
        if df is None:
            return
            
        # Events by region
        region_counts = df['region'].value_counts()
        self.print_or_write(region_counts, "Events by Region")
        
        # Map visualization
        map_fig = px.scatter_mapbox(df,
                                  lat='latitude',
                                  lon='longitude',
                                  color='event_type',
                                  hover_name='location',
                                  zoom=6,
                                  title='Geographic Distribution of Events')
        
        map_fig.update_layout(mapbox_style="carto-positron",
                            margin={"r":0,"t":30,"l":0,"b":0})
        self.display_or_plot(map_fig)
    
    def run_analysis(self, file_path):
        """Run the complete analysis"""
        # Load data
        df = self.load_and_preprocess_data(file_path)
        
        if df is None:
            return
            
        if IN_STREAMLIT:
            st.title("Sri Lanka Events Analysis")
            st.markdown("---")
        
        # Run analyses
        self.temporal_analysis(df)
        self.spatial_analysis(df)
        # Add other analyses as needed

def main():
    analyzer = EventsAnalyzer()
    analyzer.run_analysis('paste.txt')

if __name__ == "__main__":
    main()