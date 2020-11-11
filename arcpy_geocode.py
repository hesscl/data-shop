#This script takes the CSV of new listings from an online rental listing source and
#uses ESRI Business Analyst address locators to geocode listings to a point, interpolated
#street segment or approximate area. 

#dependencies
import os
import csv
import sys
import re
import arcinfo
import arcpy

#allow overwriting previous shapefiles
arcpy.env.overwriteOutput = True

#handle the mandatory input arguments
input_filename = sys.argv[1] # we're going to grab the first command line argument, use that as our input file
output_shape = sys.argv[2] #use this to name what the script returns
add_field = sys.argv[3] #use these to specify what columns of input data table are used within GeocodeAddresses

#handle the components if provided
if len(sys.argv) > 4:
	city_field = sys.argv[4]
	state_field = sys.argv[5]
else:
	city_field = None
	state_field = None
	
#for runs where we want to use a single field
if (city_field is None and state_field is None):
	in_add_string = "'Single Line Input' " + add_field + " VISIBLE NONE"
	
#for normal runs
else:
	in_add_string = "Address " + add_field + " VISIBLE NONE;City " + city_field + " VISIBLE NONE;Region " + state_field + " VISIBLE NONE;Postal <None> VISIBLE NONE"

#run the geocoding based on the current best-working setup
arcpy.GeocodeAddresses_geocoding(in_table=input_filename, 
								 address_locator="R:/Data/GIS/Geocoding/2019 Business Analyst Data/Geocoding Data/USA", 
								 in_address_fields=in_add_string, 
								 out_feature_class=output_shape, 
								 out_relationship_type="STATIC",
								 country="", location_type="ROUTING_LOCATION"
                                 )
