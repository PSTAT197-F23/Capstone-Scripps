# -*- coding: utf-8 -*-
"""
Created on Mon Jan 22 11:35:46 2024

@author: Michaela Alksne 

Script to run when modifying Triton logger annotation excel datasheets
converts xls to csv containing the audio file path, the annotation label, the frequency bounds, and time bounds. 
saves new csv in "modified annotations subfolder"
wav is the audio file
start time = start time of call in number of seconds since start of wav 
end time = end time of call in number of seconds since start of wav

"""

from datetime import datetime
import os
import glob
import opensoundscape
import sys
from opensoundscape import Audio, Spectrogram
sys.path.append(r"C:\Users\DAM1\CV4E")
from AudioStreamDescriptor import XWAVhdr
from AudioStreamDescriptor import WAVhdr
import random
import pandas as pd
import numpy as np

directory_path = "C:/Users/Lenovo/Documents/PSTAT 197a/labs/Capstone-Scripps" # point to original logger files
all_files = glob.glob(os.path.join(directory_path,'*.xls')) # path for all files

new_base_path = 'C:/Users/Lenovo/Documents/PSTAT 197a/labs/Capstone-Scripps/master_wav_sonobuoy' # path to change to 

# hepler function uses WAVhdr to read wav file header info and extract wav file start time as a datetime object
def extract_wav_start(path):
    wav_hdr = WAVhdr(path)
    wav_start_time = wav_hdr.start
    return wav_start_time

# helper function to modify the original logger files
def modify_annotations(df):
    # Ensure working on a copy to avoid SettingWithCopyWarning
    df = df.copy()
    
    # Creating the audio column with new base path
    modified_paths = []
    for in_file in df['Input file']:
        separator_index = in_file.rfind("\\")
        directory = in_file[:separator_index] if separator_index != -1 else ''
        new_path = in_file.replace(directory, new_base_path)
        modified_paths.append(new_path)
    df['audio_file'] = [path.replace("\\", "/") for path in modified_paths]
    
    # Removing a defective wave file
    df = df[df['audio_file'] != "C:/Users/Lenovo/Documents/PSTAT 197a/labs/Capstone-Scripps/master_wav_sonobuoy/CC0711-SB02-071103-214000.d24.wav"].copy()
    
    # Creating the date_time column
    file_datetimes = []
    for audio_file in df['audio_file']:
        datetime_value = extract_wav_start(audio_file)
        file_datetimes.append(datetime_value)
    df['file_datetime'] = file_datetimes
    
    # Calculating start and end times
    df['start_time'] = (df['Start time'] - df['file_datetime']).dt.total_seconds()
    df['end_time'] = (df['End time'] - df['file_datetime']).dt.total_seconds()
    
    # Assigning other columns
    df['annotation'] = df['Call']
    df['high_f'] = df['Parameter 1']
    df['low_f'] = df['Parameter 2']
    
    # Subset the DataFrame to keep specified columns
    df = df.loc[:, ['audio_file', 'annotation', 'high_f', 'low_f', 'start_time', 'end_time']]
    
    return df

    
# make a subfolder for saving modified logs 
subfolder_name = "modified_annotations"
# Create the subfolder if it doesn't exist
subfolder_path = os.path.join(directory_path, subfolder_name)
os.makedirs(subfolder_path, exist_ok=True)

# loop through all annotation files and save them in subfolder "modified_annotations"

for file in all_files:
    data = pd.read_excel(file)
    subset_df = modify_annotations(data)
    filename = os.path.basename(file)
    new_filename = filename.replace('.xls', '_modification.csv')
     # Construct the path to save the modified DataFrame as a CSV file
    save_path = os.path.join(subfolder_path, new_filename)
    # Save the subset DataFrame to the subset folder as a CSV file
    subset_df.to_csv(save_path, index=False)


