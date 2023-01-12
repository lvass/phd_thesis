# -*- coding: utf-8 -*-
"""
Created on Mon Apr 20 16:08:15 2020

@author: lv13916
"""

import pandas as pd
import numpy as np
import sys
import csv
import os
from fuzzywuzzy import fuzz
from io import StringIO
import matplotlib.pyplot as plt


path = r"C:\Users\lv13916\University of Bristol\AMR Database - Documents\Lucy\synergy\lucy_data"
path = path.replace("\\", "/")

sfh_df = pd.read_csv(path + os.sep + "synergy_abs_descriptions.csv")

# method to prevent unicoding problem
whole_file = list()
with open(path + os.sep + "abs_medicines_database.csv") as csv_file:
    csv_reader = csv.reader(csv_file, delimiter=',')
    line_count = 0
    for row in csv_reader:
        if line_count > 276:
            break
        else:
            whole_file.append(row)
            line_count +=1

db_df = pd.DataFrame.from_records(whole_file[1:], columns = whole_file[0])

med = sfh_df['sale_item_description'][29]

sfh_name = sfh_name.split(" ")

db_med = db_df['Medicine'][0]
db_name = db_name.split(" ")

best_match_list =  []
best_match_score_list = []

# each synergy medicine
for med in sfh_df['sale_item_description']:
    med_split = med.split(" ")
    print(med)
    final_match_list = list()
    final_score_list = list()
    # loop each database medicine 
    for db_med in db_df['Medicine']:
        #print(db_med)
        db_med_split = db_med.split(" ")
        count = 0
        n = len(med_split)
        status = "okay"
        if n == 1:
            score_weights = [1*n] 
        if n > 1:
            score_weights = [0.75*n, 0.25*n] 
            
        if n > 2:
            score_weights = [0.5*n, 0.25*n] + list(np.repeat(0.25/(n-2), n-2)*n)
        else:
            print("Error")
            status = "error"
        
        if status == "okay":
            max_score_token_str = ""
            max_score_num_total = 0
            
            
            # loop each token of synergy medicine
            for token in med_split:
                score_list = list()
                matched_list = list()
                
                count += 1
                #print("Token: " + token)
                #loop each token in db_med
                for db_token in db_med_split:
                    score = fuzz.ratio(token, db_token.lower()) * score_weights[count-1]
                    score_list.append(score)
                    matched_list.append(db_token)
                    #print(db_token.lower() + " :" + token + " =" + str(score))
                    #print(count)
                
                # put scores into a dataframe    
                df_scores = pd.DataFrame(score_list, index = matched_list, columns =['score'])
                #get maximum scored word for this token
                #print("Matched: " + df_scores.idxmax()[0])
                max_score_token = df_scores.idxmax()[0]
                max_score_num = df_scores.max()[0] 
                
                ser = pd.Series(db_med_split)
                ind = ser[ser == max_score_token].index[0]
                
                # remove as that token has been matched
                db_med_split[ind] = ''
                
                max_score_token_str = max_score_token_str + " " + max_score_token
                max_score_num_total = max_score_num_total + max_score_num
                
            final_score = max_score_num_total/(n*100)
            #print(db_med + ": " + str(final_score))
            
            final_match_list.append(db_med)
            final_score_list.append(final_score)
            
        else:
            final_score = 0            
            final_match_list.append('NA')
            final_score_list.append(final_score)
    
    #whole df of scores for that sfh medicine    
    df_final_matched = pd.DataFrame(list(zip(final_match_list, final_score_list)), columns =['db_match', 'score'])

    best_score = max(df_final_matched['score'])
    
    best_match = df_final_matched['db_match'][df_final_matched['score'].idxmax()]
    
    best_match_list.append(best_match)
    best_match_score_list.append(best_score)
    
df_final = pd.DataFrame(list(zip(sfh_df['sale_item_description'], best_match_list, best_match_score_list)), columns =['sfh_med', 'db_match', 'score'])
df_final.to_csv('C:/Users/lv13916/University of Bristol/AMR Database - Documents/Lucy/synergy/df_final.csv')

np.histogram(df_final['score'])
plt.show()

df_final.to_csv('C:/Users/lv13916/University of Bristol/AMR Database - Documents/Lucy/synergy/df_final.csv')
































