########################################################################################################################
###################################### Load libraries and read in data #################################################

import pandas as pd
pd.set_option('display.max_rows', None)
snr_cutoff = 5
amp_cutoff = 0.001

full_df = pd.read_csv("Project_Data.csv",header=0)
# print(full_df)

########################################################################################################################
############################################### Exclude rows ###########################################################

### Stim1 and SNR < cutoff or
### Stim1 and amp < cutoff or
### Layers = edge or
### Stim1 and visual == 0

remove_index_list = []
for i in range(len(full_df)):
# for i in range(0,2100):
#     if 'Stim1' in full_df.iloc[i].loc["Pulse_index"] and full_df.iloc[i].loc["SNR"] < snr_cutoff:
    if full_df.iloc[i].loc["SNR"] < snr_cutoff:
            remove_index_list.append(i)
    # elif 'Stim1' in full_df.iloc[i].loc["Pulse_index"] and full_df.iloc[i].loc["Amp"] < amp_cutoff:
    elif full_df.iloc[i].loc["Amp"] < amp_cutoff:
        remove_index_list.append(i)
    elif 'Edge' == full_df.iloc[i].loc["Layers"]:
        remove_index_list.append(i)
    # elif 'Stim1' in full_df.iloc[i].loc["Pulse_index"] and full_df.iloc[i].loc["Visual"] == 0:
    elif full_df.iloc[i].loc["Visual"] == 0:
        remove_index_list.append(i)
# print(len(remove_index_list))
trimmed_df = full_df.drop(remove_index_list)
# print(trimmed_df)
trimmed_df.to_csv('Trimmed_Data.csv',index=False)



# ########################################################################################################################
# ################################################  Fix Latencies ########################################################
#
# ### Stimulation time shown in channel 2 is true stimulation time
# ### Records taken on:
# ###     03-24-2020 and earlier: from neuroplex (start stim at frame 84)
# np_days =  ["04-19-2019","04-22-2019","04-30-2019","05-02-2019","05-03-2019",
#             "05-06-2019","05-07-2019","05-09-2019","05-14-2019","05-16-2019",
#             "05-27-2019","06-06-2019","06-10-2019","06-11-2019","06-12-2019",
#             "06-13-2019","06-20-2019","06-24-2019","06-25-2019","06-26-2019",
#             "06-27-2019","07-01-2019","07-02-2019","07-03-2019","07-08-2019",
#             "07-10-2019","07-16-2019","07-22-2019","08-12-2019","08-26-2019",
#             "08-27-2019","09-04-2019","09-09-2019","09-11-2019","09-13-2019",
#             "09-16-2019","09-17-2019","09-18-2019","09-19-2019","09-20-2019",
#             "09-23-2019","09-25-2019","10-07-2019","10-08-2019","10-09-2019",
#             "10-24-2019","11-14-2019","11-15-2019","11-18-2019","11-20-2019",
#             "11-21-2019","12-02-2019","12-04-2019","12-05-2019","12-11-2019",
#             "12-12-2019","12-16-2019","01-06-2020","01-07-2020","01-10-2020",
#             "01-16-2020","02-03-2020","02-04-2020","02-05-2020","02-06-2020",
#             "02-13-2020","02-19-2020","02-20-2020","02-24-2020","02-25-2020",
#             "02-26-2020","02-27-2020","03-23-2020","03-24-2020"]
# trimmed_df.loc[trimmed_df.Date.isin(np_days),"Latency"] -= 42
# trimmed_df.loc[trimmed_df.Date.isin(np_days),"PeakTime"] -= 42
# ###     05-28-2020 through and including 10-29-2020: from photoZ with bug (start stim at frame 96)
# pz_bug_days = ["05-28-2020","05-29-2020","06-01-2020","06-02-2020","07-02-2020",
#                 "07-03-2020","07-09-2020","07-10-2020","07-11-2020","07-12-2020",
#                 "07-13-2020","07-14-2020","07-15-2020","07-16-2020","07-17-2020",
#                 "07-18-2020","07-19-2020","08-03-2020","08-04-2020","08-05-2020",
#                 "08-17-2020","09-09-2020","09-10-2020","09-11-2020","09-14-2020",
#                 "09-15-2020","09-16-2020","09-17-2020","10-01-2020","10-02-2020",
#                 "10-05-2020","10-26-2020","10-27-2020","10-28-2020","10-29-2020"]
# trimmed_df.loc[trimmed_df.Date.isin(pz_bug_days),"Latency"] -= 48
# trimmed_df.loc[trimmed_df.Date.isin(pz_bug_days),"PeakTime"] -= 48
#
# ###     12-13-2020 through and including 12-17-2020: from photoZ with bug (start stim at frame 95)
# pz_bug_days2 = ["12-13-2020","12-14-2020","12-15-2020","12-17-2020"]
# trimmed_df.loc[trimmed_df.Date.isin(pz_bug_days2),"Latency"] -= 47.5
# trimmed_df.loc[trimmed_df.Date.isin(pz_bug_days2),"PeakTime"] -= 47.5
#
# ###     12-28-2020 and later: from photoZ without bug (stim at frame 94)
# pz_no_bug_days = ["12-28-2020","12-29-2020","01-11-2021","01-18-2021","01-19-2021","03-02-2021",
#                   "03-09-2021","03-23-2021","04-13-2021","05-25-2021","05-26-2021",
#                   "05-31-2021","06-28-2021","06-29-2021","07-29-2021","08-06-2021",
#                   "08-09-2021","08-10-2021","08-31-2021","09-06-2021","09-08-2021",
#                   "10-04-2021","10-05-2021","11-15-2021","11-16-2021","11-22-2021",
#                   "11-23-2021","11-29-2021","11-30-2021","12-06-2021","12-07-2021",
#                   "12-08-2021","12-13-2021","12-20-2021","01-24-2022","01-25-2022",
#                   "01-31-2022","02-01-2022","02-02-2022","02-07-2022","02-08-2022",
#                   "02-21-2022","02-23-2022","02-24-2022"]
# # pz_no_bug_days = trimmed_df[~trimmed_df.Date.isin(pz_bug_days)]["Date"].unique()
# # print("pz_no_bug_days is ",pz_no_bug_days)
# trimmed_df.loc[trimmed_df.Date.isin(pz_no_bug_days),"Latency"] -= 47
# trimmed_df.loc[trimmed_df.Date.isin(pz_no_bug_days),"PeakTime"] -= 47

########################################################################################################################
#######################################  Add intra/interlaminar column #################################################

trimmed_df.insert(17,'Laminar',value='')
# print(trimmed_df)
laminar_col_index = trimmed_df.columns.get_loc('Laminar')
# for i in range(12):

# trimmed_df["Stim_Layer"] = trimmed_df["Stim_Layer"].astype(str)

# for i in range(12):
for i in range(len(trimmed_df)):
    stim_layer = trimmed_df.iloc[i].loc['Stim_Layer']
    roi_layer = trimmed_df.iloc[i].loc['Layers']
    if stim_layer == roi_layer:
        # print(stim_layer, roi_layer, 'equal')
        trimmed_df.iloc[i,laminar_col_index] = 'Intra'
    else:
        # print(stim_layer, roi_layer, 'not equal')
        trimmed_df.iloc[i,laminar_col_index] = 'Inter'

########################################################################################################################
##############################################  Create Stim1 table #####################################################

# stim1_df = trimmed_df[trimmed_df['Pulse_index'].str.match('Stim1')]
# stim1_df = trimmed_df
# stim1_df = stim1_df.copy()
# stim1_df.insert(8,'Slice_Loc',value = stim1_df['Slice_Loc_Run'].str[:5])
# print(stim1_df['Slice_Loc_Run'])
# stim1_df[stim1_df.columns[0:19]] = stim1_df[stim1_df.columns[0:19]].astype(str)
# print(stim1_df)
# stim1_df.insert(9,'Full_ROI_Id',value = stim1_df['Date']+'__'+stim1_df['Slice_Loc']+'__'+stim1_df['ROI_Id'].astype(str))
# stim1_df = stim1_df.groupby(list(stim1_df['Full_ROI_Id']),sort=False).agg(
#     {'Date': lambda x: x.iloc[0], 'Id': lambda x: x.iloc[0],'Genotype': lambda x: x.iloc[0],
#      'Birthdate': lambda x: x.iloc[0],'Sex': lambda x: x.iloc[0],'Tx': lambda x: x.iloc[0],
#      'Tx_Start': lambda x: x.iloc[0],'Slice_Loc_Run': lambda x: x.iloc[0],'Slice_Loc': lambda x: x.iloc[0],
#      'Full_ROI_Id': lambda x: x.iloc[0],'Trial_x_Time': lambda x: x.iloc[0],'Stim_Intensity': lambda x: x.iloc[0],
#      'Stim_Layer': lambda x: x.iloc[0],'RLI': lambda x: x.iloc[0],'Cx': lambda x: x.iloc[0],
#      'n_Pulses': lambda x: x.iloc[0],'Pulse_index': lambda x: x.iloc[0],'IPI': lambda x: x.iloc[0],
#      'ROI_Id': lambda x: x.iloc[0],'Laminar': lambda x: x.iloc[0],
#      'Visual': lambda x: x.iloc[0],
#      'Layers': lambda x: x.iloc[0],
#      'Amp':'mean','PeakTime':'mean','SNR':'mean','Latency':'mean','Halfwidth':'mean',
#      'Rise':'mean','Decay':'mean',
#      # 'X_dist':'mean','Y_dist':'mean',
#      'Euc_dist':'mean',
#      # 'X_shift_dist':'mean','Y_shift_dist':'mean',
#      # 'Euc_shift_dist':'mean',
#      'nPixel':'mean'})
# # print(stim1_df)
#
# print(stim1_df.head())
# stim1_df = stim1_df.reset_index()
# print(stim1_df.head())
# todelete_indexes = []

# for date in stim1_df.Date.unique():
#     # print(date)
#     date_indexes = stim1_df[stim1_df['Date']==date].index
#     if len(date_indexes)>75:
#         min_snr_val_to_keep = stim1_df.SNR.iloc[date_indexes].sort_values(ascending=False)[0:75].min()
#         for index in date_indexes:
#             # print(index)
#             if stim1_df.SNR.iloc[index] < min_snr_val_to_keep:
#                 todelete_indexes.append(index)

# print(stim1_df.shape)

# stim1_df = stim1_df.drop(todelete_indexes)
# stim1_df = stim1_df.drop(columns='index')
#
# print(stim1_df.head())
#
# # print(stim1_df.shape)
#
# # stim1_df = stim1_df.reset_index()
# # print(stim1_df)
# stim1_df.to_csv('Stim1_Data.csv',index=False)

#
#
#
# ########################################################################################################################
# #########################################  Create Stim2 and PP tables ##################################################
#
# stim2_df = trimmed_df.copy()
# stim2_df.insert(8,'Slice_Loc',value = stim2_df['Slice_Loc_Run'].str[:5])
# stim2_df.insert(9,'Full_ROI_Id',value = stim2_df['Date']+'__'+stim2_df['Slice_Loc_Run']+'__'+stim2_df['ROI_Id'].astype(str))
# # print(stim2_df)
#
# stim2_dup = stim2_df[stim2_df.duplicated(subset='Full_ROI_Id')] #identifies everything that is a duplicate but still no original
# # stim2_dup.to_csv('stim2dup.csv',index=Falyse)
# # print(stim2_dup)
#
# dup_index = []
# full_roi_id_col_index = stim2_df.columns.get_loc('Full_ROI_Id')
# for i in range(len(stim2_dup)):
#     # print('test')
#     # print(stim2_dup.iloc[i,9])
#     for j in range(len(stim2_df)):
#         if stim2_dup.iloc[i,full_roi_id_col_index] == stim2_df.iloc[j,full_roi_id_col_index]:
#             # print(stim2_dup.iloc[i,9])
#             # print(stim2_df.iloc[j,9])
#             dup_index.append(j)
# # print(dup_index)
# stim2_df = stim2_df.iloc[dup_index]
# # print(stim2_orig)
# # print(test)
# stim2_df.to_csv('Stim2_Data.csv',index=False)
#
#
# PP_df = stim2_df.copy()
# PP_df = PP_df[PP_df.duplicated(subset='Full_ROI_Id',keep='first')]
# PP_df['Pulse_index'] = 'PP'
# PP_df['PPR'] = ''
# # print(PP_df)
# row_index = []
# for i in range(len(stim2_df)):
#     for j in range(len(stim2_df)):
#         if stim2_df.iloc[i,full_roi_id_col_index] == stim2_df.iloc[j,full_roi_id_col_index] and j>i:
#         #so no [[0,0]] and no duplication of pairs
#             # print(stim2_df.iloc[i,9],stim2_df.iloc[j,9])
#             row_index.append([i,j])
# # print(row_index)
# amp_col_index = stim2_df.columns.get_loc('Amp')
# ppr_col_index = PP_df.columns.get_loc('PPR')
# for i in range(len(row_index)):
#     current_amp = stim2_df.iloc[row_index[i][1],amp_col_index]/stim2_df.iloc[row_index[i][0],amp_col_index]
#     PP_df.iloc[i,ppr_col_index] = current_amp
#     # print(current_amp)
# PP_df.to_csv('PP_Data.csv',index=False)
#
