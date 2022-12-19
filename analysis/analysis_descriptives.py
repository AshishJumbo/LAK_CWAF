import pandas as pd

pd.options.display.float_format = '{:20,.4f}'.format

df = pd.read_csv('../data/processed0.2/cwaf_v1_v2_plogs_final_R.csv')

avg = df.groupby(['psa_id', 'control_treatments', 'pra_id', 'answer_text']).size().reset_index(name='frequency')
cwa_5 = avg.loc[avg.frequency >= 5]
cwa_10 = avg.loc[avg.frequency >= 10]

# cwa_ctrl_5_count = cwa_5.loc[
#     (cwa_5.control_treatments == 'control')
# ].groupby(['psa_id', 'control_treatments']).size().reset_index(name='frequency')
cwa_ctrl_5_count = cwa_5.groupby(['psa_id', 'control_treatments']).size().reset_index(name='frequency')
cwa_ctrl_10_count = cwa_10.groupby(['psa_id', 'control_treatments']).size().reset_index(name='frequency')

print(cwa_ctrl_5_count)
print(cwa_ctrl_10_count)

# NOTE:
#   N>=5 to be classified as CWA
#      psa_id control_treatments  frequency
#   0  PSAGF4            control        232
#   1  PSAGF4          treatment        150
#   2  PSAHQV            control        436
#   3  PSAHQV          treatment        438
#   N>=10 to be classified as CWA
#      psa_id control_treatments  frequency
#   0  PSAGF4            control        170
#   1  PSAGF4          treatment        104
#   2  PSAHQV            control        249
#   3  PSAHQV          treatment        221


average_pr_accuracy = df.groupby(['psa_id', 'control_treatments', 'pra_id'])['continuous_score'].mean().reset_index()
stdeaviation_pr_accuracy = df.groupby(['psa_id', 'control_treatments', 'pra_id'])['continuous_score'].std().reset_index()

print("problem difficulty")
avg_pr = average_pr_accuracy.groupby(['psa_id', 'control_treatments'])['continuous_score'].mean().reset_index()
print(avg_pr)
stdev_pr = stdeaviation_pr_accuracy.groupby(['psa_id', 'control_treatments'])['continuous_score'].std().reset_index()
print(stdev_pr)

df_filtered = pd.read_csv('../data/processed0.2/cwaf_v1_v2_plogs_final_filtered_R.csv')

print("mastery rates")
mastery = df_filtered.groupby(['psa_id', 'control_treatments', 'mastery']).size().reset_index(name='frequency')
print(mastery)
print(df_filtered.groupby(['psa_id', 'control_treatments'])['mastery'].mean().reset_index())

print("other descriptives")
average_descriptives = df.groupby(['psa_id', 'control_treatments'])['hint_count', 'continuous_score'].mean().reset_index()
print(average_descriptives)

stdeviation_descriptives = df.groupby(['psa_id', 'control_treatments'])['hint_count', 'continuous_score'].std().reset_index()
print(stdeviation_descriptives)


response_time_avg = df.groupby(['psa_id', 'control_treatments'])['first_response_time'].median().reset_index()
response_time_avg['first_response_time'] = response_time_avg['first_response_time']/(1000)
print(response_time_avg)

response_time_std = df.groupby(['psa_id', 'control_treatments'])['first_response_time'].std().reset_index()
response_time_std['first_response_time'] = response_time_std['first_response_time']/(1000)
print(response_time_std)
