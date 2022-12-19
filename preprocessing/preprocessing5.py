import pandas as pd

# cwa_grouping = pd.read_csv('../data/processed0.2/cwa_grouping_0.csv')
cwa_grouping = pd.read_csv('../data/processed0.2/cwa_grouping_5.csv')
# cwa_grouping = pd.read_csv('../data/processed0.2/cwa_grouping_10.csv')

print("======================================================================================")
print("PROBLEM COUNT::")
print(cwa_grouping.groupby(['psa_id', 'is_correct']).size().reset_index(name='frequency'))
print("======================================================================================")
print(cwa_grouping.loc[cwa_grouping.is_correct == '-1'].problem_id.unique())
print("======================================================================================")
print(cwa_grouping.loc[((cwa_grouping.is_correct == '-1') & (cwa_grouping.psa_id == 'PSAGF4'))].problem_id.unique().size)
print("======================================================================================")
print("CWA COUNT::")
print(cwa_grouping.groupby(['psa_id', 'is_correct'])['CWA_count'].sum().reset_index(name='sum'))
print("======================================================================================")
print("CWA above threshold COUNT::")
print(cwa_grouping.groupby(['psa_id', 'is_correct'])['CWA_total_frequency'].sum().reset_index(name='sum'))
print("======================================================================================")
print("======================================================================================")
print("TOTAL FIrst Incorrect Attempt::")
core_cwa_grouping = pd.read_csv("../data/processed0.2/core_cwa_grouping.csv")
# final_df_v1_v2_clean2_CWA_count__ = core_cwa_grouping.loc[
#     ((core_cwa_grouping.CWA_frequency >= 10) &
#      (core_cwa_grouping.section_names == '{"S2 [treatment2]"}'))]
# final_df_v1_v2_clean2_CWA_count__.reset_index(drop=True, inplace=True)
#
# print("================================================================")
# final_df_v1_v2_clean2_CWA_count__.is_correct.fillna(-1, inplace=True)
# final_df_grouping = final_df_v1_v2_clean2_CWA_count__.groupby(
#     ['psa_id', 'section_names', 'problem_id', 'is_correct']).agg({'CWA_frequency': ['sum', 'count']}).reset_index()
core_cwa_grouping = core_cwa_grouping.loc[core_cwa_grouping.section_names == '{"S2 [treatment2]"}']
core_cwa_grouping.fillna(-1, inplace=True)
count_cwa_per_psa = core_cwa_grouping.groupby(['psa_id', 'is_correct'])['CWA_frequency'].sum().reset_index(name='sum')
print(count_cwa_per_psa)
print("======================================================================================")

df_designed_cwas = pd.read_csv('../data/raw/rds_dev_feedback_bodies_cwaf_vs_nofeedback_v1_v2.csv')
print(df_designed_cwas.psa_id.unique())
df_designed_cwas = df_designed_cwas.loc[df_designed_cwas.psa_id.isin(['PSAGF4', 'PSAHQV'])]
df_designed_cwas.drop_duplicates(subset=['psa_id', 'problem_id', 'answer'], keep='first', inplace=True)
df_designed_cwas = df_designed_cwas.loc[df_designed_cwas.name == 'S2 [treatment2]']
df_designed_cwas.reset_index(drop=True, inplace=True)
df_designed_cwas = df_designed_cwas.loc[df_designed_cwas.is_correct == False]
df_designed_cwas.reset_index(drop=True, inplace=True)




