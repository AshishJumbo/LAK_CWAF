import pandas as pd

df_v2_new = pd.read_csv("../data/processed0.2/cwaf_v2_new.csv")

df_v2_new = df_v2_new.loc[df_v2_new.psa_id == "PSABTZFT"]

print(df_v2_new.control_treatments.value_counts())

df_v2_new['user_psa_combo'] = df_v2_new.user_id.astype(str) + "_" + df_v2_new.psa_id.astype(str)
corrupted_user_psa_combos = df_v2_new.loc[df_v2_new.control_treatments == "both_treatments"].user_psa_combo.unique()
df_v2_new = df_v2_new.loc[~df_v2_new.user_psa_combo.isin(corrupted_user_psa_combos)]

test = df_v2_new.groupby(['psa_id', 'user_id', 'problem_log_id', 'pra_id', 'continuous_score', 'control_treatments',
                          'mastery', 'skb_mastery_count', 'wheel_spinning', 'skb_problem_count']
                         ).size().reset_index(name='frequency')

test.sort_values(by=['psa_id', 'user_id', 'problem_log_id'], inplace=True)
