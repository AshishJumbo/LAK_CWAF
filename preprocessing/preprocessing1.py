# Important:
#  analyzing the two CWAF designs that were run longitudionally from 08 to today

import pandas as pd


def merge_all_files_v1_v2():
    df_08_09_v1 = pd.read_csv("../data/raw/rds_dev_08_09_cwaf_vs_nofeedback_v1.csv")
    df_09_10_v1 = pd.read_csv("../data/raw/rds_dev_09_10_cwaf_vs_nofeedback_v1.csv")
    df_10_11_v1 = pd.read_csv("../data/raw/rds_dev_10_11_cwaf_vs_nofeedback_v1.csv")
    df_11_12_v1 = pd.read_csv("../data/raw/rds_dev_11_12_cwaf_vs_nofeedback_v1.csv")
    df_12_13_v1 = pd.read_csv("../data/raw/rds_dev_12_13_cwaf_vs_nofeedback_v1.csv")
    df_13_14_v1 = pd.read_csv("../data/raw/rds_dev_13_14_cwaf_vs_nofeedback_v1.csv")
    df_14_15_v1 = pd.read_csv("../data/raw/rds_dev_14_15_cwaf_vs_nofeedback_v1.csv")
    df_15_16_v1 = pd.read_csv("../data/raw/rds_dev_15_16_cwaf_vs_nofeedback_v1.csv")
    df_16_17_v1 = pd.read_csv("../data/raw/rds_dev_16_17_cwaf_vs_nofeedback_v1.csv")
    df_17_18_v1 = pd.read_csv("../data/raw/rds_dev_17_18_cwaf_vs_nofeedback_v1.csv")
    df_18_19_v1 = pd.read_csv("../data/raw/rds_dev_18_19_cwaf_vs_nofeedback_v1.csv")
    df_19_20_v1 = pd.read_csv("../data/raw/rds_dev_19_20_cwaf_vs_nofeedback_v1.csv")
    df_20_21_v1 = pd.read_csv("../data/raw/rds_dev_20_21_cwaf_vs_nofeedback_v1.csv")
    df_21_22_v1 = pd.read_csv("../data/raw/rds_dev_21_22_cwaf_vs_nofeedback_v1.csv")

    df_09_v1 = pd.read_csv("../data/raw/rds_dev_09_cwaf_vs_nofeedback_v1.csv")
    df_10_v1 = pd.read_csv("../data/raw/rds_dev_10_cwaf_vs_nofeedback_v1.csv")
    df_11_v1 = pd.read_csv("../data/raw/rds_dev_11_cwaf_vs_nofeedback_v1.csv")
    df_12_v1 = pd.read_csv("../data/raw/rds_dev_12_cwaf_vs_nofeedback_v1.csv")
    df_13_v1 = pd.read_csv("../data/raw/rds_dev_13_cwaf_vs_nofeedback_v1.csv")
    df_14_v1 = pd.read_csv("../data/raw/rds_dev_14_cwaf_vs_nofeedback_v1.csv")
    df_15_v1 = pd.read_csv("../data/raw/rds_dev_15_cwaf_vs_nofeedback_v1.csv")
    df_16_v1 = pd.read_csv("../data/raw/rds_dev_16_cwaf_vs_nofeedback_v1.csv")
    df_17_v1 = pd.read_csv("../data/raw/rds_dev_17_cwaf_vs_nofeedback_v1.csv")
    df_18_v1 = pd.read_csv("../data/raw/rds_dev_18_cwaf_vs_nofeedback_v1.csv")
    df_19_v1 = pd.read_csv("../data/raw/rds_dev_19_cwaf_vs_nofeedback_v1.csv")
    df_20_v1 = pd.read_csv("../data/raw/rds_dev_20_cwaf_vs_nofeedback_v1.csv")
    df_21_v1 = pd.read_csv("../data/raw/rds_dev_21_cwaf_vs_nofeedback_v1.csv")
    df_22_v1 = pd.read_csv("../data/raw/rds_dev_22_cwaf_vs_nofeedback_v1.csv")

    df_18_19_v2 = pd.read_csv("../data/raw/rds_dev_18_19_cwaf_vs_nofeedback_v2.csv")
    df_19_20_v2 = pd.read_csv("../data/raw/rds_dev_19_20_cwaf_vs_nofeedback_v2.csv")
    df_20_21_v2 = pd.read_csv("../data/raw/rds_dev_20_21_cwaf_vs_nofeedback_v2.csv")
    df_21_22_v2 = pd.read_csv("../data/raw/rds_dev_21_22_cwaf_vs_nofeedback_v2.csv")
    df_21_22_v2_new = pd.read_csv("../data/raw/rds_dev_PSABTZFT_ 21_22_cwaf_vs_nofeedback_v2.csv")
    df_19_v2 = pd.read_csv("../data/raw/rds_dev_19_cwaf_vs_nofeedback_v2.csv")
    df_20_v2 = pd.read_csv("../data/raw/rds_dev_20_cwaf_vs_nofeedback_v2.csv")
    df_21_v2 = pd.read_csv("../data/raw/rds_dev_21_cwaf_vs_nofeedback_v2.csv")
    df_22_v2 = pd.read_csv("../data/raw/rds_dev_22_cwaf_vs_nofeedback_v2.csv")
    df_22_v2_new = pd.read_csv("../data/raw/rds_dev_PSABTZFT_22_cwaf_vs_nofeedback_v2.csv")

    psa_info = pd.read_csv("../data/raw/rds_dev_PSA_cwaf_vs_nofeedback_v1_v2.csv")

    df_08_09_v1 = df_08_09_v1.merge(psa_info, how='left', on=['sequence_id', 'assistment_id', 'problem_id'])
    df_09_10_v1 = df_09_10_v1.merge(psa_info, how='left', on=['sequence_id', 'assistment_id', 'problem_id'])
    df_10_11_v1 = df_10_11_v1.merge(psa_info, how='left', on=['sequence_id', 'assistment_id', 'problem_id'])
    df_11_12_v1 = df_11_12_v1.merge(psa_info, how='left', on=['sequence_id', 'assistment_id', 'problem_id'])
    df_12_13_v1 = df_12_13_v1.merge(psa_info, how='left', on=['sequence_id', 'assistment_id', 'problem_id'])
    df_13_14_v1 = df_13_14_v1.merge(psa_info, how='left', on=['sequence_id', 'assistment_id', 'problem_id'])
    df_14_15_v1 = df_14_15_v1.merge(psa_info, how='left', on=['sequence_id', 'assistment_id', 'problem_id'])
    df_15_16_v1 = df_15_16_v1.merge(psa_info, how='left', on=['sequence_id', 'assistment_id', 'problem_id'])
    df_16_17_v1 = df_16_17_v1.merge(psa_info, how='left', on=['sequence_id', 'assistment_id', 'problem_id'])
    df_17_18_v1 = df_17_18_v1.merge(psa_info, how='left', on=['sequence_id', 'assistment_id', 'problem_id'])
    df_18_19_v1 = df_18_19_v1.merge(psa_info, how='left', on=['sequence_id', 'assistment_id', 'problem_id'])
    df_19_20_v1 = df_19_20_v1.merge(psa_info, how='left', on=['sequence_id', 'assistment_id', 'problem_id'])
    df_20_21_v1 = df_20_21_v1.merge(psa_info, how='left', on=['sequence_id', 'assistment_id', 'problem_id'])
    df_21_22_v1 = df_21_22_v1.merge(psa_info, how='left', on=['sequence_id', 'assistment_id', 'problem_id'])

    df_09_v1 = df_09_v1.merge(psa_info, how='left', on=['sequence_id', 'assistment_id', 'problem_id'])
    df_10_v1 = df_10_v1.merge(psa_info, how='left', on=['sequence_id', 'assistment_id', 'problem_id'])
    df_11_v1 = df_11_v1.merge(psa_info, how='left', on=['sequence_id', 'assistment_id', 'problem_id'])
    df_12_v1 = df_12_v1.merge(psa_info, how='left', on=['sequence_id', 'assistment_id', 'problem_id'])
    df_13_v1 = df_13_v1.merge(psa_info, how='left', on=['sequence_id', 'assistment_id', 'problem_id'])
    df_14_v1 = df_14_v1.merge(psa_info, how='left', on=['sequence_id', 'assistment_id', 'problem_id'])
    df_15_v1 = df_15_v1.merge(psa_info, how='left', on=['sequence_id', 'assistment_id', 'problem_id'])
    df_16_v1 = df_16_v1.merge(psa_info, how='left', on=['sequence_id', 'assistment_id', 'problem_id'])
    df_17_v1 = df_17_v1.merge(psa_info, how='left', on=['sequence_id', 'assistment_id', 'problem_id'])
    df_18_v1 = df_18_v1.merge(psa_info, how='left', on=['sequence_id', 'assistment_id', 'problem_id'])
    df_19_v1 = df_19_v1.merge(psa_info, how='left', on=['sequence_id', 'assistment_id', 'problem_id'])
    df_20_v1 = df_20_v1.merge(psa_info, how='left', on=['sequence_id', 'assistment_id', 'problem_id'])
    df_21_v1 = df_21_v1.merge(psa_info, how='left', on=['sequence_id', 'assistment_id', 'problem_id'])
    df_22_v1 = df_22_v1.merge(psa_info, how='left', on=['sequence_id', 'assistment_id', 'problem_id'])

    df_18_19_v2 = df_18_19_v2.merge(psa_info, how='left', on=['sequence_id', 'problem_id'])
    df_19_20_v2 = df_19_20_v2.merge(psa_info, how='left', on=['sequence_id', 'problem_id'])
    df_20_21_v2 = df_20_21_v2.merge(psa_info, how='left', on=['sequence_id', 'problem_id'])
    df_21_22_v2 = df_21_22_v2.merge(psa_info, how='left', on=['sequence_id', 'problem_id'])
    df_21_22_v2_new = df_21_22_v2_new.merge(psa_info, how='left', on=['sequence_id', 'problem_id'])
    df_19_v2 = df_19_v2.merge(psa_info, how='left', on=['sequence_id', 'problem_id'])
    df_20_v2 = df_20_v2.merge(psa_info, how='left', on=['sequence_id', 'problem_id'])
    df_21_v2 = df_21_v2.merge(psa_info, how='left', on=['sequence_id', 'problem_id'])
    df_22_v2 = df_22_v2.merge(psa_info, how='left', on=['sequence_id', 'problem_id'])
    df_22_v2_new = df_22_v2_new.merge(psa_info, how='left', on=['sequence_id', 'problem_id'])

    df_08_09_v1['academic_year'] = 'v1_08_09'
    df_09_10_v1['academic_year'] = 'v1_09_10'
    df_10_11_v1['academic_year'] = 'v1_10_11'
    df_11_12_v1['academic_year'] = 'v1_11_12'
    df_12_13_v1['academic_year'] = 'v1_12_13'
    df_13_14_v1['academic_year'] = 'v1_13_14'
    df_14_15_v1['academic_year'] = 'v1_14_15'
    df_15_16_v1['academic_year'] = 'v1_15_16'
    df_16_17_v1['academic_year'] = 'v1_16_17'
    df_17_18_v1['academic_year'] = 'v1_17_18'
    df_18_19_v1['academic_year'] = 'v1_18_19'
    df_19_20_v1['academic_year'] = 'v1_19_20'
    df_20_21_v1['academic_year'] = 'v1_20_21'
    df_21_22_v1['academic_year'] = 'v1_21_22'
    df_09_v1['academic_year'] = 'v1_09_summer'
    df_10_v1['academic_year'] = 'v1_10_summer'
    df_11_v1['academic_year'] = 'v1_11_summer'
    df_12_v1['academic_year'] = 'v1_12_summer'
    df_13_v1['academic_year'] = 'v1_13_summer'
    df_14_v1['academic_year'] = 'v1_14_summer'
    df_15_v1['academic_year'] = 'v1_15_summer'
    df_16_v1['academic_year'] = 'v1_16_summer'
    df_17_v1['academic_year'] = 'v1_17_summer'
    df_18_v1['academic_year'] = 'v1_18_summer'
    df_19_v1['academic_year'] = 'v1_19_summer'
    df_20_v1['academic_year'] = 'v1_20_summer'
    df_21_v1['academic_year'] = 'v1_21_summer'
    df_22_v1['academic_year'] = 'v1_22_summer'

    df_18_19_v2['academic_year'] = 'v2_18_19'
    df_19_20_v2['academic_year'] = 'v2_19_20'
    df_20_21_v2['academic_year'] = 'v2_20_21'
    df_21_22_v2['academic_year'] = 'v2_21_22'
    df_21_22_v2_new['academic_year'] = 'v2_21_22'
    df_19_v2['academic_year'] = 'v2_19_summer'
    df_20_v2['academic_year'] = 'v2_20_summer'
    df_21_v2['academic_year'] = 'v2_21_summer'
    df_22_v2['academic_year'] = 'v2_22_summer'
    df_22_v2_new['academic_year'] = 'v2_22_summer'

    df_v1_ = pd.concat([
        df_08_09_v1, df_09_10_v1, df_10_11_v1, df_11_12_v1, df_12_13_v1, df_13_14_v1, df_14_15_v1, df_15_16_v1, df_16_17_v1,
        df_17_18_v1, df_18_19_v1, df_19_20_v1, df_20_21_v1, df_21_22_v1, df_09_v1, df_10_v1, df_11_v1, df_12_v1, df_13_v1,
        df_14_v1, df_15_v1, df_16_v1, df_17_v1, df_18_v1, df_19_v1, df_20_v1, df_21_v1, df_22_v1
    ], ignore_index=True)

    df_v2_ = pd.concat([
        df_18_19_v2, df_19_20_v2, df_20_21_v2, df_21_22_v2, df_21_22_v2_new, df_19_v2, df_20_v2, df_21_v2,
        df_22_v2, df_22_v2_new
    ], ignore_index=True)

    return df_v1_, df_v2_


def remove_assignmetns(df_temp):
    df_temp['user_assignment_combo'] = df_temp['assignment_id'].astype(str) + '_' + df_temp['user_id'].astype(str)
    corrupted_user_assignment_combos = df_temp.loc[
        df_temp.assistment_id.isin([493175, 493598])].user_assignment_combo.unique()
    df_temp = df_temp.loc[~(df_temp.user_assignment_combo.isin(corrupted_user_assignment_combos))]
    corrupted_nan_user_assignment_combos = df_temp.loc[df_temp.psa_id.isna()].user_assignment_combo.unique()
    df_temp = df_temp.loc[~(df_temp.user_assignment_combo.isin(corrupted_nan_user_assignment_combos))]
    return df_temp


def print_control_treatment_breakdown(df_v1_, df_v2_):
    v1_counts = df_v1_.groupby(['academic_year', 'psa_id', 'user_id', 'section_names']).size().reset_index(
        name='frequency')
    v2_counts = df_v2_.groupby(['academic_year', 'psa_id', 'user_id', 'section_names']).size().reset_index(
        name='frequency')

    print("======================================")
    print("psa > condition > user counts")
    print(v1_counts.groupby(['psa_id', 'section_names']).size().reset_index(name='frequency'))
    print(v2_counts.groupby(['psa_id', 'section_names']).size().reset_index(name='frequency'))
    print("======================================")
    v1_v2_counts = pd.concat([v1_counts, v2_counts], ignore_index=True)
    # print(v1_v2_counts.groupby(['academic_year', 'psa_id', 'section_names']).size().reset_index(name='frequency'))
    print(v1_v2_counts.groupby(['psa_id', 'section_names']).size().reset_index(name='frequency'))
    print("======================================")


df_v1, df_v2 = merge_all_files_v1_v2()
df_v2.rename({'user_xid': 'user_id'}, axis=1, inplace=True)
print_control_treatment_breakdown(df_v1, df_v2)
df_v1 = remove_assignmetns(df_v1)
df_v2 = remove_assignmetns(df_v2)
print_control_treatment_breakdown(df_v1, df_v2)

df_v1.to_csv('../data/processed0.1/df_v1.csv', index=False)
df_v2.to_csv('../data/processed0.1/df_v2.csv', index=False)


