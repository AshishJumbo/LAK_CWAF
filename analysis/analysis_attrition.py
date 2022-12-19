import pandas as pd
import statsmodels.formula.api as smf
import seaborn as sns
import matplotlib.pyplot as plt


def compute_prior_perform():
    df_prior_perform_v1 = pd.read_csv('../data/raw/rds_dev_prior_kng_dump_v1_CWAF_experiment.csv')
    df_prior_perform_v2 = pd.read_csv('../data/raw/rds_dev_prior_kng_dump_v2_CWAF_experiment.csv')
    df_prior_perform_v2.rename(columns={'user_xid': 'user_id'}, inplace=True)

    df_prior_perform_v1['user_id'] = df_prior_perform_v1.user_id.astype(str) + '_v1'
    df_prior_perform_v2['user_id'] = df_prior_perform_v2.user_id.astype(str) + '_v2'
    df_prior_perform_v1['problem_log_id_v1'] = df_prior_perform_v1.problem_log_id_v1.astype(str) + '_v1'
    df_prior_perform_v2['problem_log_id_v2'] = df_prior_perform_v2.problem_log_id_v2.astype(str) + '_v2'

    df_prior_perform_v1.rename(columns={'problem_log_id_v1': 'problem_log_id'}, inplace=True)
    df_prior_perform_v2.rename(columns={'problem_log_id_v2': 'problem_log_id'}, inplace=True)

    df_prior_perform_ = pd.concat([df_prior_perform_v1, df_prior_perform_v2], ignore_index=True)
    return df_prior_perform_


df_prior_perform = compute_prior_perform()
df_v1_v2 = pd.read_csv('../data/processed0.2/cwaf_v1_v2_plogs_final.csv')
df_v1_v2 = df_v1_v2.merge(df_prior_perform, on=['problem_log_id', 'user_id'], how='inner')

df_v1_v2 = df_v1_v2.loc[df_v1_v2.psa_id.isin(['PSAGF4', 'PSAHQV'])]
# df_v1_v2 = df_v1_v2.loc[:, ~df_v1_v2.columns.str.contains('^Unnamed')]

df_v1_v2['day_1_mastery'] = False
df_v1_v2.loc[((df_v1_v2.mastery == True) & (df_v1_v2.skb_problem_count <= 12)), 'day_1_mastery'] = True
df_experiment_outcomes = df_v1_v2.groupby(['psa_id', 'academic_year_merged', 'teacher_id', 'student_class_id',
                                           'control_treatments', 'user_id', 'mastery', 'day_1_mastery',
                                           'wheel_spinning', 'skb_problem_count']
                                          ).size().reset_index(name='frequency')

df_crossover_academic_sessions = df_experiment_outcomes.groupby(['user_id', 'academic_year_merged']).size().reset_index(
    name='frequency')
corss_over_user_ids = df_crossover_academic_sessions.user_id.value_counts().reset_index(name='frequency')
duplicate_user_id = corss_over_user_ids.loc[corss_over_user_ids.frequency > 1]['index'].unique()
print("# of studetns who corssed over multiple academic sessions :: " + str(duplicate_user_id.__len__()))
df_experiment_outcomes = df_experiment_outcomes.loc[~df_experiment_outcomes.user_id.isin(duplicate_user_id)]

day_1_mastery = df_experiment_outcomes.loc[df_experiment_outcomes.day_1_mastery == True]
mastery = df_experiment_outcomes.loc[df_experiment_outcomes.mastery == True]
mastery2 = df_experiment_outcomes.loc[
    ((df_experiment_outcomes.mastery == True) | (df_experiment_outcomes.skb_problem_count >= 10))]

print("============================================================================")
print(df_experiment_outcomes.control_treatments.value_counts())
print("============================================================================")
print(df_experiment_outcomes.groupby(['psa_id', 'control_treatments']).size().reset_index(name="frequency"))
print("============================================================================")
# print(" <= 12 problems to MASTERY::")
# print(day_1_mastery.control_treatments.value_counts())
# print("============================================================================")
# print(day_1_mastery.groupby(['psa_id', 'control_treatments']).size().reset_index(name="frequency"))
# print("============================================================================")
print("============================================================================")
print(" ATTRIT::")
print(mastery2.control_treatments.value_counts())
print("============================================================================")
print(mastery2.groupby(['psa_id', 'control_treatments']).size().reset_index(name="frequency"))
print("============================================================================")

df_filtered_v1_v2 = df_v1_v2.merge(
    df_experiment_outcomes[['psa_id', 'academic_year_merged', 'teacher_id', 'student_class_id', 'control_treatments',
                            'user_id', 'skb_problem_count']], how='right',
    on=['psa_id', 'academic_year_merged', 'teacher_id', 'student_class_id', 'control_treatments', 'user_id',
        'skb_problem_count'])

df_filtered_v1_v2.to_csv('../data/processed0.2/cwaf_v1_v2_plogs_final_R.csv', index=False)
df_experiment_outcomes = df_filtered_v1_v2.drop_duplicates(
    subset=['psa_id', 'academic_year_merged', 'teacher_id', 'student_class_id', 'control_treatments', 'user_id',
            'skb_problem_count'], keep='first')

# sns.set(style="whitegrid", rc={'figure.figsize': (40, 25)}, font_scale=1.5)
# sns.heatmap(
#     df_experiment_outcomes[['mastery', 'day_1_mastery', 'wheel_spinning', 'skb_problem_count']].corr(method='spearman'),
#     annot=True, cmap='PiYG')
# # sns.heatmap(df_experiment_outcomes.corr(), annot=True, cmap='PiYG')
# plt.xticks(rotation=35, horizontalalignment='right')
# plt.title("correlation ")
# plt.clf()

df_experiment_outcomes['mastery'] = df_experiment_outcomes['mastery'].astype(int)
df_experiment_outcomes['wheel_spinning'] = df_experiment_outcomes['wheel_spinning'].astype(int)

summer_year = ['14_summer', '15_summer', '16_summer', '17_summer', '18_summer', '19_summer', '20_summer', '21_summer',
               '22_summer']
academic_years = ['13_14', '14_15', '15_16', '16_17', '17_18', '18_19', '19_20', '20_21', '21_22']


logistic_regression_regular_year_PSAGF4 = smf.logit(
    'mastery ~ psa_id * control_treatments + prior_5pr_avg_correctness + prior_pr_avg_correctness',
    data=df_experiment_outcomes).fit()
print(logistic_regression_regular_year_PSAGF4.summary())

logistic_regression_regular_year_PSAGF4 = smf.logit(
    'mastery ~ control_treatments + academic_year_merged + prior_5pr_avg_correctness + prior_pr_avg_correctness',
    data=df_experiment_outcomes.loc[
        ((df_experiment_outcomes.psa_id == "PSAGF4") &
         (df_experiment_outcomes.academic_year_merged.isin(academic_years)))]).fit()
print(logistic_regression_regular_year_PSAGF4.summary())

logistic_regression_regular_year_PSAHQV = smf.logit(
    'mastery ~ control_treatments + academic_year_merged + prior_5pr_avg_correctness + prior_pr_avg_correctness',
    data=df_experiment_outcomes.loc[
        ((df_experiment_outcomes.psa_id == "PSAHQV") &
         (df_experiment_outcomes.academic_year_merged.isin(academic_years)))]).fit()
print(logistic_regression_regular_year_PSAHQV.summary())

logistic_regression_summer_year_PSAGF4 = smf.logit(
    'mastery ~ control_treatments + prior_5pr_avg_correctness + prior_pr_avg_correctness',
    data=df_experiment_outcomes.loc[
        ((df_experiment_outcomes.psa_id == "PSAGF4") &
         (df_experiment_outcomes.academic_year_merged.isin(summer_year)))]).fit()
print(logistic_regression_summer_year_PSAGF4.summary())

logistic_regression_summer_year_PSAHQV = smf.logit(
    'mastery ~ control_treatments + prior_5pr_avg_correctness + prior_pr_avg_correctness',
    data=df_experiment_outcomes.loc[
        ((df_experiment_outcomes.psa_id == "PSAHQV") &
         (df_experiment_outcomes.academic_year_merged.isin(summer_year)))]).fit()
print(logistic_regression_summer_year_PSAHQV.summary())
