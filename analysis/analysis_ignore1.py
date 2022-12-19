import pandas as pd
import statsmodels.formula.api as smf

df_v1 = pd.read_csv("../data/processed0.2/cwaf_v1_new.csv")
df_v2 = pd.read_csv("../data/processed0.2/cwaf_v2_new.csv")
df_v1_v2 = pd.concat([df_v1, df_v2], ignore_index=True)
df_v1_v2['academic_year_merged'] = df_v1_v2['academic_year'].str[3:]

# academic_years = ['13_14', '14_15', '15_16', '16_17', '17_18', '18_19', '19_20', '20_21', '21_22']
academic_years = ['16_17', '17_18', '18_19', '19_20', '20_21', '21_22']
summer_year = ['14_summer', '15_summer', '16_summer', '17_summer', '18_summer', '19_summer',
               '20_summer', '21_summer', '22_summer']
c_ = df_v1_v2.groupby(['psa_id', 'academic_year_merged', 'control_treatments', 'user_id', 'mastery',
                       'wheel_spinning', 'skb_problem_count']).size().reset_index(name='frequency')
c_['mastery'] = c_['mastery'].astype(int)
c_['wheel_spinning'] = c_['wheel_spinning'].astype(int)
c_count_frequency_ = c_.groupby(['psa_id', 'academic_year_merged', 'control_treatments', 'mastery']).size().reset_index(
    name='frequency')

print("===================================================")

log_regression = smf.logit('mastery ~ psa_id * control_treatments + C(academic_year_merged)',
                           data=c_.loc[c_.academic_year_merged.isin(academic_years)]).fit()
print(log_regression.summary())
print("===================================================")

log_regression = smf.logit('mastery ~ psa_id * control_treatments',
                           data=c_.loc[c_.academic_year_merged.isin(academic_years)]).fit()
print(log_regression.summary())
print("===================================================")

log_regression = smf.logit('mastery ~ control_treatments',
                           data=c_.loc[c_.academic_year_merged.isin(academic_years)]).fit()
print(log_regression.summary())
print("===================================================")

print("===================================================")

log_regression = smf.logit('wheel_spinning ~ psa_id * control_treatments + C(academic_year_merged)',
                           data=c_.loc[c_.academic_year_merged.isin(academic_years)]).fit()
print(log_regression.summary())
print("===================================================")

log_regression = smf.logit('wheel_spinning ~ psa_id * control_treatments',
                           data=c_.loc[c_.academic_year_merged.isin(academic_years)]).fit()
print(log_regression.summary())
print("===================================================")

log_regression = smf.logit('wheel_spinning ~ control_treatments',
                           data=c_.loc[c_.academic_year_merged.isin(academic_years)]).fit()
print(log_regression.summary())
print("===================================================")

print("===================================================")

linear_regression = smf.ols('skb_problem_count ~ psa_id * control_treatments + C(academic_year_merged)',
                            data=c_.loc[c_.academic_year_merged.isin(academic_years)]).fit()
print(linear_regression.summary())
print("===================================================")

linear_regression = smf.ols('skb_problem_count ~ psa_id * control_treatments',
                            data=c_.loc[c_.academic_year_merged.isin(academic_years)]).fit()
print(linear_regression.summary())
print("===================================================")

linear_regression = smf.ols('skb_problem_count ~ control_treatments',
                            data=c_.loc[c_.academic_year_merged.isin(academic_years)]).fit()
print(linear_regression.summary())
print("===================================================")

c_ = df_v1_v2.groupby(['psa_id', 'academic_year_merged', 'control_treatments', 'user_id', 'mastery',
                       'wheel_spinning', 'skb_problem_count'])['first_response_time'].mean().reset_index(
    name='avg_first_response_time')
c_['mastery'] = c_['mastery'].astype(int)
c_['wheel_spinning'] = c_['wheel_spinning'].astype(int)
c_['avg_first_response_time'] /= 1000

print("===================================================")

linear_regression = smf.ols('avg_first_response_time ~ psa_id * control_treatments + C(academic_year_merged)',
                            data=c_.loc[c_.academic_year_merged.isin(academic_years)]).fit()
print(linear_regression.summary())
print("===================================================")

linear_regression = smf.ols('avg_first_response_time ~ psa_id * control_treatments',
                            data=c_.loc[c_.academic_year_merged.isin(academic_years)]).fit()
print(linear_regression.summary())
print("===================================================")

linear_regression = smf.ols('avg_first_response_time ~ control_treatments',
                            data=c_.loc[c_.academic_year_merged.isin(academic_years)]).fit()
print(linear_regression.summary())
print("===================================================")
