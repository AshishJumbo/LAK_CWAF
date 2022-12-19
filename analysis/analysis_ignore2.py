import pandas as pd
import numpy as np
import statsmodels.formula.api as smf

# NOTE: For PSABTZFT treatment assistment_id are different but the
#  problem body is the same:
#  ===================================================================
#  |      Control        |   Treatment1        |   Treatment2        |
#  ===================================================================
#  |  1403645(2036942) <-->  1403648(2036945) <--> 1403651(2036948)  |
#  |  1403643(2036940) <-->  1403646(2036943) <--> 1403649(2036946)  |
#  |  1403644(2036941) <-->  1403647(2036944) <--> 1403650(2036947)  |
#  ===================================================================

# similar_problems = {
#     'control': [2036942, 2036940, 2036941],
#     'treatment1': [2036945, 2036943, 2036944],
#     'treatment2': [2036948, 2036946, 2036947]
# }
# df_similar_problems = pd.DataFrame.from_dict(similar_problems)
# df_pr_cwas = pd.read_csv("../data/raw/rds_dev_PSABTZFT_feedback_bodies_cwaf_vs_nofeedback_v1_v2_new.csv")
# df_pr_cwas = df_pr_cwas.merge(df_similar_problems, how='inner', left_on=['problem_id'], right_on=['treatment2'])
# df_pr_cwas.to_csv("../data/raw/rds_dev_PSABTZFT_feedback_bodies_cwaf_vs_nofeedback_v1_v2_new.csv", index=False)


def compute_regression(data_regression, caption=""):
    print("======================================================================================")
    print("     Regression Title: ", caption.upper())
    print("======================================================================================")
    print("     logistic regression:: formula ->  posttest_score ~ control_treatments")
    print("--------------------------------------------------------------------------------------")
    log_regression = smf.logit('posttest_score ~ control_treatments', data=data_regression).fit()
    print(log_regression.summary())
    print("======================================================================================")
    print("     logistic regression:: formula ->  mastery ~ control_treatments")
    print("--------------------------------------------------------------------------------------")
    log_regression = smf.logit('mastery ~ control_treatments', data=data_regression).fit()
    print(log_regression.summary())
    print("======================================================================================")
    print("     logistic regression:: formula ->  wheel_spinning ~ control_treatments")
    print("--------------------------------------------------------------------------------------")
    log_regression = smf.logit('wheel_spinning ~ control_treatments', data=data_regression).fit()
    print(log_regression.summary())
    print("======================================================================================")
    # print("   linear regression:: formula ->  skb_problem_count ~ control_treatments")
    # print("--------------------------------------------------------------------------------------")
    # ols_regression = smf.ols('skb_problem_count ~ control_treatments', data=data_regression).fit()
    # print(ols_regression.summary())
    # print("====================================================================================")


df_pr_cwas = pd.read_csv("../data/raw/rds_dev_PSABTZFT_feedback_bodies_cwaf_vs_nofeedback_v1_v2_new.csv")
df_v2_new = pd.read_csv("../data/processed0.2/cwaf_v2_new.csv")
df_v2_randomization_balance = df_v2_new.loc[((df_v2_new.psa_id == "PSABTZFT") &
                                             (df_v2_new.control_treatments.isin(
                                                 ['treatment2_initial', 'control_initial', 'treatment1_initial'])))]

df_v2_new = df_v2_new.loc[((df_v2_new.psa_id == "PSABTZFT") & (df_v2_new.control_treatments == "posttest"))]
df_v2_new = df_v2_new[['continuous_score', 'user_id', 'assignment_id']]
df_v2_new.rename({"continuous_score": "posttest_score"}, axis=1, inplace=True)
df_v2_randomization_balance = df_v2_randomization_balance.merge(df_v2_new, how='left', on=['user_id', 'assignment_id'])

c_ = df_v2_randomization_balance.groupby(
    ['psa_id', 'control_treatments', 'continuous_score', 'user_id', 'mastery', 'wheel_spinning',
     'skb_problem_count', 'posttest_score']).size().reset_index(name='frequency')
c_['mastery'] = c_['mastery'].astype(int)
c_['wheel_spinning'] = c_['wheel_spinning'].astype(int)
# c__ = c_.loc[c_.continuous_score < 1]
c__ = c_.loc[c_.continuous_score <= 1]

print("\n")
data = c__
compute_regression(data, "Intent to Treat")
print("\n \n")

# NOTE: just trying to see if there are other CWA that don't have a feedback
other_cwas = df_v2_randomization_balance.loc[df_v2_randomization_balance.continuous_score < 1].groupby(["pra_id", "problem_id", "answer_text"]).size().reset_index(name='frequency')
other_cwas = other_cwas.merge(df_pr_cwas, how='left', left_on=['problem_id', 'answer_text'], right_on=['problem_id', 'answer'])
other_cwas.is_correct.replace(np.nan, False, inplace=True)
other_cwas = other_cwas.loc[~other_cwas.is_correct]
other_cwas.sort_values(by=['problem_id', 'frequency'], ascending=False, inplace=True)
other_cwas['is_new_cwa'] = other_cwas.position.isna()

# NOTE:
#  let's only look at studetns who actually gave a CWA as their first attempt
c___ = df_v2_randomization_balance.merge(df_pr_cwas, how='inner', left_on=['problem_id', 'answer_text'],
                                         right_on=['problem_id', 'answer'])
c___ = c___.loc[~c___.is_correct]
c___ = c___.groupby(['psa_id', 'control_treatments', 'continuous_score', 'user_id', 'mastery', 'wheel_spinning',
                     'skb_problem_count', 'posttest_score']).size().reset_index(name='frequency')
c___['mastery'] = c___['mastery'].astype(int)
c___['wheel_spinning'] = c___['wheel_spinning'].astype(int)

data = c___
compute_regression(data, "Treated Analysis: the first attempt was a CWA")
print("\n")



