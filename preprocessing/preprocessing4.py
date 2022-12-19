import pandas as pd
import numpy as np
import sympy as solvepy


df_v1 = pd.read_csv("../data/processed0.2/cwaf_v1_new.csv")
df_v2 = pd.read_csv("../data/processed0.2/cwaf_v2_new.csv")

df_v1['teacher_id'] = df_v1.teacher_id.astype(str) + "_v1"
df_v1['problem_log_id'] = df_v1.problem_log_id.astype(str) + "_v1"
df_v1['assignment_id'] = df_v1.assignment_id.astype(str) + "_v1"
df_v1['student_class_id'] = df_v1.student_class_id.astype(str) + "_v1"
df_v1['user_id'] = df_v1.user_id.astype(str) + "_v1"

df_v2['teacher_id'] = df_v2.teacher_id.astype(str) + "_v2"
df_v2['problem_log_id'] = df_v2.problem_log_id.astype(str) + "_v2"
df_v2['assignment_id'] = df_v2.assignment_id.astype(str) + "_v2"
df_v2['student_class_id'] = df_v2.student_class_id.astype(str) + "_v2"
df_v2['user_id'] = df_v2.user_id.astype(str) + "_v2"

df_v1_v2 = pd.concat([df_v1, df_v2], ignore_index=True)
df_v1_v2['academic_year_merged'] = df_v1_v2['academic_year'].str[3:]

a = df_v1.groupby(['psa_id', 'academic_year', 'control_treatments', 'user_id']).size().reset_index(name='frequency')
b = df_v2.groupby(['psa_id', 'academic_year', 'control_treatments', 'user_id']).size().reset_index(name='frequency')
c = df_v1_v2.groupby(['psa_id', 'academic_year', 'control_treatments', 'user_id']).size().reset_index(name='frequency')
c_ = df_v1_v2.groupby(['psa_id', 'academic_year_merged', 'control_treatments', 'user_id']).size().reset_index(
    name='frequency')

a_count_frequency = a.groupby(['psa_id', 'academic_year', 'control_treatments']).size().reset_index(name='frequency')
b_count_frequency = b.groupby(['psa_id', 'academic_year', 'control_treatments']).size().reset_index(name='frequency')
c_count_frequency = c.groupby(['psa_id', 'academic_year', 'control_treatments']).size().reset_index(name='frequency')
c_count_frequency_ = c_.groupby(['psa_id', 'academic_year_merged', 'control_treatments']).size().reset_index(
    name='frequency')

a_count_frequency.sort_values(by=['psa_id', 'academic_year', 'control_treatments'], inplace=True)
b_count_frequency.sort_values(by=['psa_id', 'academic_year', 'control_treatments'], inplace=True)
c_count_frequency.sort_values(by=['psa_id', 'academic_year', 'control_treatments'], inplace=True)
c_count_frequency_.sort_values(by=['psa_id', 'academic_year_merged', 'control_treatments'], inplace=True)

df_prior_kng_v1 = pd.read_csv("../data/raw/rds_dev_prior_kng_dump_v1_CWAF_experiment.csv")
df_prior_kng_v2 = pd.read_csv("../data/raw/rds_dev_prior_kng_dump_v2_CWAF_experiment.csv")

df_ps_structure = pd.read_csv('../data/raw/rds_dev_PSA_cwaf_vs_nofeedback_v1_v2.csv')

counting_class_asignmetn_user_counts = df_v1_v2.groupby(
    ['psa_id', 'student_class_id', 'assignment_id', 'user_id']).size().reset_index(name="frequency")

counting_class_asignmetn_pairs = counting_class_asignmetn_user_counts.groupby(
    ['psa_id', 'student_class_id', 'assignment_id']).size().reset_index(name="frequency")
counting_psa_user_pairs = counting_class_asignmetn_user_counts.groupby(['psa_id', 'user_id']).size().reset_index(
    name="frequency")
repeaters = counting_psa_user_pairs.loc[counting_psa_user_pairs.frequency > 1]

# NOTE: there were 3436 instances where the students worked on a problem set more than ones.
#  This usually happens when teachers reset the assignmetn and have the studetns work on it again.
# Action: I am dropping the suplicates and only holding onto the first assignmetn which is where the
# studetns likely got randomized for the first time for the PSA

counting_class_asignmetn_user_counts = counting_class_asignmetn_user_counts[['psa_id', 'assignment_id', 'user_id']]
counting_class_asignmetn_user_counts.sort_values(by=['assignment_id', 'psa_id', 'user_id'], inplace=True)

counting_psa_user_pairs_ = counting_class_asignmetn_user_counts.groupby(['psa_id', 'user_id']).size().reset_index(
    name="frequency")
repeaters_ = counting_psa_user_pairs_.loc[counting_psa_user_pairs_.frequency > 1]
# print(repeaters_)
counting_class_asignmetn_user_counts.drop_duplicates(subset=['psa_id', 'user_id'], keep='first', inplace=True)

counting_psa_user_pairs_ = counting_class_asignmetn_user_counts.groupby(['psa_id', 'user_id']).size().reset_index(
    name="frequency")
repeaters_ = counting_psa_user_pairs_.loc[counting_psa_user_pairs_.frequency > 1]

final_df_v1_v2_clean1 = df_v1_v2.merge(counting_class_asignmetn_user_counts, how='inner',
                                       on=['psa_id', 'assignment_id', 'user_id'])

# NOTE: next we need to drop students who did more than one PS
#   dropping all the second or more instances if the did both using the assignment_id

counting_class_asignmetn_user_counts = final_df_v1_v2_clean1.groupby(
    ['assignment_id', 'user_id']).size().reset_index(name="frequency")
counting_class_asignmetn_user_counts.sort_values(by=['user_id', 'assignment_id'], inplace=True)

counting_psa_user_pairs__ = counting_class_asignmetn_user_counts.groupby(['user_id']).size().reset_index(
    name="frequency")
repeaters__ = counting_psa_user_pairs__.loc[counting_psa_user_pairs__.frequency > 1]
# print(repeaters__)
counting_class_asignmetn_user_counts.drop_duplicates(subset=['user_id'], keep='first', inplace=True)

counting_psa_user_pairs__ = counting_class_asignmetn_user_counts.groupby(['user_id']).size().reset_index(
    name="frequency")
repeaters__ = counting_psa_user_pairs__.loc[counting_psa_user_pairs__.frequency > 1]

final_df_v1_v2_clean2 = final_df_v1_v2_clean1.merge(counting_class_asignmetn_user_counts, how='inner',
                                                    on=['assignment_id', 'user_id'])
final_df_v1_v2_clean2.to_csv("../data/processed0.2/cwaf_v1_v2_plogs_final.csv", index=False)

print("=============================================================================================================")
print("All")
print('teacher',
      final_df_v1_v2_clean2.loc[final_df_v1_v2_clean2.psa_id.isin(["PSAHQV", "PSAGF4"])].teacher_id.unique().size)
print('class',
      final_df_v1_v2_clean2.loc[final_df_v1_v2_clean2.psa_id.isin(["PSAHQV", "PSAGF4"])].student_class_id.unique().size)
print('assignment',
      final_df_v1_v2_clean2.loc[final_df_v1_v2_clean2.psa_id.isin(["PSAHQV", "PSAGF4"])].assignment_id.unique().size)
print('students',
      final_df_v1_v2_clean2.loc[final_df_v1_v2_clean2.psa_id.isin(["PSAHQV", "PSAGF4"])].user_id.unique().size)
print("=============================================================================================================")
print("PSAHQV")
print('teacher', final_df_v1_v2_clean2.loc[final_df_v1_v2_clean2.psa_id == "PSAHQV"].teacher_id.unique().size)
print('class', final_df_v1_v2_clean2.loc[final_df_v1_v2_clean2.psa_id == "PSAHQV"].student_class_id.unique().size)
print('assignment', final_df_v1_v2_clean2.loc[final_df_v1_v2_clean2.psa_id == "PSAHQV"].assignment_id.unique().size)
print('students', final_df_v1_v2_clean2.loc[final_df_v1_v2_clean2.psa_id == "PSAHQV"].user_id.unique().size)
print("=============================================================================================================")
print("PSAGF4")
print('teacher', final_df_v1_v2_clean2.loc[final_df_v1_v2_clean2.psa_id == "PSAGF4"].teacher_id.unique().size)
print('class', final_df_v1_v2_clean2.loc[final_df_v1_v2_clean2.psa_id == "PSAGF4"].student_class_id.unique().size)
print('assignment', final_df_v1_v2_clean2.loc[final_df_v1_v2_clean2.psa_id == "PSAGF4"].assignment_id.unique().size)
print('students', final_df_v1_v2_clean2.loc[final_df_v1_v2_clean2.psa_id == "PSAGF4"].user_id.unique().size)
print("=============================================================================================================")

print("=============================================================================================================")
df_designed_cwas = pd.read_csv('../data/raw/rds_dev_feedback_bodies_cwaf_vs_nofeedback_v1_v2.csv')
df_designed_cwas = df_designed_cwas.loc[
    (df_designed_cwas.psa_id.isin(["PSAHQV", "PSAGF4"]) & (df_designed_cwas.name == 'S2 [treatment2]'))]
df_designed_cwas.drop_duplicates(subset=['problem_id', 'answer'], keep='first', inplace=True)
df_designed_cwas['solved_answer'] = -999999
answers1 = df_designed_cwas.answer.unique()
for answer1 in answers1:
    if not pd.isnull(answer1):
        df_designed_cwas.loc[df_designed_cwas.answer == answer1, "solved_answer"] = pd.eval(answer1)

final_df_v1_v2_clean2_count = final_df_v1_v2_clean2.loc[
    (final_df_v1_v2_clean2.psa_id.isin(["PSAHQV", "PSAGF4"]) & (final_df_v1_v2_clean2.continuous_score < 1))]

final_df_v1_v2_clean2_count['solved_answer'] = -999999
answers2 = final_df_v1_v2_clean2_count.answer_text.unique()
for answer2 in answers2:
    if not pd.isnull(answer2):
        # print("__" + answer2 +"__")
        try:
            if answer2.__len__() < 50:
                if answer2 in ['2(3-4)', '4(6)', '4(-8 * 6)', '7(8)', '5(6)', '9(2)+10', '2(4)+7', '10(8)+2', '7(5)-4', '7(4) - 2']:
                    final_df_v1_v2_clean2_count.loc[
                        final_df_v1_v2_clean2_count.answer_text == answer2, "solved_answer"] = pd.eval(answer2.replace('(', '*('))
                elif answer2 in ['(8-7)2']:
                    final_df_v1_v2_clean2_count.loc[
                        final_df_v1_v2_clean2_count.answer_text == answer2, "solved_answer"] = pd.eval(answer2.replace(')', ')*'))
                elif answer2 in ['4^5']:
                    final_df_v1_v2_clean2_count.loc[
                        final_df_v1_v2_clean2_count.answer_text == answer2, "solved_answer"] = pd.eval(answer2.replace('^', '**'))
                # elif answer2 in ['9 3/5', '-1 5/7', '27 3/7', '1 1/11', '-2 3/7', '2 1/5', '1 7/11', '-2 3/5', '1 2/3',
                #                  '-1 1/3', '3 2/3', '63 1/3', '1 1/3', '2 2/6', '1 5/7', '2 1/9', '-7 2/5', '1 1/9',
                #                  '2 8/9', '5 2/3', '-1 2/3', '7 3/5', '10 17/20', '1 1/2', '-4 1/5', '-3 1/3', '6 1/3',
                #                  '-2 1/2', '6 1/9', '7 1/2', '5 5/6', '2 4/5', '4 2/5', '2 6/7', '1 3/9', '2 1/2', '3 2/6',
                #                  '-2 1/3', '-3 2/3', '2 4/10']:
                #     final_df_v1_v2_clean2_count.loc[
                #         final_df_v1_v2_clean2_count.answer_text == answer2, "solved_answer"] = pd.eval(answer2.replace(' ', '*(')+')')
                elif answer2 in ['01', '04', '08', '084', '098765', '-098765']:
                    final_df_v1_v2_clean2_count.loc[
                        final_df_v1_v2_clean2_count.answer_text == answer2, "solved_answer"] = pd.eval(int(answer2))
                else:
                    final_df_v1_v2_clean2_count.loc[
                        final_df_v1_v2_clean2_count.answer_text == answer2, "solved_answer"] = pd.eval(answer2)
        except:
            error = 1
            # print("invalid syntax due to complex fraction")


final_df_v1_v2_clean2_CWA_count = final_df_v1_v2_clean2_count.groupby(
    ['psa_id', 'section_names', 'problem_id', 'answer_text', 'solved_answer']).size().reset_index(name="CWA_frequency")
final_df_v1_v2_clean2_CWA_count = final_df_v1_v2_clean2_CWA_count.merge(
    df_designed_cwas, how='outer', left_on=['psa_id', 'problem_id', 'solved_answer'],
    right_on=['psa_id', 'problem_id', 'solved_answer'])
final_df_v1_v2_clean2_CWA_count.sort_values(by=['psa_id', 'problem_id', 'CWA_frequency'], ascending=False, inplace=True)
# final_df_v1_v2_clean2_CWA_count.drop_duplicates(subset=['problem_id', 'answer_text'], keep='first', inplace=True)
final_df_v1_v2_clean2_CWA_count = final_df_v1_v2_clean2_CWA_count.loc[
    ~(final_df_v1_v2_clean2_CWA_count.is_correct == True)]

# final_df_v1_v2_clean2_CWA_count.to_csv("../data/processed0.2/core_cwa_grouping.csv", index=False)
final_df_v1_v2_clean2_CWA_count__ = final_df_v1_v2_clean2_CWA_count.loc[
    ((final_df_v1_v2_clean2_CWA_count.CWA_frequency >= 10) &
     (final_df_v1_v2_clean2_CWA_count.section_names == '{"S2 [treatment2]"}'))]
final_df_v1_v2_clean2_CWA_count__.reset_index(drop=True, inplace=True)

print("================================================================")
final_df_v1_v2_clean2_CWA_count__.is_correct.fillna(-1, inplace=True)
final_df_grouping = final_df_v1_v2_clean2_CWA_count__.groupby(
    ['psa_id', 'section_names', 'problem_id', 'is_correct']).agg({'CWA_frequency': ['sum', 'count']}).reset_index()

# final_df_grouping.to_csv("../data/processed0.2/cwa_grouping_5.csv", index=False)
# final_df_grouping.to_csv("../data/processed0.2/cwa_grouping_10.csv", index=False)
