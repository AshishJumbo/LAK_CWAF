def check_for_mastery_wheel_spinning(df_temp_, psa_id):
    user_ids = df_temp_.loc[df_temp_.psa_id == psa_id].user_id.unique()

    for user_id in user_ids:
        assignment_ids = df_temp_.loc[(df_temp_.psa_id == psa_id) & (df_temp_.user_id == user_id)].assignment_id.unique()
        for assignment_id in assignment_ids:
            problem_log_ids = df_temp_.loc[
                (df_temp_.psa_id == psa_id) & (df_temp_.user_id == user_id) & (df_temp_.assignment_id == assignment_id)
                ].problem_log_id

            skb_mastery_count = 0
            skb_problem_count = 0
            for problem_log_id in problem_log_ids:
                # control_treatment_info = df_temp_.loc[df_temp_.problem_log_id == problem_log_id].control_treatments.unique()[0]
                # print(control_treatment_info)
                # if 'initial' in control_treatment_info:
                #     continue
                # else:
                continuous_score = df_temp_.loc[df_temp_.problem_log_id == problem_log_id].continuous_score.values
                skb_problem_count += 1
                if len(continuous_score) > 0:
                    if continuous_score[0] == 1:
                        skb_mastery_count += 1
                    else:
                        skb_mastery_count = 0
                else:
                    skb_mastery_count = 0

            if skb_mastery_count >= 3:
                df_temp_.loc[(df_temp_.psa_id == psa_id) & (df_temp_.user_id == user_id) & (
                        df_temp_.assignment_id == assignment_id), 'mastery'] = True
                df_temp_.loc[(df_temp_.psa_id == psa_id) & (df_temp_.user_id == user_id) & (
                        df_temp_.assignment_id == assignment_id), 'skb_mastery_count'] = skb_mastery_count

            if skb_problem_count >= 12:
                df_temp_.loc[(df_temp_.psa_id == psa_id) & (df_temp_.user_id == user_id) & (
                        df_temp_.assignment_id == assignment_id), 'wheel_spinning'] = True

            df_temp_.loc[(df_temp_.psa_id == psa_id) & (df_temp_.user_id == user_id) & (
                    df_temp_.assignment_id == assignment_id), 'skb_problem_count'] = skb_problem_count

    return df_temp_

