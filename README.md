# Please note that while the analysis code is availabel through CC the dataset we use is covered by the [ASSISTments License](https://new.assistments.org/terms-and-conditions-assistments)

------
# LAK_CWAF

> 3 PSA in this section 
> PSABTZFT, PSAGF4, PSAHQV
>> PSABTZFT <br/>
>> Solving 2-Step Equations 7.EE.B.4a EX <br/>
>> control    : No Message ***2036940, 2036941, 2036942***<br/>
>> treatment1 : Msg with no SE ***2036943, 2036944, 2036945***<br/> 
>> treatment2 : Msg with SE***2036946, 2036947, 2036948***<br/>
>> posttest   : Immediate Post test ***2036939***<br/>
>> SKB: regular skb with feedback.
>
>> PSAHQV <br/>
>> Solving 2-Step Equations 7.EE.B.4a EX <br/>
>> control:  "S1: treatment1" <br/>
>> treatment:"S2: treatment2" 
>
>> PSAGF4 <br/>
>> Order of Operations: Addition and Multiplication 7.NS.A.3 EX <br/>
>> control:  "S1: treatment1" <br/>
>> treatment:"S2: treatment2" 
> 


### The preprocessing and initial analysis of this work is done using Python but the final analysis and verification of the data is done using R.

### Dev Notes

To the more adventerous who want to dive deeper into the python preprocessing:
1. The two analysis csv used in the final analysis are generated from 
  1. analysis_attrition.py from cwaf_v1_v2_plogs_final_R.csv 
  2. analysis_descriptives.py for cwaf_v1_v2_plogs_final_filtered_R.csv
2. Preprocessing1: merges data from different school years into a single dataframe for both V1 and V2 respectively
3. Preprocessing2: parses through all the problem logs of the students and categorizes them into conditions they were assigned to during the experiment, i.e., control(business as usual) and treatment(common wrong answer feedback)
4. Preprocessing3: this is just to check if there are duplicates as teachers occasionally reset assignmetns and let their studetns redo the skillbuilder that can lead to multiple records for the same student
5. Preprocessing4: this merges the v1 and v2 data into a single dataframe so that we can analyze the experiemnt 
6. Preprocessing5: additional exploration of the CWAF patterns at different points within the skill builder, i.e., within 5 problems. within 10 problems and so on.
7. analysis_attirtion: this conducted the attrition analysis across condition.
8. analysis_descriptives: this generated the descriptives to gain a better understanding of the student behavior across conditions.

