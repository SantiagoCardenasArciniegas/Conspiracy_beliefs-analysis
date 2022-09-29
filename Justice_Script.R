

# Packages ----------------------------------------------------------------


library(easypackages)

libraries("boot","ggm","ggplot2","polycor","Hmisc","dplyr",
          "readxl","devtools","tidyverse","lubridate",
          "ggridges","wesanderson","RColorBrewer","knitr",
          "kableExtra","hrbrthemes","statsr","stargazer","psych",
          "corrplot","corrr","GGally","ggcorrplot","PerformanceAnalytics",
          "pander","broom","purrr","kableExtra","egg", "vtable",
          "qwraps2","readxl","forestmangr","janitor","gtools")


# Directories -------------------------------------------------------------


dir.create("Correlation_Matrices_html")
dir.create("Summary_Tables")
dir.create("Correlation_Plots")
dir.create("Regression_Tables")
dir.create("Graficas_Definitivas")
dir.create("Bases_CSV")

Datos_Justice <- read_excel("Justice and society.xlsx")




# Cleaning data set -------------------------------------------------------


#Erase all empty columns
Datos_Justice<- rm_empty_col(Datos_Justice)
View(Datos_Justice)

#Clean names function from Janitor package

Datos_Justice <- clean_names(Datos_Justice)

#RENAME (for long names)

Datos_Justice_Renamed <-Datos_Justice %>% 
  rename(perception_justice=dear_participant_we_want_to_understand_the_impact_of_the_perception_of_justice_on_how_people_feel_about_society_you_will_be_asked_a_series_of_questions_on_a_form_you_just_have_to_check_the_box,
         prolific_id=please_insert_your_unique_prolific_id,
         age=how_old_are_you,
         gender=what_gender_do_you_identify_yourself_with,
        describe_yourself=how_would_you_describe_yourself_respondents_can_choose_multiple_options,
        highest_degree_finished =what_is_the_highest_degree_or_level_of_school_you_have_completed_if_you_re_currently_enrolled_in_school_please_indicate_the_highest_degree_you_have_finished,
        political_orientation=what_is_your_political_orientation,
        fairness_with_which_issues_and_decisions_that_come_up_in_us_society_are_handled=how_would_you_rate_the_overall_fairness_with_which_issues_and_decisions_that_come_up_in_us_society_are_handled)



unique(Datos_Justice$comic_books)



# Recoding ----------------------------------------------------------------


Datos_Justice_Renamed_Filtered <-Datos_Justice_Renamed %>% 
  select(#Quality of Decision-Making Procedures (FORMAl/16)
        the_rules_dictate_that_decisions_should_be_fair_and_unbiased,
        the_rules_and_procedures_are_applied_consistently_across_people_and_situations,
        the_rules_ensure_that_decisions_are_made_based_on_facts_not_personal_biases_and_opinions,
        the_rules_and_procedures_in_the_us_society_are_equally_fair_to_everyone,
    #Quality of Decision-Making Procedures (Informal/16)
        decisions_are_consistent_across_people_and_situations,
        decisions_are_made_based_on_facts_not_their_personal_biases_and_opinions,
        decisions_are_equally_fair_to_everyone,
    #Quality of Treatment (17)
        the_rules_lead_to_fair_treatment_when_decisions_are_being_made,
        the_rules_lead_to_fair_treatment_when_decisions_are_being_implemented,
        the_rules_require_that_i_get_an_honest_explanation_for_how_decisions_are_made,
        my_views_are_considered_when_rules_are_being_applied,
        the_rules_ensure_that_my_needs_will_be_taken_into_account,
        the_rules_respect_my_rights_as_a_citizen, 
        the_rules_respect_my_rights_as_a_person,
    # (Identification/ 23)
        when_i_talk_about_the_us_society_i_usually_say_we_rather_than_they,
        i_have_a_sense_that_i_personally_belong_in_the_us_society,
        i_feel_like_an_important_part_of_the_us_society,
        i_feel_connected_to_the_people_around_me, 
        i_am_close_to_other_people_within_the_us_society_regardless_of_the_community_to_which_they_belong,
        others_leave_me_out_of_the_group,
    #(Pride/23)
        i_feel_proud_to_be_a_part_of_the_us_society,
        my_community_group_is_highly_respected_within_the_us_society_community_group_american_indian_or_alaska_native_asian_black_or_african_american_latinx_native_hawaiian_or_other_pacific_islander_w,
    #24. Respect
        value_you_as_a_member_and_respect_your_ideas,
        value_what_you_contribute_to_society_respect_the_work_you_do,
        appreciate_your_unique_contributions,    
    #DEPENDENT Variable (Conspiracy belief/ 26)
        the_government_is_involved_in_the_murder_of_innocent_citizens_and_or_well_known_public_figures_and_keeps_this_a_secret_the_power_held_by_heads_of_state_is_second_to_that_of_small_unknown_groups,
        secret_organizations_communicate_with_extraterrestrials_but_keep_this_fact_from_the_public,
        the_spread_of_certain_viruses_and_or_diseases_is_the_result_of_the_deliberate_concealed_efforts_of_some_organization_groups_of_scientists_manipulate_fabricate_or_suppress_evidence_in_order_to_d,
        the_government_permits_or_perpetrates_acts_of_terrorism_on_its_own_soil_disguising_its_involvement,
        a_small_secret_group_of_people_is_responsible_for_making_all_major_world_decisions_such_as_going_to_war,
        evidence_of_alien_contact_is_being_concealed_from_the_public,
        technology_with_mind_control_capacities_is_used_on_people_without_their_knowledge,
        new_and_advanced_technology_which_would_harm_current_industry_is_being_suppressed,
        the_government_uses_people_as_patsies_to_hide_its_involvement_in_criminal_activity,
        certain_significant_events_have_been_the_result_of_the_activity_of_a_small_group_who_secretly_manipulate_world_events,
        some_ufo_sightings_and_rumors_are_planned_or_staged_in_order_to_distract_the_public_from_real_alien_contact,
        experiments_involving_new_drugs_or_technologies_are_routinely_carried_out_on_the_public_without_their_knowledge_or_consent_a_lot_of_important_information_is_deliberately_concealed_from_the_public,
      #(Socio-economic level/9)
        what_socio_economic_level_do_you_identify_with,
    #(Education)
        highest_degree_finished,
    #(Literacy level item / 7)
        i_only_read_if_i_have_to,
        reading_is_one_of_my_favorite_pastimes,
        i_find_it_hard_to_finish_a_book,
        for_me_reading_for_pleasure_is_a_waste_of_time,
        i_really_enjoy_going_to_a_bookstore_or_a_library,
        as_soon_as_i_finish_a_book_i_already_think_about_the_one_im_going_to_start_next,
    #Group (minority/majority / 5)
        describe_yourself,
    #(Political orientation /10)
        political_orientation)




#the_rules_dictate_that_decisions_should_be_fair_and_unbiased

#1= Strongly disagree, 2= Disagree, 3= Slightly disagree, 4= Slightly agree , 5= Agree, 6= Strongly agree


justice_recoded <- Datos_Justice_Renamed_Filtered %>% 
  mutate(the_rules_dictate_that_decisions_should_be_fair_and_unbiased=
           recode(the_rules_dictate_that_decisions_should_be_fair_and_unbiased, 
                  "strongly disagree" = 1,
                  "Disagree"=2,
                  "Slightly Disagree"=3,
                  "Slightly Agree"=4,
                  "Agree" =5,
                  "strongly agree."=6))


unique(Datos_Justice_Renamed_Filtered$the_rules_dictate_that_decisions_should_be_fair_and_unbiased)
sum(is.na(Datos_Justice_Renamed_Filtered$the_rules_dictate_that_decisions_should_be_fair_and_unbiased))


sum(is.na(justice_recoded$the_rules_dictate_that_decisions_should_be_fair_and_unbiased))
unique(justice_recoded$the_rules_dictate_that_decisions_should_be_fair_and_unbiased)



#the_rules_and_procedures_are_applied_consistently_across_people_and_situations,

#1= Strongly disagree, 2= Disagree, 3= Slightly disagree, 4= Slightly agree , 5= Agree, 6= Strongly agree

justice_recoded <- justice_recoded %>% 
  mutate(the_rules_and_procedures_are_applied_consistently_across_people_and_situations=
           recode(the_rules_and_procedures_are_applied_consistently_across_people_and_situations, 
                  "strongly disagree" = 1,
                  "Disagree"=2,
                  "Slightly Disagree"=3,
                  "Slightly Agree"=4,
                  "Agree" =5,
                  "strongly agree."=6))


unique(Datos_Justice_Renamed_Filtered$the_rules_and_procedures_are_applied_consistently_across_people_and_situations)
sum(is.na(Datos_Justice_Renamed_Filtered$the_rules_and_procedures_are_applied_consistently_across_people_and_situations))

unique(justice_recoded$the_rules_and_procedures_are_applied_consistently_across_people_and_situations)
sum(is.na(justice_recoded$the_rules_and_procedures_are_applied_consistently_across_people_and_situations))




#the_rules_ensure_that_decisions_are_made_based_on_facts_not_personal_biases_and_opinions

#1= Strongly disagree, 2= Disagree, 3= Slightly disagree, 4= Slightly agree , 5= Agree, 6= Strongly agree

justice_recoded <- justice_recoded %>% 
  mutate(the_rules_ensure_that_decisions_are_made_based_on_facts_not_personal_biases_and_opinions=
           recode(the_rules_ensure_that_decisions_are_made_based_on_facts_not_personal_biases_and_opinions, 
                  "strongly disagree" = 1,
                  "Disagree"=2,
                  "Slightly Disagree"=3,
                  "Slightly Agree"=4,
                  "Agree" =5,
                  "strongly agree."=6))


unique(Datos_Justice_Renamed_Filtered$the_rules_ensure_that_decisions_are_made_based_on_facts_not_personal_biases_and_opinions)
sum(is.na(Datos_Justice_Renamed_Filtered$the_rules_ensure_that_decisions_are_made_based_on_facts_not_personal_biases_and_opinions))

unique(justice_recoded$the_rules_ensure_that_decisions_are_made_based_on_facts_not_personal_biases_and_opinions)
sum(is.na(justice_recoded$the_rules_ensure_that_decisions_are_made_based_on_facts_not_personal_biases_and_opinions))





#the_rules_and_procedures_in_the_us_society_are_equally_fair_to_everyone

#1= Strongly disagree, 2= Disagree, 3= Slightly disagree, 4= Slightly agree , 5= Agree, 6= Strongly agree


justice_recoded <- justice_recoded %>% 
  mutate(the_rules_and_procedures_in_the_us_society_are_equally_fair_to_everyone=
           recode(the_rules_and_procedures_in_the_us_society_are_equally_fair_to_everyone, 
                  "strongly disagree" = 1,
                  "Disagree"=2,
                  "Slightly Disagree"=3,
                  "Slightly Agree"=4,
                  "Agree" =5,
                  "strongly agree."=6))


unique(Datos_Justice_Renamed_Filtered$the_rules_and_procedures_in_the_us_society_are_equally_fair_to_everyone)
sum(is.na(Datos_Justice_Renamed_Filtered$the_rules_and_procedures_in_the_us_society_are_equally_fair_to_everyone))

unique(justice_recoded$the_rules_and_procedures_in_the_us_society_are_equally_fair_to_everyone)
sum(is.na(justice_recoded$the_rules_and_procedures_in_the_us_society_are_equally_fair_to_everyone))




#decisions_are_consistent_across_people_and_situations

#1= Strongly disagree, 2= Disagree, 3= Slightly disagree, 4= Slightly agree , 5= Agree, 6= Strongly agree


justice_recoded <- justice_recoded %>% 
  mutate(decisions_are_consistent_across_people_and_situations=
           recode(decisions_are_consistent_across_people_and_situations, 
                  "strongly disagree" = 1,
                  "Disagree"=2,
                  "Slightly Disagree"=3,
                  "Slightly Agree"=4,
                  "Agree" =5,
                  "strongly agree."=6))


unique(Datos_Justice_Renamed_Filtered$decisions_are_consistent_across_people_and_situations)
sum(is.na(Datos_Justice_Renamed_Filtered$decisions_are_consistent_across_people_and_situations))

unique(justice_recoded$decisions_are_consistent_across_people_and_situations)
sum(is.na(justice_recoded$decisions_are_consistent_across_people_and_situations))




#decisions_are_made_based_on_facts_not_their_personal_biases_and_opinions

#1= Strongly disagree, 2= Disagree, 3= Slightly disagree, 4= Slightly agree , 5= Agree, 6= Strongly agree


justice_recoded <- justice_recoded %>% 
  mutate(decisions_are_made_based_on_facts_not_their_personal_biases_and_opinions=
           recode(decisions_are_made_based_on_facts_not_their_personal_biases_and_opinions, 
                  "strongly disagree" = 1,
                  "Disagree"=2,
                  "Slightly Disagree"=3,
                  "Slightly Agree"=4,
                  "Agree" =5,
                  "strongly agree."=6))


unique(Datos_Justice_Renamed_Filtered$decisions_are_made_based_on_facts_not_their_personal_biases_and_opinions)
sum(is.na(Datos_Justice_Renamed_Filtered$decisions_are_made_based_on_facts_not_their_personal_biases_and_opinions))

unique(justice_recoded$decisions_are_made_based_on_facts_not_their_personal_biases_and_opinions)
sum(is.na(justice_recoded$decisions_are_made_based_on_facts_not_their_personal_biases_and_opinions))





#decisions_are_equally_fair_to_everyone

#1= Strongly disagree, 2= Disagree, 3= Slightly disagree, 4= Slightly agree , 5= Agree, 6= Strongly agree


justice_recoded <- justice_recoded %>% 
  mutate(decisions_are_equally_fair_to_everyone=
           recode(decisions_are_equally_fair_to_everyone, 
                  "strongly disagree" = 1,
                  "Disagree"=2,
                  "Slightly Disagree"=3,
                  "Slightly Agree"=4,
                  "Agree" =5,
                  "strongly agree."=6))


unique(Datos_Justice_Renamed_Filtered$decisions_are_equally_fair_to_everyone)
sum(is.na(Datos_Justice_Renamed_Filtered$decisions_are_equally_fair_to_everyone))

unique(justice_recoded$decisions_are_equally_fair_to_everyone)
sum(is.na(justice_recoded$decisions_are_equally_fair_to_everyone))





#decisions_are_equally_fair_to_everyone

#1= Strongly disagree, 2= Disagree, 3= Slightly disagree, 4= Slightly agree , 5= Agree, 6= Strongly agree


justice_recoded <- justice_recoded %>% 
  mutate(decisions_are_equally_fair_to_everyone=
           recode(decisions_are_equally_fair_to_everyone, 
                  "strongly disagree" = 1,
                  "Disagree"=2,
                  "Slightly Disagree"=3,
                  "Slightly Agree"=4,
                  "Agree" =5,
                  "strongly agree."=6))


unique(Datos_Justice_Renamed_Filtered$decisions_are_equally_fair_to_everyone)
sum(is.na(Datos_Justice_Renamed_Filtered$decisions_are_equally_fair_to_everyone))

unique(justice_recoded$decisions_are_equally_fair_to_everyone)
sum(is.na(justice_recoded$decisions_are_equally_fair_to_everyone))


#the_rules_lead_to_fair_treatment_when_decisions_are_being_made

#1= Strongly disagree, 2= Disagree, 3= Slightly disagree, 4= Slightly agree , 5= Agree, 6= Strongly agree



justice_recoded <- justice_recoded %>% 
  mutate(the_rules_lead_to_fair_treatment_when_decisions_are_being_made=
           recode(the_rules_lead_to_fair_treatment_when_decisions_are_being_made, 
                  "strongly disagree" = 1,
                  "Disagree"=2,
                  "Slightly Disagree"=3,
                  "Slightly Agree"=4,
                  "Agree" =5,
                  "strongly agree"=6))


unique(Datos_Justice_Renamed_Filtered$the_rules_lead_to_fair_treatment_when_decisions_are_being_made)
sum(is.na(Datos_Justice_Renamed_Filtered$the_rules_lead_to_fair_treatment_when_decisions_are_being_made))

unique(justice_recoded$the_rules_lead_to_fair_treatment_when_decisions_are_being_made)
sum(is.na(justice_recoded$the_rules_lead_to_fair_treatment_when_decisions_are_being_made))




#the_rules_lead_to_fair_treatment_when_decisions_are_being_implemented

#1= Strongly disagree, 2= Disagree, 3= Slightly disagree, 4= Slightly agree , 5= Agree, 6= Strongly agree



justice_recoded <- justice_recoded %>% 
  mutate(the_rules_lead_to_fair_treatment_when_decisions_are_being_implemented=
           recode(the_rules_lead_to_fair_treatment_when_decisions_are_being_implemented, 
                  "strongly disagree" = 1,
                  "Disagree"=2,
                  "Slightly Disagree"=3,
                  "Slightly Agree"=4,
                  "Agree" =5,
                  "strongly agree"=6))


unique(Datos_Justice_Renamed_Filtered$the_rules_lead_to_fair_treatment_when_decisions_are_being_implemented)
sum(is.na(Datos_Justice_Renamed_Filtered$the_rules_lead_to_fair_treatment_when_decisions_are_being_implemented))

unique(justice_recoded$the_rules_lead_to_fair_treatment_when_decisions_are_being_implemented)
sum(is.na(justice_recoded$the_rules_lead_to_fair_treatment_when_decisions_are_being_implemented))


#the_rules_require_that_i_get_an_honest_explanation_for_how_decisions_are_made

#1= Strongly disagree, 2= Disagree, 3= Slightly disagree, 4= Slightly agree , 5= Agree, 6= Strongly agree


justice_recoded <- justice_recoded %>% 
  mutate(the_rules_require_that_i_get_an_honest_explanation_for_how_decisions_are_made=
           recode(the_rules_require_that_i_get_an_honest_explanation_for_how_decisions_are_made, 
                  "strongly disagree" = 1,
                  "Disagree"=2,
                  "Slightly Disagree"=3,
                  "Slightly Agree"=4,
                  "Agree" =5,
                  "strongly agree"=6))


unique(Datos_Justice_Renamed_Filtered$the_rules_require_that_i_get_an_honest_explanation_for_how_decisions_are_made)
sum(is.na(Datos_Justice_Renamed_Filtered$the_rules_require_that_i_get_an_honest_explanation_for_how_decisions_are_made))

unique(justice_recoded$the_rules_require_that_i_get_an_honest_explanation_for_how_decisions_are_made)
sum(is.na(justice_recoded$the_rules_require_that_i_get_an_honest_explanation_for_how_decisions_are_made))




#the_rules_require_that_i_get_an_honest_explanation_for_how_decisions_are_made

#1= Strongly disagree, 2= Disagree, 3= Slightly disagree, 4= Slightly agree , 5= Agree, 6= Strongly agree


justice_recoded <- justice_recoded %>% 
  mutate(the_rules_require_that_i_get_an_honest_explanation_for_how_decisions_are_made=
           recode(the_rules_require_that_i_get_an_honest_explanation_for_how_decisions_are_made, 
                  "strongly disagree" = 1,
                  "Disagree"=2,
                  "Slightly Disagree"=3,
                  "Slightly Agree"=4,
                  "Agree" =5,
                  "strongly agree"=6))


unique(Datos_Justice_Renamed_Filtered$the_rules_require_that_i_get_an_honest_explanation_for_how_decisions_are_made)
sum(is.na(Datos_Justice_Renamed_Filtered$the_rules_require_that_i_get_an_honest_explanation_for_how_decisions_are_made))

unique(justice_recoded$the_rules_require_that_i_get_an_honest_explanation_for_how_decisions_are_made)
sum(is.na(justice_recoded$the_rules_require_that_i_get_an_honest_explanation_for_how_decisions_are_made))



#my_views_are_considered_when_rules_are_being_applied

#1= Strongly disagree, 2= Disagree, 3= Slightly disagree, 4= Slightly agree , 5= Agree, 6= Strongly agree


justice_recoded <- justice_recoded %>% 
  mutate(my_views_are_considered_when_rules_are_being_applied=
           recode(my_views_are_considered_when_rules_are_being_applied, 
                  "strongly disagree" = 1,
                  "Disagree"=2,
                  "Slightly Disagree"=3,
                  "Slightly Agree"=4,
                  "Agree" =5,
                  "strongly agree"=6))


unique(Datos_Justice_Renamed_Filtered$my_views_are_considered_when_rules_are_being_applied)
sum(is.na(Datos_Justice_Renamed_Filtered$my_views_are_considered_when_rules_are_being_applied))

unique(justice_recoded$my_views_are_considered_when_rules_are_being_applied)
sum(is.na(justice_recoded$my_views_are_considered_when_rules_are_being_applied))




#the_rules_ensure_that_my_needs_will_be_taken_into_account

#1= Strongly disagree, 2= Disagree, 3= Slightly disagree, 4= Slightly agree , 5= Agree, 6= Strongly agree


justice_recoded <- justice_recoded %>% 
  mutate(the_rules_ensure_that_my_needs_will_be_taken_into_account=
           recode(the_rules_ensure_that_my_needs_will_be_taken_into_account, 
                  "strongly disagree" = 1,
                  "Disagree"=2,
                  "Slightly Disagree"=3,
                  "Slightly Agree"=4,
                  "Agree" =5,
                  "strongly agree"=6))


unique(Datos_Justice_Renamed_Filtered$the_rules_ensure_that_my_needs_will_be_taken_into_account)
sum(is.na(Datos_Justice_Renamed_Filtered$the_rules_ensure_that_my_needs_will_be_taken_into_account))

unique(justice_recoded$the_rules_ensure_that_my_needs_will_be_taken_into_account)
sum(is.na(justice_recoded$the_rules_ensure_that_my_needs_will_be_taken_into_account))




#the_rules_respect_my_rights_as_a_citizen

#1= Strongly disagree, 2= Disagree, 3= Slightly disagree, 4= Slightly agree , 5= Agree, 6= Strongly agree


justice_recoded <- justice_recoded %>% 
  mutate(the_rules_respect_my_rights_as_a_citizen=
           recode(the_rules_respect_my_rights_as_a_citizen, 
                  "strongly disagree" = 1,
                  "Disagree"=2,
                  "Slightly Disagree"=3,
                  "Slightly Agree"=4,
                  "Agree" =5,
                  "strongly agree"=6))


unique(Datos_Justice_Renamed_Filtered$the_rules_respect_my_rights_as_a_citizen)
sum(is.na(Datos_Justice_Renamed_Filtered$the_rules_respect_my_rights_as_a_citizen))

unique(justice_recoded$the_rules_respect_my_rights_as_a_citizen)
sum(is.na(justice_recoded$the_rules_respect_my_rights_as_a_citizen))




#the_rules_respect_my_rights_as_a_person

#1= Strongly disagree, 2= Disagree, 3= Slightly disagree, 4= Slightly agree , 5= Agree, 6= Strongly agree


justice_recoded <- justice_recoded %>% 
  mutate(the_rules_respect_my_rights_as_a_person=
           recode(the_rules_respect_my_rights_as_a_person, 
                  "strongly disagree" = 1,
                  "Disagree"=2,
                  "Slightly Disagree"=3,
                  "Slightly Agree"=4,
                  "Agree" =5,
                  "strongly agree"=6))


unique(Datos_Justice_Renamed_Filtered$the_rules_respect_my_rights_as_a_person)
sum(is.na(Datos_Justice_Renamed_Filtered$the_rules_respect_my_rights_as_a_person))

unique(justice_recoded$the_rules_respect_my_rights_as_a_person)
sum(is.na(justice_recoded$the_rules_respect_my_rights_as_a_person))



#when_i_talk_about_the_us_society_i_usually_say_we_rather_than_they

#1= Strongly disagree, 2= Disagree, 3= Slightly disagree, 4= Slightly agree , 5= Agree, 6= Strongly agree


justice_recoded <- justice_recoded %>% 
  mutate(when_i_talk_about_the_us_society_i_usually_say_we_rather_than_they=
           recode(when_i_talk_about_the_us_society_i_usually_say_we_rather_than_they, 
                  "strongly disagree" = 1,
                  "Disagree"=2,
                  "Slightly Disagree"=3,
                  "Slightly Agree"=4,
                  "Agree" =5,
                  "strongly agree"=6))


unique(Datos_Justice_Renamed_Filtered$when_i_talk_about_the_us_society_i_usually_say_we_rather_than_they)
sum(is.na(Datos_Justice_Renamed_Filtered$when_i_talk_about_the_us_society_i_usually_say_we_rather_than_they))

unique(justice_recoded$when_i_talk_about_the_us_society_i_usually_say_we_rather_than_they)
sum(is.na(justice_recoded$when_i_talk_about_the_us_society_i_usually_say_we_rather_than_they))



#i_have_a_sense_that_i_personally_belong_in_the_us_society

#1= Strongly disagree, 2= Disagree, 3= Slightly disagree, 4= Slightly agree , 5= Agree, 6= Strongly agree


justice_recoded <- justice_recoded %>% 
  mutate(i_have_a_sense_that_i_personally_belong_in_the_us_society=
           recode(i_have_a_sense_that_i_personally_belong_in_the_us_society, 
                  "strongly disagree" = 1,
                  "Disagree"=2,
                  "Slightly Disagree"=3,
                  "Slightly Agree"=4,
                  "Agree" =5,
                  "strongly agree"=6))


unique(Datos_Justice_Renamed_Filtered$i_have_a_sense_that_i_personally_belong_in_the_us_society)
sum(is.na(Datos_Justice_Renamed_Filtered$i_have_a_sense_that_i_personally_belong_in_the_us_society))

unique(justice_recoded$i_have_a_sense_that_i_personally_belong_in_the_us_society)
sum(is.na(justice_recoded$i_have_a_sense_that_i_personally_belong_in_the_us_society))



#i_feel_like_an_important_part_of_the_us_society

#1= Strongly disagree, 2= Disagree, 3= Slightly disagree, 4= Slightly agree , 5= Agree, 6= Strongly agree


justice_recoded <- justice_recoded %>% 
  mutate(i_feel_like_an_important_part_of_the_us_society=
           recode(i_feel_like_an_important_part_of_the_us_society, 
                  "strongly disagree" = 1,
                  "Disagree"=2,
                  "Slightly Disagree"=3,
                  "Slightly Agree"=4,
                  "Agree" =5,
                  "strongly agree"=6))


unique(Datos_Justice_Renamed_Filtered$i_feel_like_an_important_part_of_the_us_society)
sum(is.na(Datos_Justice_Renamed_Filtered$i_feel_like_an_important_part_of_the_us_society))

unique(justice_recoded$i_feel_like_an_important_part_of_the_us_society)
sum(is.na(justice_recoded$i_feel_like_an_important_part_of_the_us_society))


#i_feel_connected_to_the_people_around_me

#1= Strongly disagree, 2= Disagree, 3= Slightly disagree, 4= Slightly agree , 5= Agree, 6= Strongly agree


justice_recoded <- justice_recoded %>% 
  mutate(i_feel_connected_to_the_people_around_me=
           recode(i_feel_connected_to_the_people_around_me, 
                  "strongly disagree" = 1,
                  "Disagree"=2,
                  "Slightly Disagree"=3,
                  "Slightly Agree"=4,
                  "Agree" =5,
                  "strongly agree"=6))


unique(Datos_Justice_Renamed_Filtered$i_feel_connected_to_the_people_around_me)
sum(is.na(Datos_Justice_Renamed_Filtered$i_feel_connected_to_the_people_around_me))

unique(justice_recoded$i_feel_connected_to_the_people_around_me)
sum(is.na(justice_recoded$i_feel_connected_to_the_people_around_me))




#i_am_close_to_other_people_within_the_us_society_regardless_of_the_community_to_which_they_belong

#1= Strongly disagree, 2= Disagree, 3= Slightly disagree, 4= Slightly agree , 5= Agree, 6= Strongly agree


justice_recoded <- justice_recoded %>% 
  mutate(i_am_close_to_other_people_within_the_us_society_regardless_of_the_community_to_which_they_belong=
           recode(i_am_close_to_other_people_within_the_us_society_regardless_of_the_community_to_which_they_belong, 
                  "strongly disagree" = 1,
                  "Disagree"=2,
                  "Slightly Disagree"=3,
                  "Slightly Agree"=4,
                  "Agree" =5,
                  "strongly agree"=6))


unique(Datos_Justice_Renamed_Filtered$i_am_close_to_other_people_within_the_us_society_regardless_of_the_community_to_which_they_belong)
sum(is.na(Datos_Justice_Renamed_Filtered$i_am_close_to_other_people_within_the_us_society_regardless_of_the_community_to_which_they_belong))

unique(justice_recoded$i_am_close_to_other_people_within_the_us_society_regardless_of_the_community_to_which_they_belong)
sum(is.na(justice_recoded$i_am_close_to_other_people_within_the_us_society_regardless_of_the_community_to_which_they_belong))




#i_am_close_to_other_people_within_the_us_society_regardless_of_the_community_to_which_they_belong

#1= Strongly disagree, 2= Disagree, 3= Slightly disagree, 4= Slightly agree , 5= Agree, 6= Strongly agree


justice_recoded <- justice_recoded %>% 
  mutate(i_am_close_to_other_people_within_the_us_society_regardless_of_the_community_to_which_they_belong=
           recode(i_am_close_to_other_people_within_the_us_society_regardless_of_the_community_to_which_they_belong, 
                  "strongly disagree" = 1,
                  "Disagree"=2,
                  "Slightly Disagree"=3,
                  "Slightly Agree"=4,
                  "Agree" =5,
                  "strongly agree"=6))


unique(Datos_Justice_Renamed_Filtered$i_am_close_to_other_people_within_the_us_society_regardless_of_the_community_to_which_they_belong)
sum(is.na(Datos_Justice_Renamed_Filtered$i_am_close_to_other_people_within_the_us_society_regardless_of_the_community_to_which_they_belong))

unique(justice_recoded$i_am_close_to_other_people_within_the_us_society_regardless_of_the_community_to_which_they_belong)
sum(is.na(justice_recoded$i_am_close_to_other_people_within_the_us_society_regardless_of_the_community_to_which_they_belong))


#others_leave_me_out_of_the_group

#1= Strongly disagree, 2= Disagree, 3= Slightly disagree, 4= Slightly agree , 5= Agree, 6= Strongly agree


justice_recoded <- justice_recoded %>% 
  mutate(others_leave_me_out_of_the_group=
           recode(others_leave_me_out_of_the_group, 
                  "strongly disagree" = 1,
                  "Disagree"=2,
                  "Slightly Disagree"=3,
                  "Slightly Agree"=4,
                  "Agree" =5,
                  "strongly agree"=6))


unique(Datos_Justice_Renamed_Filtered$others_leave_me_out_of_the_group)
sum(is.na(Datos_Justice_Renamed_Filtered$others_leave_me_out_of_the_group))

unique(justice_recoded$others_leave_me_out_of_the_group)
sum(is.na(justice_recoded$others_leave_me_out_of_the_group))



#others_leave_me_out_of_the_group

#1= Strongly disagree, 2= Disagree, 3= Slightly disagree, 4= Slightly agree , 5= Agree, 6= Strongly agree


justice_recoded <- justice_recoded %>% 
  mutate(others_leave_me_out_of_the_group=
           recode(others_leave_me_out_of_the_group, 
                  "strongly disagree" = 1,
                  "Disagree"=2,
                  "Slightly Disagree"=3,
                  "Slightly Agree"=4,
                  "Agree" =5,
                  "strongly agree"=6))


unique(Datos_Justice_Renamed_Filtered$others_leave_me_out_of_the_group)
sum(is.na(Datos_Justice_Renamed_Filtered$others_leave_me_out_of_the_group))

unique(justice_recoded$others_leave_me_out_of_the_group)
sum(is.na(justice_recoded$others_leave_me_out_of_the_group))


#i_feel_proud_to_be_a_part_of_the_us_society

#1= Strongly disagree, 2= Disagree, 3= Slightly disagree, 4= Slightly agree , 5= Agree, 6= Strongly agree


justice_recoded <- justice_recoded %>% 
  mutate(i_feel_proud_to_be_a_part_of_the_us_society=
           recode(i_feel_proud_to_be_a_part_of_the_us_society, 
                  "strongly disagree" = 1,
                  "Disagree"=2,
                  "Slightly Disagree"=3,
                  "Slightly Agree"=4,
                  "Agree" =5,
                  "strongly agree"=6))


unique(Datos_Justice_Renamed_Filtered$i_feel_proud_to_be_a_part_of_the_us_society)
sum(is.na(Datos_Justice_Renamed_Filtered$i_feel_proud_to_be_a_part_of_the_us_society))

unique(justice_recoded$i_feel_proud_to_be_a_part_of_the_us_society)
sum(is.na(justice_recoded$i_feel_proud_to_be_a_part_of_the_us_society))



#my_community_group_is_highly_respected_within_the_us_society_community_group_american_indian_or_alaska_native_asian_black_or_african_american_latinx_native_hawaiian_or_other_pacific_islander_w

#1= Strongly disagree, 2= Disagree, 3= Slightly disagree, 4= Slightly agree , 5= Agree, 6= Strongly agree


justice_recoded <- justice_recoded %>% 
  mutate(my_community_group_is_highly_respected_within_the_us_society_community_group_american_indian_or_alaska_native_asian_black_or_african_american_latinx_native_hawaiian_or_other_pacific_islander_w=
           recode(my_community_group_is_highly_respected_within_the_us_society_community_group_american_indian_or_alaska_native_asian_black_or_african_american_latinx_native_hawaiian_or_other_pacific_islander_w, 
                  "strongly disagree" =1,
                  "Disagree"=2,
                  "Slightly Disagree"=3,
                  "Slightly Agree"=4,
                  "Agree" =5,
                  "strongly agree"=6))


unique(Datos_Justice_Renamed_Filtered$my_community_group_is_highly_respected_within_the_us_society_community_group_american_indian_or_alaska_native_asian_black_or_african_american_latinx_native_hawaiian_or_other_pacific_islander_w)
sum(is.na(Datos_Justice_Renamed_Filtered$my_community_group_is_highly_respected_within_the_us_society_community_group_american_indian_or_alaska_native_asian_black_or_african_american_latinx_native_hawaiian_or_other_pacific_islander_w))

unique(justice_recoded$my_community_group_is_highly_respected_within_the_us_society_community_group_american_indian_or_alaska_native_asian_black_or_african_american_latinx_native_hawaiian_or_other_pacific_islander_w)
sum(is.na(justice_recoded$my_community_group_is_highly_respected_within_the_us_society_community_group_american_indian_or_alaska_native_asian_black_or_african_american_latinx_native_hawaiian_or_other_pacific_islander_w))




#value_you_as_a_member_and_respect_your_ideas

#1= Strongly disagree, 2= Disagree, 3= Slightly disagree, 4= Slightly agree , 5= Agree, 6= Strongly agree


justice_recoded <- justice_recoded %>% 
  mutate(value_you_as_a_member_and_respect_your_ideas=
           recode(value_you_as_a_member_and_respect_your_ideas, 
                  "Strongly disagree" =1,
                  "Disagree"=2,
                  "Slightly Disagree"=3,
                  "Slightly Agree"=4,
                  "Agree" =5,
                  "strongly agree"=6))



unique(Datos_Justice_Renamed_Filtered$value_you_as_a_member_and_respect_your_ideas)
sum(is.na(Datos_Justice_Renamed_Filtered$value_you_as_a_member_and_respect_your_ideas))

unique(justice_recoded$value_you_as_a_member_and_respect_your_ideas)
sum(is.na(justice_recoded$value_you_as_a_member_and_respect_your_ideas))

#Truco para resolver el problema de codificación con strongly agree
justice_recoded$value_you_as_a_member_and_respect_your_ideas= justice_recoded$value_you_as_a_member_and_respect_your_ideas %>% replace(is.na(.), 1)

unique(justice_recoded$value_you_as_a_member_and_respect_your_ideas)
sum(is.na(justice_recoded$value_you_as_a_member_and_respect_your_ideas))







#value_what_you_contribute_to_society_respect_the_work_you_do

#1= Strongly disagree, 2= Disagree, 3= Slightly disagree, 4= Slightly agree , 5= Agree, 6= Strongly agree


justice_recoded <- justice_recoded %>% 
  mutate(value_what_you_contribute_to_society_respect_the_work_you_do=
           recode(value_what_you_contribute_to_society_respect_the_work_you_do, 
                  "Strongly disagree" =1,
                  "Disagree"=2,
                  "Slightly Disagree"=3,
                  "Slightly Agree"=4,
                  "Agree" =5,
                  "strongly agree"=6))



unique(Datos_Justice_Renamed_Filtered$value_what_you_contribute_to_society_respect_the_work_you_do)
sum(is.na(Datos_Justice_Renamed_Filtered$value_what_you_contribute_to_society_respect_the_work_you_do))

unique(justice_recoded$value_what_you_contribute_to_society_respect_the_work_you_do)
sum(is.na(justice_recoded$value_what_you_contribute_to_society_respect_the_work_you_do))


#Truco para resolver el problema de codificación con strongly agree
justice_recoded$value_what_you_contribute_to_society_respect_the_work_you_do= justice_recoded$value_what_you_contribute_to_society_respect_the_work_you_do %>% replace(is.na(.), 1)

unique(justice_recoded$value_what_you_contribute_to_society_respect_the_work_you_do)
sum(is.na(justice_recoded$value_what_you_contribute_to_society_respect_the_work_you_do))



#appreciate_your_unique_contributions

#1= Strongly disagree, 2= Disagree, 3= Slightly disagree, 4= Slightly agree , 5= Agree, 6= Strongly agree


justice_recoded <- justice_recoded %>% 
  mutate(appreciate_your_unique_contributions=
           recode(appreciate_your_unique_contributions, 
                  "Strongly disagree" =1,
                  "Disagree"=2,
                  "Slightly Disagree"=3,
                  "Slightly Agree"=4,
                  "Agree" =5,
                  "strongly agree"=6))



unique(Datos_Justice_Renamed_Filtered$appreciate_your_unique_contributions)
sum(is.na(Datos_Justice_Renamed_Filtered$appreciate_your_unique_contributions))

unique(justice_recoded$appreciate_your_unique_contributions)
sum(is.na(justice_recoded$appreciate_your_unique_contributions))


#Truco para resolver el problema de codificación con strongly agree
justice_recoded$appreciate_your_unique_contributions= justice_recoded$appreciate_your_unique_contributions %>% replace(is.na(.), 1)

unique(justice_recoded$appreciate_your_unique_contributions)
sum(is.na(justice_recoded$appreciate_your_unique_contributions))





#the_government_is_involved_in_the_murder_of_innocent_citizens_and_or_well_known_public_figures_and_keeps_this_a_secret_the_power_held_by_heads_of_state_is_second_to_that_of_small_unknown_groups

#1= Definitely not true; 2= Not true, 3= probably not true, 4= probably true, 5= true, 6= definitely true.

justice_recoded <- justice_recoded %>% 
  mutate(the_government_is_involved_in_the_murder_of_innocent_citizens_and_or_well_known_public_figures_and_keeps_this_a_secret_the_power_held_by_heads_of_state_is_second_to_that_of_small_unknown_groups=
           recode(the_government_is_involved_in_the_murder_of_innocent_citizens_and_or_well_known_public_figures_and_keeps_this_a_secret_the_power_held_by_heads_of_state_is_second_to_that_of_small_unknown_groups, 
                  "Definitely not true" =1,
                  "Not true" =2,
                  "Probably not true"=3,
                  "Probably true"=4,
                  "True" =5,
                  "Definitely true"=6))



unique(Datos_Justice_Renamed_Filtered$the_government_is_involved_in_the_murder_of_innocent_citizens_and_or_well_known_public_figures_and_keeps_this_a_secret_the_power_held_by_heads_of_state_is_second_to_that_of_small_unknown_groups)
sum(is.na(Datos_Justice_Renamed_Filtered$the_government_is_involved_in_the_murder_of_innocent_citizens_and_or_well_known_public_figures_and_keeps_this_a_secret_the_power_held_by_heads_of_state_is_second_to_that_of_small_unknown_groups))

unique(justice_recoded$the_government_is_involved_in_the_murder_of_innocent_citizens_and_or_well_known_public_figures_and_keeps_this_a_secret_the_power_held_by_heads_of_state_is_second_to_that_of_small_unknown_groups)
sum(is.na(justice_recoded$the_government_is_involved_in_the_murder_of_innocent_citizens_and_or_well_known_public_figures_and_keeps_this_a_secret_the_power_held_by_heads_of_state_is_second_to_that_of_small_unknown_groups))




#secret_organizations_communicate_with_extraterrestrials_but_keep_this_fact_from_the_public

#1= Definitely not true; 2= Not true, 3= probably not true, 4= probably true, 5= true, 6= definitely true.

justice_recoded <- justice_recoded %>% 
  mutate(secret_organizations_communicate_with_extraterrestrials_but_keep_this_fact_from_the_public=
           recode(secret_organizations_communicate_with_extraterrestrials_but_keep_this_fact_from_the_public, 
                  "Definitely not true" =1,
                  "Not true" =2,
                  "Probably not true"=3,
                  "Probably true"=4,
                  "True" =5,
                  "Definitely true"=6))



unique(Datos_Justice_Renamed_Filtered$secret_organizations_communicate_with_extraterrestrials_but_keep_this_fact_from_the_public)
sum(is.na(Datos_Justice_Renamed_Filtered$secret_organizations_communicate_with_extraterrestrials_but_keep_this_fact_from_the_public))

unique(justice_recoded$secret_organizations_communicate_with_extraterrestrials_but_keep_this_fact_from_the_public)
sum(is.na(justice_recoded$secret_organizations_communicate_with_extraterrestrials_but_keep_this_fact_from_the_public))



#the_spread_of_certain_viruses_and_or_diseases_is_the_result_of_the_deliberate_concealed_efforts_of_some_organization_groups_of_scientists_manipulate_fabricate_or_suppress_evidence_in_order_to_d

#1= Definitely not true; 2= Not true, 3= probably not true, 4= probably true, 5= true, 6= definitely true.

justice_recoded <- justice_recoded %>% 
  mutate(the_spread_of_certain_viruses_and_or_diseases_is_the_result_of_the_deliberate_concealed_efforts_of_some_organization_groups_of_scientists_manipulate_fabricate_or_suppress_evidence_in_order_to_d=
           recode(the_spread_of_certain_viruses_and_or_diseases_is_the_result_of_the_deliberate_concealed_efforts_of_some_organization_groups_of_scientists_manipulate_fabricate_or_suppress_evidence_in_order_to_d, 
                  "Definitely not true" =1,
                  "Not true" =2,
                  "Probably not true"=3,
                  "Probably true"=4,
                  "True" =5,
                  "Definitely true"=6))



unique(Datos_Justice_Renamed_Filtered$the_spread_of_certain_viruses_and_or_diseases_is_the_result_of_the_deliberate_concealed_efforts_of_some_organization_groups_of_scientists_manipulate_fabricate_or_suppress_evidence_in_order_to_d)
sum(is.na(Datos_Justice_Renamed_Filtered$the_spread_of_certain_viruses_and_or_diseases_is_the_result_of_the_deliberate_concealed_efforts_of_some_organization_groups_of_scientists_manipulate_fabricate_or_suppress_evidence_in_order_to_d))

unique(justice_recoded$the_spread_of_certain_viruses_and_or_diseases_is_the_result_of_the_deliberate_concealed_efforts_of_some_organization_groups_of_scientists_manipulate_fabricate_or_suppress_evidence_in_order_to_d)
sum(is.na(justice_recoded$the_spread_of_certain_viruses_and_or_diseases_is_the_result_of_the_deliberate_concealed_efforts_of_some_organization_groups_of_scientists_manipulate_fabricate_or_suppress_evidence_in_order_to_d))




#the_government_permits_or_perpetrates_acts_of_terrorism_on_its_own_soil_disguising_its_involvement

#1= Definitely not true; 2= Not true, 3= probably not true, 4= probably true, 5= true, 6= definitely true.

justice_recoded <- justice_recoded %>% 
  mutate(the_government_permits_or_perpetrates_acts_of_terrorism_on_its_own_soil_disguising_its_involvement=
           recode(the_government_permits_or_perpetrates_acts_of_terrorism_on_its_own_soil_disguising_its_involvement, 
                  "Definitely not true" =1,
                  "Not true" =2,
                  "Probably not true"=3,
                  "Probably true"=4,
                  "True" =5,
                  "Definitely true"=6))



unique(Datos_Justice_Renamed_Filtered$the_government_permits_or_perpetrates_acts_of_terrorism_on_its_own_soil_disguising_its_involvement)
sum(is.na(Datos_Justice_Renamed_Filtered$the_government_permits_or_perpetrates_acts_of_terrorism_on_its_own_soil_disguising_its_involvement))

unique(justice_recoded$the_government_permits_or_perpetrates_acts_of_terrorism_on_its_own_soil_disguising_its_involvement)
sum(is.na(justice_recoded$the_government_permits_or_perpetrates_acts_of_terrorism_on_its_own_soil_disguising_its_involvement))





#a_small_secret_group_of_people_is_responsible_for_making_all_major_world_decisions_such_as_going_to_war

#1= Definitely not true; 2= Not true, 3= probably not true, 4= probably true, 5= true, 6= definitely true.

justice_recoded <- justice_recoded %>% 
  mutate(a_small_secret_group_of_people_is_responsible_for_making_all_major_world_decisions_such_as_going_to_war=
           recode(a_small_secret_group_of_people_is_responsible_for_making_all_major_world_decisions_such_as_going_to_war, 
                  "Definitely not true" =1,
                  "Not true" =2,
                  "Probably not true"=3,
                  "Probably true"=4,
                  "True" =5,
                  "Definitely true"=6))



unique(Datos_Justice_Renamed_Filtered$a_small_secret_group_of_people_is_responsible_for_making_all_major_world_decisions_such_as_going_to_war)
sum(is.na(Datos_Justice_Renamed_Filtered$a_small_secret_group_of_people_is_responsible_for_making_all_major_world_decisions_such_as_going_to_war))

unique(justice_recoded$a_small_secret_group_of_people_is_responsible_for_making_all_major_world_decisions_such_as_going_to_war)
sum(is.na(justice_recoded$a_small_secret_group_of_people_is_responsible_for_making_all_major_world_decisions_such_as_going_to_war))




#evidence_of_alien_contact_is_being_concealed_from_the_public

#1= Definitely not true; 2= Not true, 3= probably not true, 4= probably true, 5= true, 6= definitely true.

justice_recoded <- justice_recoded %>% 
  mutate(evidence_of_alien_contact_is_being_concealed_from_the_public=
           recode(evidence_of_alien_contact_is_being_concealed_from_the_public, 
                  "Definitely not true" =1,
                  "Not true" =2,
                  "Probably not true"=3,
                  "Probably true"=4,
                  "True" =5,
                  "Definitely true"=6))



unique(Datos_Justice_Renamed_Filtered$evidence_of_alien_contact_is_being_concealed_from_the_public)
sum(is.na(Datos_Justice_Renamed_Filtered$evidence_of_alien_contact_is_being_concealed_from_the_public))

unique(justice_recoded$evidence_of_alien_contact_is_being_concealed_from_the_public)
sum(is.na(justice_recoded$evidence_of_alien_contact_is_being_concealed_from_the_public))





#technology_with_mind_control_capacities_is_used_on_people_without_their_knowledge

#1= Definitely not true; 2= Not true, 3= probably not true, 4= probably true, 5= true, 6= definitely true.

justice_recoded <- justice_recoded %>% 
  mutate(technology_with_mind_control_capacities_is_used_on_people_without_their_knowledge=
           recode(technology_with_mind_control_capacities_is_used_on_people_without_their_knowledge, 
                  "Definitely not true" =1,
                  "Not true" =2,
                  "Probably not true"=3,
                  "Probably true"=4,
                  "True" =5,
                  "Definitely true"=6))



unique(Datos_Justice_Renamed_Filtered$technology_with_mind_control_capacities_is_used_on_people_without_their_knowledge)
sum(is.na(Datos_Justice_Renamed_Filtered$technology_with_mind_control_capacities_is_used_on_people_without_their_knowledge))

unique(justice_recoded$technology_with_mind_control_capacities_is_used_on_people_without_their_knowledge)
sum(is.na(justice_recoded$technology_with_mind_control_capacities_is_used_on_people_without_their_knowledge))




#new_and_advanced_technology_which_would_harm_current_industry_is_being_suppressed

#1= Definitely not true; 2= Not true, 3= probably not true, 4= probably true, 5= true, 6= definitely true.

justice_recoded <- justice_recoded %>% 
  mutate(new_and_advanced_technology_which_would_harm_current_industry_is_being_suppressed=
           recode(new_and_advanced_technology_which_would_harm_current_industry_is_being_suppressed, 
                  "Definitely not true" =1,
                  "Not true" =2,
                  "Probably not true"=3,
                  "Probably true"=4,
                  "True" =5,
                  "Definitely true"=6))



unique(Datos_Justice_Renamed_Filtered$new_and_advanced_technology_which_would_harm_current_industry_is_being_suppressed)
sum(is.na(Datos_Justice_Renamed_Filtered$new_and_advanced_technology_which_would_harm_current_industry_is_being_suppressed))

unique(justice_recoded$new_and_advanced_technology_which_would_harm_current_industry_is_being_suppressed)
sum(is.na(justice_recoded$new_and_advanced_technology_which_would_harm_current_industry_is_being_suppressed))




#the_government_uses_people_as_patsies_to_hide_its_involvement_in_criminal_activity

#1= Definitely not true; 2= Not true, 3= probably not true, 4= probably true, 5= true, 6= definitely true.

justice_recoded <- justice_recoded %>% 
  mutate(the_government_uses_people_as_patsies_to_hide_its_involvement_in_criminal_activity=
           recode(the_government_uses_people_as_patsies_to_hide_its_involvement_in_criminal_activity, 
                  "Definitely not true" =1,
                  "Not true" =2,
                  "Probably not true"=3,
                  "Probably true"=4,
                  "True" =5,
                  "Definitely true"=6))



unique(Datos_Justice_Renamed_Filtered$the_government_uses_people_as_patsies_to_hide_its_involvement_in_criminal_activity)
sum(is.na(Datos_Justice_Renamed_Filtered$the_government_uses_people_as_patsies_to_hide_its_involvement_in_criminal_activity))

unique(justice_recoded$the_government_uses_people_as_patsies_to_hide_its_involvement_in_criminal_activity)
sum(is.na(justice_recoded$the_government_uses_people_as_patsies_to_hide_its_involvement_in_criminal_activity))




#certain_significant_events_have_been_the_result_of_the_activity_of_a_small_group_who_secretly_manipulate_world_events

#1= Definitely not true; 2= Not true, 3= probably not true, 4= probably true, 5= true, 6= definitely true.

justice_recoded <- justice_recoded %>% 
  mutate(certain_significant_events_have_been_the_result_of_the_activity_of_a_small_group_who_secretly_manipulate_world_events=
           recode(certain_significant_events_have_been_the_result_of_the_activity_of_a_small_group_who_secretly_manipulate_world_events, 
                  "Definitely not true" =1,
                  "Not true" =2,
                  "Probably not true"=3,
                  "Probably true"=4,
                  "True" =5,
                  "Definitely true"=6))



unique(Datos_Justice_Renamed_Filtered$certain_significant_events_have_been_the_result_of_the_activity_of_a_small_group_who_secretly_manipulate_world_events)
sum(is.na(Datos_Justice_Renamed_Filtered$certain_significant_events_have_been_the_result_of_the_activity_of_a_small_group_who_secretly_manipulate_world_events))

unique(justice_recoded$certain_significant_events_have_been_the_result_of_the_activity_of_a_small_group_who_secretly_manipulate_world_events)
sum(is.na(justice_recoded$certain_significant_events_have_been_the_result_of_the_activity_of_a_small_group_who_secretly_manipulate_world_events))




#some_ufo_sightings_and_rumors_are_planned_or_staged_in_order_to_distract_the_public_from_real_alien_contact

#1= Definitely not true; 2= Not true, 3= probably not true, 4= probably true, 5= true, 6= definitely true.

justice_recoded <- justice_recoded %>% 
  mutate(some_ufo_sightings_and_rumors_are_planned_or_staged_in_order_to_distract_the_public_from_real_alien_contact=
           recode(some_ufo_sightings_and_rumors_are_planned_or_staged_in_order_to_distract_the_public_from_real_alien_contact, 
                  "Definitely not true" =1,
                  "Not true" =2,
                  "Probably not true"=3,
                  "Probably true"=4,
                  "True" =5,
                  "Definitely true"=6))



unique(Datos_Justice_Renamed_Filtered$some_ufo_sightings_and_rumors_are_planned_or_staged_in_order_to_distract_the_public_from_real_alien_contact)
sum(is.na(Datos_Justice_Renamed_Filtered$some_ufo_sightings_and_rumors_are_planned_or_staged_in_order_to_distract_the_public_from_real_alien_contact))

unique(justice_recoded$some_ufo_sightings_and_rumors_are_planned_or_staged_in_order_to_distract_the_public_from_real_alien_contact)
sum(is.na(justice_recoded$some_ufo_sightings_and_rumors_are_planned_or_staged_in_order_to_distract_the_public_from_real_alien_contact))



#experiments_involving_new_drugs_or_technologies_are_routinely_carried_out_on_the_public_without_their_knowledge_or_consent_a_lot_of_important_information_is_deliberately_concealed_from_the_public

#1= Definitely not true; 2= Not true, 3= probably not true, 4= probably true, 5= true, 6= definitely true.

justice_recoded <- justice_recoded %>% 
  mutate(experiments_involving_new_drugs_or_technologies_are_routinely_carried_out_on_the_public_without_their_knowledge_or_consent_a_lot_of_important_information_is_deliberately_concealed_from_the_public=
           recode(experiments_involving_new_drugs_or_technologies_are_routinely_carried_out_on_the_public_without_their_knowledge_or_consent_a_lot_of_important_information_is_deliberately_concealed_from_the_public, 
                  "Definitely not true" =1,
                  "Not true" =2,
                  "Probably not true"=3,
                  "Probably true"=4,
                  "True" =5,
                  "Definitely true"=6))



unique(Datos_Justice_Renamed_Filtered$experiments_involving_new_drugs_or_technologies_are_routinely_carried_out_on_the_public_without_their_knowledge_or_consent_a_lot_of_important_information_is_deliberately_concealed_from_the_public)
sum(is.na(Datos_Justice_Renamed_Filtered$experiments_involving_new_drugs_or_technologies_are_routinely_carried_out_on_the_public_without_their_knowledge_or_consent_a_lot_of_important_information_is_deliberately_concealed_from_the_public))

unique(justice_recoded$experiments_involving_new_drugs_or_technologies_are_routinely_carried_out_on_the_public_without_their_knowledge_or_consent_a_lot_of_important_information_is_deliberately_concealed_from_the_public)
sum(is.na(justice_recoded$experiments_involving_new_drugs_or_technologies_are_routinely_carried_out_on_the_public_without_their_knowledge_or_consent_a_lot_of_important_information_is_deliberately_concealed_from_the_public))



#i_only_read_if_i_have_to

#1= Totally disagree, 2= Disagree, 3= Disagree somewhat, 4= Agree somewhat, 5= Agree, 6= Totally agree

justice_recoded <- justice_recoded %>% 
  mutate(i_only_read_if_i_have_to=
           recode(i_only_read_if_i_have_to, 
                  "Totally disagree" =1,
                  "Disagree" =2,
                  "Disagree somewhat"=3,
                  "agree somewhat"=4,
                  "agree" =5,
                  "Totally agree"=6))



unique(Datos_Justice_Renamed_Filtered$i_only_read_if_i_have_to)
sum(is.na(Datos_Justice_Renamed_Filtered$i_only_read_if_i_have_to))

unique(justice_recoded$i_only_read_if_i_have_to)
sum(is.na(justice_recoded$i_only_read_if_i_have_to))



#reading_is_one_of_my_favorite_pastimes

#1= Totally disagree, 2= Disagree, 3= Disagree somewhat, 4= Agree somewhat, 5= Agree, 6= Totally agree

justice_recoded <- justice_recoded %>% 
  mutate(reading_is_one_of_my_favorite_pastimes=
           recode(reading_is_one_of_my_favorite_pastimes, 
                  "Totally disagree" =1,
                  "Disagree" =2,
                  "Disagree somewhat"=3,
                  "agree somewhat"=4,
                  "agree" =5,
                  "Totally agree"=6))



unique(Datos_Justice_Renamed_Filtered$reading_is_one_of_my_favorite_pastimes)
sum(is.na(Datos_Justice_Renamed_Filtered$reading_is_one_of_my_favorite_pastimes))

unique(justice_recoded$reading_is_one_of_my_favorite_pastimes)
sum(is.na(justice_recoded$reading_is_one_of_my_favorite_pastimes))



#i_find_it_hard_to_finish_a_book

#1= Totally disagree, 2= Disagree, 3= Disagree somewhat, 4= Agree somewhat, 5= Agree, 6= Totally agree

justice_recoded <- justice_recoded %>% 
  mutate(i_find_it_hard_to_finish_a_book=
           recode(i_find_it_hard_to_finish_a_book, 
                  "Totally disagree" =1,
                  "Disagree" =2,
                  "Disagree somewhat"=3,
                  "agree somewhat"=4,
                  "agree" =5,
                  "Totally agree"=6))



unique(Datos_Justice_Renamed_Filtered$i_find_it_hard_to_finish_a_book)
sum(is.na(Datos_Justice_Renamed_Filtered$i_find_it_hard_to_finish_a_book))

unique(justice_recoded$i_find_it_hard_to_finish_a_book)
sum(is.na(justice_recoded$i_find_it_hard_to_finish_a_book))




#for_me_reading_for_pleasure_is_a_waste_of_time

#1= Totally disagree, 2= Disagree, 3= Disagree somewhat, 4= Agree somewhat, 5= Agree, 6= Totally agree

justice_recoded <- justice_recoded %>% 
  mutate(for_me_reading_for_pleasure_is_a_waste_of_time=
           recode(for_me_reading_for_pleasure_is_a_waste_of_time, 
                  "Totally disagree" =1,
                  "Disagree" =2,
                  "Disagree somewhat"=3,
                  "agree somewhat"=4,
                  "agree" =5,
                  "Totally agree"=6))



unique(Datos_Justice_Renamed_Filtered$for_me_reading_for_pleasure_is_a_waste_of_time)
sum(is.na(Datos_Justice_Renamed_Filtered$for_me_reading_for_pleasure_is_a_waste_of_time))

unique(justice_recoded$for_me_reading_for_pleasure_is_a_waste_of_time)
sum(is.na(justice_recoded$for_me_reading_for_pleasure_is_a_waste_of_time))





#i_really_enjoy_going_to_a_bookstore_or_a_library

#1= Totally disagree, 2= Disagree, 3= Disagree somewhat, 4= Agree somewhat, 5= Agree, 6= Totally agree

justice_recoded <- justice_recoded %>% 
  mutate(i_really_enjoy_going_to_a_bookstore_or_a_library=
           recode(i_really_enjoy_going_to_a_bookstore_or_a_library, 
                  "Totally disagree" =1,
                  "Disagree" =2,
                  "Disagree somewhat"=3,
                  "agree somewhat"=4,
                  "agree" =5,
                  "Totally agree"=6))



unique(Datos_Justice_Renamed_Filtered$i_really_enjoy_going_to_a_bookstore_or_a_library)
sum(is.na(Datos_Justice_Renamed_Filtered$i_really_enjoy_going_to_a_bookstore_or_a_library))

unique(justice_recoded$i_really_enjoy_going_to_a_bookstore_or_a_library)
sum(is.na(justice_recoded$i_really_enjoy_going_to_a_bookstore_or_a_library))



#as_soon_as_i_finish_a_book_i_already_think_about_the_one_im_going_to_start_next

#1= Totally disagree, 2= Disagree, 3= Disagree somewhat, 4= Agree somewhat, 5= Agree, 6= Totally agree

justice_recoded <- justice_recoded %>% 
  mutate(as_soon_as_i_finish_a_book_i_already_think_about_the_one_im_going_to_start_next=
           recode(as_soon_as_i_finish_a_book_i_already_think_about_the_one_im_going_to_start_next, 
                  "Totally disagree" =1,
                  "Disagree" =2,
                  "Disagree somewhat"=3,
                  "agree somewhat"=4,
                  "agree" =5,
                  "Totally agree"=6))



unique(Datos_Justice_Renamed_Filtered$as_soon_as_i_finish_a_book_i_already_think_about_the_one_im_going_to_start_next)
sum(is.na(Datos_Justice_Renamed_Filtered$as_soon_as_i_finish_a_book_i_already_think_about_the_one_im_going_to_start_next))

unique(justice_recoded$as_soon_as_i_finish_a_book_i_already_think_about_the_one_im_going_to_start_next)
sum(is.na(justice_recoded$as_soon_as_i_finish_a_book_i_already_think_about_the_one_im_going_to_start_next))



#as_soon_as_i_finish_a_book_i_already_think_about_the_one_im_going_to_start_next

#1= Totally disagree, 2= Disagree, 3= Disagree somewhat, 4= Agree somewhat, 5= Agree, 6= Totally agree

justice_recoded <- justice_recoded %>% 
  mutate(as_soon_as_i_finish_a_book_i_already_think_about_the_one_im_going_to_start_next=
           recode(as_soon_as_i_finish_a_book_i_already_think_about_the_one_im_going_to_start_next, 
                  "Totally disagree" =1,
                  "Disagree" =2,
                  "Disagree somewhat"=3,
                  "agree somewhat"=4,
                  "agree" =5,
                  "Totally agree"=6))



unique(Datos_Justice_Renamed_Filtered$as_soon_as_i_finish_a_book_i_already_think_about_the_one_im_going_to_start_next)
sum(is.na(Datos_Justice_Renamed_Filtered$as_soon_as_i_finish_a_book_i_already_think_about_the_one_im_going_to_start_next))

unique(justice_recoded$as_soon_as_i_finish_a_book_i_already_think_about_the_one_im_going_to_start_next)
sum(is.na(justice_recoded$as_soon_as_i_finish_a_book_i_already_think_about_the_one_im_going_to_start_next))



View(justice_recoded)




# Merging indicators into single variables --------------------------------



#procedural_justice

justice_recoded_merged <- mutate(justice_recoded, index_procedural_justice = rowMeans(select(justice_recoded,
                                                                                    the_rules_dictate_that_decisions_should_be_fair_and_unbiased:
                                                                                      the_rules_respect_my_rights_as_a_person), na.rm = TRUE))

#identity_judgements

justice_recoded_merged <- mutate(justice_recoded_merged, 
                          index_identity_judgements = 
                          rowMeans(select(justice_recoded,
                           when_i_talk_about_the_us_society_i_usually_say_we_rather_than_they:
                             appreciate_your_unique_contributions), 
                          na.rm = TRUE))



#conspiracy_belief

justice_recoded_merged <- mutate(justice_recoded_merged, 
                          index_conspiracy_belief = 
                            rowMeans(select(justice_recoded,
                            the_government_is_involved_in_the_murder_of_innocent_citizens_and_or_well_known_public_figures_and_keeps_this_a_secret_the_power_held_by_heads_of_state_is_second_to_that_of_small_unknown_groups:
                            experiments_involving_new_drugs_or_technologies_are_routinely_carried_out_on_the_public_without_their_knowledge_or_consent_a_lot_of_important_information_is_deliberately_concealed_from_the_public), 
                                     na.rm = TRUE))


#socio_economic_status


justice_recoded_merged$socio_economic_status <- justice_recoded_merged$what_socio_economic_level_do_you_identify_with


#education_level

justice_recoded_merged$education_level<-justice_recoded_merged$highest_degree_finished



#literacy_level

justice_recoded_merged <- mutate(justice_recoded_merged, 
                                 index_literacy_level = 
                                   rowMeans(select(justice_recoded,
                                              i_only_read_if_i_have_to:
                                              as_soon_as_i_finish_a_book_i_already_think_about_the_one_im_going_to_start_next), 
                                            na.rm = TRUE))



##Group (minority/majority / 5)
justice_recoded_merged$group<-justice_recoded_merged$describe_yourself


#(Political orientation /10)
justice_recoded_merged$dup_political_orientation<-justice_recoded_merged$political_orientation


View(justice_recoded_merged)

# #Final data set and exports -------------------------------------------------------

#Final data set
def_justice<- justice_recoded_merged %>% 
  select(index_procedural_justice:dup_political_orientation)


#export Datos_Justice_Renamed_Filtered
write.csv(Datos_Justice_Renamed_Filtered,"Bases_CSV/Datos_Justice_Renamed_Filtered.csv", row.names = FALSE)

#export justice_recoded_merged

write.csv(justice_recoded_merged,"Bases_CSV/justice_recoded_merged.csv", row.names = FALSE)

#export def_justice

write.csv(def_justice,"Bases_CSV/def_justice.csv", row.names = FALSE)



# Summary statistics and Corplots -----------------------------------------


stargazer(as.data.frame(def_justice),
          type = "text",
          median = TRUE,
          title = "Summary Statistics",
          out="./Summary_Tables/Summary_Statistics.html")

#Corplot (ggpairs)

forcorplot_def_justice <- def_justice %>% 
  select(index_procedural_justice,
     index_identity_judgements,
         index_conspiracy_belief,
         index_literacy_level,
         dup_political_orientation) 


ggpairs(forcorplot_def_justice,             
        columns = 1:5)

#Export

plot1<- ggpairs(forcorplot_def_justice,             
        columns = 1:5)

ggsave(plot = plot1, filename = "./Correlation_Plots/Corplot_1.png", 
       width = 11, 
       height = 7,
       type = "cairo",
       dpi = "retina")


#Corplot (chart.Correlation)

chart.Correlation(forcorplot_def_justice)


png("./Correlation_Plots/Corplot_2.png",
    width = 2000, height = 1800,type = "cairo",res=300)

chart.Correlation(forcorplot_def_justice, histogram=TRUE, pch=19,
                  method = "pearson")
dev.off()

View(forcorplot_def_justice)


#Corplot

###Corplot 4
corrplot.mixed(cor(forcorplot_def_justice,use = "pairwise.complete.obs"),
               lower = "circle", 
               upper = "number",
               tl.col = "black",
               title="Correlaciones Well_Being") 

#Cormatrix


Correlation_Matrix <-forcorplot_def_justice %>% 
  cor(use = "pairwise.complete.obs")

stargazer(Correlation_Matrix,
          title="Correlation Matrix",
          type = "text",
          out = "./Correlation_Matrices_html/Correlation_Matrix.html")



# Histograms --------------------------------------------------------------

#Conspiracy beliefs

def_justice %>% 
  ggplot(aes(x=index_conspiracy_belief))+
  geom_histogram(fill="#9b19f5")+
  geom_density(aes(y=..count..*0.2), alpha=.1, fill="#FF6666")+ 
  labs(title="Conspiracy beliefs", 
       x="Conspiracy beliefs index",
       y = "Count",
       fill="Type")+
theme_classic()+
  theme(axis.text.x = element_text( size = 13, angle = 0,hjust = 0.5,vjust = 0.5),
        axis.title.y = element_text( size = 15),
        axis.title.x = element_text( size = 15),
        plot.title = element_text(size = 20,face = "bold"),
        plot.subtitle = element_text(size = 15),
        legend.title = element_text(size = 15),
        legend.text = element_text(size = 13))


      #Export
      
      histogram_1<- def_justice %>% 
        ggplot(aes(x=index_conspiracy_belief))+
        geom_histogram(fill="#9b19f5")+
        geom_density(aes(y=..count..*0.2), alpha=.1, fill="#FF6666")+ 
        labs(title="Conspiracy beliefs", 
             x="Conspiracy beliefs index",
             y = "Count",
             fill="Type")+
        theme_classic()+
        theme(axis.text.x = element_text( size = 13, angle = 0,hjust = 0.5,vjust = 0.5),
              axis.title.y = element_text( size = 15),
              axis.title.x = element_text( size = 15),
              plot.title = element_text(size = 20,face = "bold"),
              plot.subtitle = element_text(size = 15),
              legend.title = element_text(size = 15),
              legend.text = element_text(size = 13))
      
      ggsave(plot = histogram_1, 
             filename = "./Graficas_Definitivas/index_conspiracy_beliefs.png", 
             width = 11, height = 7,
             type= "cairo",
             dpi = "retina")
      
      


#procedural Justice

def_justice %>% 
  ggplot(aes(x=index_procedural_justice))+
  geom_histogram(fill="#e60049")+
  geom_density(aes(y=..count..*0.2), alpha=.1, fill="#FF6666")+ 
  labs(title="Procedural justice", 
       x="procedural Justice index",
       y = "Count",
       fill="Type")+
  theme_classic()+
  theme(axis.text.x = element_text( size = 13, angle = 0,hjust = 0.5,vjust = 0.5),
        axis.title.y = element_text( size = 15),
        axis.title.x = element_text( size = 15),
        plot.title = element_text(size = 20,face = "bold"),
        plot.subtitle = element_text(size = 15),
        legend.title = element_text(size = 15),
        legend.text = element_text(size = 13))
        

      #Export
      
      histogram_2<- def_justice %>% 
        ggplot(aes(x=index_procedural_justice))+
        geom_histogram(fill="#e60049")+
        geom_density(aes(y=..count..*0.2), alpha=.1, fill="#FF6666")+ 
        labs(title="procedural justice", 
             x="procedural Justice index",
             y = "Count",
             fill="Type")+
        theme_classic()+
        theme(axis.text.x = element_text( size = 13, angle = 0,hjust = 0.5,vjust = 0.5),
              axis.title.y = element_text( size = 15),
              axis.title.x = element_text( size = 15),
              plot.title = element_text(size = 20,face = "bold"),
              plot.subtitle = element_text(size = 15),
              legend.title = element_text(size = 15),
              legend.text = element_text(size = 13))
      
      ggsave(plot = histogram_2, 
             filename = "./Graficas_Definitivas/index_procedural_justice.png", 
             width = 11, height = 7,
             type= "cairo",
             dpi = "retina")
      
            
  
#Identity Judgments
  

def_justice %>% 
  ggplot(aes(x=index_identity_judgements))+
  geom_histogram(fill="#e60049")+
  geom_density(aes(y=..count..*0.2), alpha=.1, fill="#FF6666")+ 
  labs(title="procedural justice", 
       x="Identity Judgments",
       y = "Count",
       fill="Type")+
  theme_classic()+
  theme(axis.text.x = element_text( size = 13, angle = 0,hjust = 0.5,vjust = 0.5),
        axis.title.y = element_text( size = 15),
        axis.title.x = element_text( size = 15),
        plot.title = element_text(size = 20,face = "bold"),
        plot.subtitle = element_text(size = 15),
        legend.title = element_text(size = 15),
        legend.text = element_text(size = 13))
      
      #Export
      
      histogram_3<- def_justice %>% 
        ggplot(aes(x=index_identity_judgements))+
        geom_histogram(fill="#e60049")+
        geom_density(aes(y=..count..*0.2), alpha=.1, fill="#FF6666")+ 
        labs(title="procedural justice", 
             x="Identity Judgments",
             y = "Count",
             fill="Type")+
        theme_classic()+
        theme(axis.text.x = element_text( size = 13, angle = 0,hjust = 0.5,vjust = 0.5),
              axis.title.y = element_text( size = 15),
              axis.title.x = element_text( size = 15),
              plot.title = element_text(size = 20,face = "bold"),
              plot.subtitle = element_text(size = 15),
              legend.title = element_text(size = 15),
              legend.text = element_text(size = 13))
      
      ggsave(plot = histogram_3, 
             filename = "./Graficas_Definitivas/index_identity_judgements.png", 
             width = 11, height = 7,
             type= "cairo",
             dpi = "retina")



#Literacy Level 


def_justice %>% 
  ggplot(aes(x=index_literacy_level))+
  geom_histogram(fill="#e60049")+
  geom_density(aes(y=..count..*0.2), alpha=.1, fill="#FF6666")+ 
  labs(title="Literacy level", 
       x="Literacy level",
       y = "Count",
       fill="Type")+
  theme_classic()+
  theme(axis.text.x = element_text( size = 13, angle = 0,hjust = 0.5,vjust = 0.5),
        axis.title.y = element_text( size = 15),
        axis.title.x = element_text( size = 15),
        plot.title = element_text(size = 20,face = "bold"),
        plot.subtitle = element_text(size = 15),
        legend.title = element_text(size = 15),
        legend.text = element_text(size = 13))

      #Export
      
      histogram_4<-def_justice %>% 
        ggplot(aes(x=index_literacy_level))+
        geom_histogram(fill="#e60049")+
        geom_density(aes(y=..count..*0.2), alpha=.1, fill="#FF6666")+ 
        labs(title="Literacy level", 
             x="Literacy level",
             y = "Count",
             fill="Type")+
        theme_classic()+
        theme(axis.text.x = element_text( size = 13, angle = 0,hjust = 0.5,vjust = 0.5),
              axis.title.y = element_text( size = 15),
              axis.title.x = element_text( size = 15),
              plot.title = element_text(size = 20,face = "bold"),
              plot.subtitle = element_text(size = 15),
              legend.title = element_text(size = 15),
              legend.text = element_text(size = 13))
      
      ggsave(plot = histogram_4, 
             filename = "./Graficas_Definitivas/index_literacy_level.png", 
             width = 11, height = 7,
             type= "cairo",
             dpi = "retina")


# Barplots ----------------------------------------------------------------

  #Socio economic status
  
  def_justice %>% 
    ggplot(aes(x=socio_economic_status))+
    geom_bar(fill="#0bb4ff")+
    labs(title="Socio economic status", 
         x="Education level",
         y = "Count",
         fill="Type")+
    theme_classic()+
    theme(axis.text.x = element_text( size = 13, angle = 45,hjust = 0.5,vjust = 0.5),
          axis.title.y = element_text( size = 15),
          axis.title.x = element_text( size = 15),
          plot.title = element_text(size = 20,face = "bold"),
          plot.subtitle = element_text(size = 15),
          legend.title = element_text(size = 15),
          legend.text = element_text(size = 13))
  
  

      #Export
        
      barplot_1<- def_justice %>% 
          ggplot(aes(x=socio_economic_status))+
          geom_bar(fill="#0bb4ff")+
          labs(title="Socio economic status", 
               x="Education level",
               y = "Count",
               fill="Type")+
          theme_classic()+
          theme(axis.text.x = element_text( size = 13, angle = 45,hjust = 0.5,vjust = 0.5),
                axis.title.y = element_text( size = 15),
                axis.title.x = element_text( size = 15),
                plot.title = element_text(size = 20,face = "bold"),
                plot.subtitle = element_text(size = 15),
                legend.title = element_text(size = 15),
                legend.text = element_text(size = 13))
        
        ggsave(plot = barplot_1, 
               filename = "./Graficas_Definitivas/socio_economic_status.png", 
               width = 11, height = 7,
               type= "cairo",
               dpi = "retina")

        
        

#Education Level
  
  def_justice %>% 
    ggplot(aes(x=education_level))+
    geom_bar(fill="#0bb4ff")+
    labs(title="Education level", 
         x="Education level",
         y = "Count",
         fill="Type")+
  theme_classic()+
    theme(axis.text.x = element_text( size = 13, angle = 45,hjust = 0.5,vjust = 0.5),
          axis.title.y = element_text( size = 15),
          axis.title.x = element_text( size = 15),
          plot.title = element_text(size = 20,face = "bold"),
          plot.subtitle = element_text(size = 15),
          legend.title = element_text(size = 15),
          legend.text = element_text(size = 13))
  
  
  #Export
  
  barplot_2<- def_justice %>% 
    ggplot(aes(x=education_level))+
    geom_bar(fill="#0bb4ff")+
    labs(title="Education level", 
         x="Education level",
         y = "Count",
         fill="Type")+
    theme_classic()+
    theme(axis.text.x = element_text( size = 13, angle = 45,hjust = 0.5,vjust = 0.5),
          axis.title.y = element_text( size = 15),
          axis.title.x = element_text( size = 15),
          plot.title = element_text(size = 20,face = "bold"),
          plot.subtitle = element_text(size = 15),
          legend.title = element_text(size = 15),
          legend.text = element_text(size = 13))
  
  
  ggsave(plot = barplot_2, 
         filename = "./Graficas_Definitivas/education_level.png", 
         width = 11, height = 7,
         type= "cairo",
         dpi = "retina")

  
  
  
  
#Group
  
  def_justice %>% 
    ggplot(aes(x=group))+
    geom_bar(fill="#0bb4ff")+
    labs(title="Group", 
         x="Group",
         y = "Count",
         fill="Type")+
    theme_classic()+
    theme(axis.text.x = element_text( size = 13, angle = 45,hjust = 0.5,vjust = 0.5),
          axis.title.y = element_text( size = 15),
          axis.title.x = element_text( size = 15),
          plot.title = element_text(size = 20,face = "bold"),
          plot.subtitle = element_text(size = 15),
          legend.title = element_text(size = 15),
          legend.text = element_text(size = 13))
  
  #Export
  
  barplot_3<-def_justice %>% 
    ggplot(aes(x=group))+
    geom_bar(fill="#0bb4ff")+
    labs(title="Group", 
         x="Group",
         y = "Count",
         fill="Type")+
    theme_classic()+
    theme(axis.text.x = element_text( size = 13, angle = 45,hjust = 0.5,vjust = 0.5),
          axis.title.y = element_text( size = 15),
          axis.title.x = element_text( size = 15),
          plot.title = element_text(size = 20,face = "bold"),
          plot.subtitle = element_text(size = 15),
          legend.title = element_text(size = 15),
          legend.text = element_text(size = 13))
  
  
  ggsave(plot = barplot_3, 
         filename = "./Graficas_Definitivas/group.png", 
         width = 11, height = 7,
         type= "cairo",
         dpi = "retina")
  
  
  
  
  
  
  #Political Orientation
  
  def_justice %>% 
    ggplot(aes(x=as.factor(dup_political_orientation)))+
    geom_bar(fill="#0bb4ff")+
    labs(title="Political orientation", 
         x="Political orientation",
         y = "Count",
         fill="Type")+
    theme_classic()+
    theme(axis.text.x = element_text( size = 13, angle = 0,hjust = 0.5,vjust = 0.5),
          axis.title.y = element_text( size = 15),
          axis.title.x = element_text( size = 15),
          plot.title = element_text(size = 20,face = "bold"),
          plot.subtitle = element_text(size = 15),
          legend.title = element_text(size = 15),
          legend.text = element_text(size = 13))

  #Export
  
  barplot_4<-def_justice %>% 
    ggplot(aes(x=as.factor(dup_political_orientation)))+
    geom_bar(fill="#0bb4ff")+
    labs(title="Political orientation", 
         x="Political orientation",
         y = "Count",
         fill="Type")+
    theme_classic()+
    theme(axis.text.x = element_text( size = 13, angle = 0,hjust = 0.5,vjust = 0.5),
          axis.title.y = element_text( size = 15),
          axis.title.x = element_text( size = 15),
          plot.title = element_text(size = 20,face = "bold"),
          plot.subtitle = element_text(size = 15),
          legend.title = element_text(size = 15),
          legend.text = element_text(size = 13))
  
  
  ggsave(plot = barplot_4, 
         filename = "./Graficas_Definitivas/political_orientation.png", 
         width = 11, height = 7,
         type= "cairo",
         dpi = "retina")
  
  
  
# Regression --------------------------------------------------------------


#Regression table

#M1 (Completo)

m1_justice<-lm(index_conspiracy_belief ~ index_procedural_justice +
                 index_identity_judgements +
                 socio_economic_status +
                 education_level+
                 index_literacy_level+
                 group+
                 dup_political_orientation,
                 data = def_justice)


summary(m1_justice)



stargazer(m1_justice,
          ci = TRUE,
          type = "text",
          title= "Conspiracy beliefs",
          align=TRUE,
          style = "all",
          out = "Regression_Tables/Regression_Table_1.html")

#M2 (Drop: socio_economic_status )

m2_justice<-lm(index_conspiracy_belief ~ index_procedural_justice +
                 index_identity_judgements +
                 education_level+
                 index_literacy_level+
                 group+
                 dup_political_orientation,
               data = def_justice)


summary(m2_justice)


stargazer(m2_justice,
          ci = TRUE,
          type = "text",
          title="Conspiracy beliefs",
          align=TRUE,
          style = "all",
          out = "Regression_Tables/Regression_Table_2.html")


#M3 (Drop: socio_economic_status/ education_level)

m3_justice<-lm(index_conspiracy_belief ~ index_procedural_justice +
                 index_identity_judgements +
                 index_literacy_level+
                 group+
                 dup_political_orientation,
               data = def_justice)


summary(m3_justice)


stargazer(m3_justice,
          ci = TRUE,
          type = "text",
          title="Conspiracy beliefs",
          align=TRUE,
          style = "all",
          out = "Regression_Tables/Regression_Table_3.html")



#M4 (Drop: socio_economic_status/ education_level/group)

m4_justice<-lm(index_conspiracy_belief ~ index_procedural_justice +
                 index_identity_judgements +
                 index_literacy_level+
                 dup_political_orientation,
               data = def_justice)


summary(m4_justice)


stargazer(m4_justice,
          ci = TRUE,
          type = "text",
          title="Conspiracy beliefs",
          align=TRUE,
          style = "all",
          out = "Regression_Tables/Regression_Table_4.html")



#Regression table with all 4 models


stargazer( m1_justice,
           m2_justice,
           m3_justice,
           m4_justice,
           ci = TRUE,
           type = "text",
           title="Conspiracy beliefs",
           align=TRUE,
           out = "Regression_Tables/Full_Table.html")

