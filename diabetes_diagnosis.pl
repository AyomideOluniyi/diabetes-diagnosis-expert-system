% Dynamic predicates declaration to allow for adding and retracting facts dynamically.
:- dynamic symptom/2.
:- dynamic patient/3.
:- dynamic test_result/3.
:- dynamic medical_history/2.

% Discontiguous predicates declaration
:- discontiguous read_age/1, read_weight_status/1, read_medical_history/1, read_symptoms/1, read_test_results/1, diagnose_patient/0, diagnose/4, check_diabetes/6, has_critical_symptoms/1, has_moderate_symptoms/1, has_risk_factors/1, continue_or_end/0, handle_error/2.

% Main entry point for the expert system
start_diagnosis :-
    write('Welcome to the Type 2 Diabetes Diagnosis System'), nl,
    write('You can type ''terminate.'' at any point to end the session.'), nl,
    write('Please enter patient details (enclose names in single quotes)...'), nl,
    read_patient_data.

% Function to read patient data, moving to diagnose after data collection
read_patient_data :-
    write('Enter patient name (type ''end.'' to finish, use single quotes for names): '), 
    catch(read(Patient), Error, handle_error(Error, read_patient_data)),
    (   Patient = terminate -> terminate_session
    ;   Patient = end -> diagnose_patient  % Call diagnose_patient after data entry is complete
    ;   read_age(Patient),
        read_weight_status(Patient),
        read_medical_history(Patient),
        read_symptoms(Patient),
        read_test_results(Patient),
        read_patient_data
    ).

% Reading the patients age and validating the input
read_age(Patient) :-
    write('Enter patient age (must be a positive integer): '), 
    catch(read(Age), Error, handle_error(Error, read_age(Patient))),
    (   Age = terminate -> terminate_session
    ;   integer(Age), Age > 0
    ->  assert(patient(Patient, age, Age))
    ;   write('Invalid age. Please enter a valid number.'), nl,
        read_age(Patient)
    ).

% Reading the patients weight status with input validation
read_weight_status(Patient) :-
    write('Is the patient overweight? (yes/no): '), 
    catch(read(WeightStatus), Error, handle_error(Error, read_weight_status(Patient))),
    (   WeightStatus = terminate -> terminate_session
    ;   member(WeightStatus, [yes, no])
    ->  assert(patient(Patient, weight_status, WeightStatus))
    ;   write('Invalid input. Please type ''yes'' or ''no''. '), nl,
        read_weight_status(Patient)
    ).

% Reading the patients medical history with input validation
read_medical_history(Patient) :-
    write('Does the patient have a family history of diabetes? (yes/no): '), 
    catch(read(FamilyHistory), Error, handle_error(Error, read_medical_history(Patient))),
    (   FamilyHistory = terminate -> terminate_session
    ;   member(FamilyHistory, [yes, no])
    ->  assert(medical_history(Patient, family_history, FamilyHistory))
    ;   write('Invalid input. Please type ''yes'' or ''no''. '), nl,
        read_medical_history(Patient)
    ),
    write('Has the patient been previously diagnosed with diabetes? (yes/no): '), 
    catch(read(PreviousDiagnosis), Error, handle_error(Error, read_medical_history(Patient))),
    (   PreviousDiagnosis = terminate -> terminate_session
    ;   member(PreviousDiagnosis, [yes, no])
    ->  assert(medical_history(Patient, previous_diagnosis, PreviousDiagnosis))
    ;   write('Invalid input. Please type ''yes'' or ''no''. '), nl,
        read_medical_history(Patient)
    ),
    write('Does the patient have hypertension? (yes/no): '), 
    catch(read(Hypertension), Error, handle_error(Error, read_medical_history(Patient))),
    (   Hypertension = terminate -> terminate_session
    ;   member(Hypertension, [yes, no])
    ->  assert(medical_history(Patient, hypertension, Hypertension))
    ;   write('Invalid input. Please type ''yes'' or ''no''. '), nl,
        read_medical_history(Patient)
    ).

% Function to read symptoms, allowing multiple symptoms to be entered
read_symptoms(Patient) :-
    write('Enter one symptom at a time (type ''done.'' alone when finished): '), 
    catch(read(Symptom), Error, handle_error(Error, read_symptoms(Patient))),
    (   Symptom = terminate -> terminate_session
    ;   Symptom = done -> true  % Ends symptom entry if 'done.' is typed
    ;   assert(symptom(Patient, Symptom)),
        read_symptoms(Patient)
    ).

% Reading diagnostic test results for a comprehensive diagnosis
read_test_results(Patient) :-
    write('Enter fasting glucose level (mg/dL): '), 
    catch(read(FastingGlucose), Error, handle_error(Error, read_test_results(Patient))),
    (   FastingGlucose = terminate -> terminate_session
    ;   number(FastingGlucose), FastingGlucose >= 0
    ->  assert(test_result(Patient, fasting_glucose, FastingGlucose))
    ;   write('Invalid glucose level. Please enter a valid number.'), nl,
        read_test_results(Patient)
    ),
    write('Enter HbA1c percentage: '), 
    catch(read(HbA1c), Error, handle_error(Error, read_test_results(Patient))),
    (   HbA1c = terminate -> terminate_session
    ;   number(HbA1c), HbA1c >= 0, HbA1c =< 100
    ->  assert(test_result(Patient, hbA1c, HbA1c))
    ;   write('Invalid HbA1c percentage. Please enter a valid number.'), nl,
        read_test_results(Patient)
    ).

% Diagnose the patient based on collected data
diagnose_patient :-
    write('Enter patient name to diagnose (use single quotes for names): '), 
    catch(read(Name), Error, handle_error(Error, diagnose_patient)),
    (   Name = terminate -> terminate_session
    ;   patient(Name, age, Age),
        findall(Symptom, symptom(Name, Symptom), Symptoms),
        findall(MedicalHistory, medical_history(Name, _, MedicalHistory), MedicalHistories),
        diagnose(Name, Age, Symptoms, MedicalHistories),
        continue_or_end
    ;   write('No such patient found.'), nl,
        continue_or_end
    ).

% Combining age, symptoms, medical history, and test results to diagnose diabetes or suggest further testing
diagnose(Name, Age, Symptoms, MedicalHistories) :-
    test_result(Name, fasting_glucose, FastingGlucose),
    test_result(Name, hbA1c, HbA1c),
    check_diabetes(Age, Symptoms, MedicalHistories, FastingGlucose, HbA1c, Result),
    write(Name), write(Result), nl.

% Checking conditions for diabetes based on WHO guidelines, age, symptoms, medical history, and common medical practices
check_diabetes(Age, Symptoms, MedicalHistories, FastingGlucose, HbA1c, Result) :-
    (   (FastingGlucose > 126; HbA1c >= 6.5),
        (Age > 45; has_critical_symptoms(Symptoms); has_risk_factors(MedicalHistories))
    ->  Result = ' is likely to have diabetes based on age, multiple symptoms, medical history, and test results.'
    ;   (FastingGlucose > 100, FastingGlucose =< 126; HbA1c >= 5.7, HbA1c < 6.5),
        (Age > 30; has_moderate_symptoms(Symptoms); has_risk_factors(MedicalHistories))
    ->  Result = ' should have further testing due to age, suspicious symptoms, medical history, and borderline test results.'
    ;   Result = ' does not show strong indicators of diabetes based on tests, age, and medical history.'
    ).

% Checking for critical symptoms
has_critical_symptoms(Symptoms) :-
    member(increased_urination, Symptoms);
    member(excessive_thirst, Symptoms);
    member(unexplained_weight_loss, Symptoms).

% Checking for moderate symptoms
has_moderate_symptoms(Symptoms) :-
    member(tiredness, Symptoms);
    member(increased_hunger, Symptoms);
    member(frequent_infections, Symptoms);
    member(slow_healing_sores, Symptoms);
    member(blurred_vision, Symptoms).

% Checking for risk factors in medical history
has_risk_factors(MedicalHistories) :-
    member(yes, MedicalHistories).

% Providing an option to continue with another diagnosis or to end the session
continue_or_end :-
    write('Do you want to diagnose another patient? (yes/no): '), 
    catch(read(Response), Error, handle_error(Error, continue_or_end)),
    (   Response = terminate -> terminate_session
    ;   Response = yes -> start_diagnosis
    ;   write('Diagnosis session ended.'), nl
    ).

% Generic error handler that provides feedback and retries the operation
handle_error(Error, RetryGoal) :-
    write('An error occurred: '), write(Error), nl,
    write('Please ensure your input is correctly formatted.'), nl,
    call(RetryGoal).

% Function to terminate the session
terminate_session :-
    write('Diagnosis session terminated by user.'), nl, !, fail.
