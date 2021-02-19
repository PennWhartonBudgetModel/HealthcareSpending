import pandas as pd
import numpy as np
import os

def main():

    # read in the CPS processed dataset from HPCC
    # my HPCC is U drive. Change if it is not the case for you!
    cps = pd.read_csv(os.path.join('U:', 'DatasetProcessor', 'CPS', 'Interfaces', '2020-10-08-19-42-aherrick-971c25c', 'CPS-ASEC.csv.gz'))
    
    # set up the excel file
    writer = pd.ExcelWriter('HealthCareInCPS.xlsx')
    
    # pre-processs cps
    cps1 = clean_dataset(cps)
    cps2 = clean_dataset(cps, allow_negative_income = False)

    # year 2017
    df = calculate_distribution(cps1[cps1.Year == 2017])
    df.to_excel(writer, 'year_2017', index = False)

    # year 2016
    df = calculate_distribution(cps1[cps1.Year == 2016])
    df.to_excel(writer, 'year_2016', index = False)

    # year 2017 without negative income
    df = calculate_distribution(cps2[cps2.Year == 2017])
    df.to_excel(writer, 'year_2017_no_negative_income', index = False)

    # year 2016 without negative income
    df = calculate_distribution(cps2[cps2.Year == 2016])
    df.to_excel(writer, 'year_2016_no_negative_income', index = False)

    # output the result
    # it saves out to the same location as this script
    writer.save()

def clean_dataset(cps, allow_negative_income = True):
    # recode year
    cps['Year'] = cps['Year'] - 1
        
    # recode age
    cps['Age'] = cps['Age'] - 1

    income_var = ['WageIncomeLastYear', 'BusinessIncomeLastYear', 'FarmIncomeLastYear', 'SocialSecurityIncomeLastYear', \
        'NonSSRetirementIncomeLastYear', 'InterestIncomeLastYear', 'UnemploymentBenefitsIncomeLastYear', \
        'DividendIncomeLastYear', 'RentalIncomeLastYear', 'AlimonyIncomeLastYear', 'OtherIncomeLastYear', \
        'SelfEmploymentIncomeLastYear']
        # NOTE: NonSSRetirementIncomeLastYear, UnemploymentBenefitsIncomeLastYear (should be UnemploymentBenefitsLastYear) not available
    
    # drop individuals with negative income if stated 
    if not allow_negative_income:
        for var in income_var:
            cps.loc[cps[var] < 0, var] = 0
            # cps = cps[cps[var]>=0]

    # construct residence income
    cps['TotalIncome'] = cps[income_var].sum(axis=1)
    cps['ResidenceIncome'] = cps.groupby('ResidentID').TotalIncome.transform('sum')

    # rename and subset the processed CPS dataset
    cps = cps[['Age', 'ResidenceIncome', 'Year', \
        'PrivateHealthInsuranceCoverageLastYear', 'OtherPublicHealthInsuranceCoverageLastYear', \
        'MedicaidCoverageLastYear', 'MedicareCoverageLastYear', 'UninsuredLastYear', 'Weight']]
    cps.columns = cps.columns.str.replace('LastYear', '')
    # NOTE: PrivateHealthInsuranceCoverageLastYear, OtherPublicHealthInsuranceCoverageLastYear,
    # MedicaidCoverageLastYear, MedicareCoverageLastYear, UninsuredLastYear not available

    # create the income group
    income_bin = [cps.ResidenceIncome.min()-1, 25000, 49999, 74999, 99999, 124999, cps.ResidenceIncome.max()]
    income_label = ['Less than $25,000', '$25,000 to $49,999', '$50,000 to $74,900', '$75,000 to $99,999', '$100,000 to $124,999', '$125,000 or more']
    cps['ResidenceIncomeGroup'] = pd.cut(cps.ResidenceIncome, income_bin, labels = income_label)

    # create the age group
    # exlude individuals younger than 21
    cps = cps[cps.Age >= 21]

    age_bin = [21, 25, 34, 44, 54, 64, 74, 84, 94]
    age_label = ['21-25', '26-34', '35-44', '45-54', '55-64', '65-74', '75-84','85-94']
    cps['AgeGroup'] = pd.cut(cps.Age, age_bin, labels = age_label)

    cps['AgeGroup1'] = (cps.Age>=21)&(cps.Age<=64)
    cps['AgeGroup2'] = (cps.Age>=45)&(cps.Age<=64)
    cps['AgeGroup3'] = cps.Age>=65

    return cps

def calculate_distribution(cps):
    df_holder = []
    df = pd.DataFrame({ \
        'AgeGroup': ['Total', '21-25', '26-34', '35-44', '45-54', '55-64', '65-74', '75-84','85-94', \
        '21-64', '45-64', '65+']})

    insurance_var = ['PrivateHealthInsuranceCoverage', 'OtherPublicHealthInsuranceCoverage', \
            'MedicaidCoverage', 'MedicareCoverage', 'Uninsured']
    # NOTE: All insurance variables are not available

    for var in insurance_var:
        merger = cps.groupby('AgeGroup').apply(lambda x: np.ma.average(x[var], weights = x.Weight)).reset_index().rename(columns = {0:var})
        df = df.merge(merger, on = 'AgeGroup', how = 'left')
        df.loc[df.AgeGroup=='Total', var] = np.ma.average(cps[var], weights = cps.Weight)
        df.loc[df.AgeGroup=='21-64', var] = np.ma.average(cps[cps.AgeGroup1][var], weights = cps[cps.AgeGroup1].Weight)
        df.loc[df.AgeGroup=='45-64', var] = np.ma.average(cps[cps.AgeGroup2][var], weights = cps[cps.AgeGroup2].Weight)
        df.loc[df.AgeGroup=='65+', var] = np.ma.average(cps[cps.AgeGroup3][var], weights = cps[cps.AgeGroup3].Weight)
    
    df_holder.append(df)
    
    for inc in ['Less than $25,000', '$25,000 to $49,999', '$50,000 to $74,900', '$75,000 to $99,999', '$100,000 to $124,999', '$125,000 or more']:
        df = pd.DataFrame({ \
            'AgeGroup': [inc, '21-25', '26-34', '35-44', '45-54', '55-64', '65-74', '75-84','85-94', \
            '21-64', '45-64', '65+']})

        cps_inc = cps[cps.ResidenceIncomeGroup == inc]

        for var in insurance_var:
            merger = cps_inc.groupby('AgeGroup').apply(lambda x: np.ma.average(x[var], weights = x.Weight)).reset_index().rename(columns = {0:var})
            df = df.merge(merger, on = 'AgeGroup', how = 'left')
            df.loc[df.AgeGroup==inc, var] = np.ma.average(cps_inc[var], weights = cps_inc.Weight)
            df.loc[df.AgeGroup=='21-64', var] = np.ma.average(cps_inc[cps_inc.AgeGroup1][var], weights = cps_inc[cps_inc.AgeGroup1].Weight)
            df.loc[df.AgeGroup=='45-64', var] = np.ma.average(cps_inc[cps_inc.AgeGroup2][var], weights = cps_inc[cps_inc.AgeGroup2].Weight)
            df.loc[df.AgeGroup=='65+', var] = np.ma.average(cps_inc[cps_inc.AgeGroup3][var], weights = cps_inc[cps_inc.AgeGroup3].Weight)

        df_holder.append(df)

    df = pd.concat(df_holder)

    return df

if __name__ == "__main__":
	main()

