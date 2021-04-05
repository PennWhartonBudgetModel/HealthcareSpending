import pandas as pd
import numpy as np
import os

def main():

    # read in the CPS processed dataset from HPCC
    # my HPCC is U drive. Change if it is not the case for you!
    # cps = pd.read_csv(os.path.join('U:', 'DatasetProcessor', 'CPS', 'Interfaces', '2020-10-08-19-42-aherrick-971c25c', 'CPS-ASEC.csv.gz'))
    # cps = pd.read_pickle(r'E:\Repositories\DatasetProcessor\CPS\asec_afterhealth.pkl')
    cps = pd.read_csv(r'F:\Interfaces\CPS-ASEC.csv.gz')
    
    # set up the excel file
    writer = pd.ExcelWriter('HealthcareInCPS_updated.xlsx')
    
    # pre-processs cps
    cps_cleaned = clean_dataset(cps)

    # year 2017
    df = calculate_distribution(cps_cleaned, year = 2017)
    df.to_excel(writer, 'ByAgeGroup2017', index = False)
    df = calculate_distribution(cps_cleaned, year = 2017, by_agegrp=False)
    df.to_excel(writer, 'ByAge2017', index = False)

    # year 2018
    df = calculate_distribution(cps_cleaned, year = 2018)
    df.to_excel(writer, 'ByAgeGroup2018', index = False)
    df = calculate_distribution(cps_cleaned, year = 2018, by_agegrp=False)
    df.to_excel(writer, 'ByAge2018', index = False)

    # year 2019
    df = calculate_distribution(cps_cleaned, year = 2019)
    df.to_excel(writer, 'ByAgeGroup2019', index = False)
    df = calculate_distribution(cps_cleaned, year = 2019, by_agegrp=False)
    df.to_excel(writer, 'ByAge2019', index = False)

    # output the result
    # it saves out to the same location as this script
    writer.save()

def clean_dataset(cps):

    keep_var = ['Year', 'Age', 'Weight', 'UninsuredLastYear', 'PrivateHealthInsuranceCoverageLastYear', 
    'OtherPublicHealthInsuranceCoverageLastYear', 'MedicaidCoverageLastYear', 'MedicareCoverageLastYear']
    cps = cps[keep_var]

    # recode year
    cps['Year'] = cps['Year'] - 1
        
    # recode age
    cps['Age'] = cps['Age'] - 1

    # create the age group
    # exlude individuals younger than 21
    cps = cps[cps.Age >= 21]

    age_bin = [20, 25, 34, 44, 54, 64, 74, 84, 120]
    age_label = ['21-25', '26-34', '35-44', '45-54', '55-64', '65-74', '75-84','85+']
    cps['AgeGroup'] = pd.cut(cps.Age, age_bin, labels = age_label)

    cps['AgeGroup1'] = (cps.Age>=21)&(cps.Age<=64)
    cps['AgeGroup2'] = (cps.Age>=45)&(cps.Age<=64)
    cps['AgeGroup3'] = cps.Age>=65

    cps.rename(columns = {
        'PrivateHealthInsuranceCoverageLastYear': 'PrivateHealthInsuranceCoverage',
        'OtherPublicHealthInsuranceCoverageLastYear': 'OtherPublicHealthInsuranceCoverage',
        'MedicaidCoverageLastYear': 'MedicaidCoverage',
        'MedicareCoverageLastYear': 'MedicareCoverage',
        'UninsuredLastYear': 'Uninsured'
    }, inplace = True)

    return cps

def calculate_distribution(cps, year, by_agegrp = True):

    cps = cps[cps.Year == year]

    insurance_var = ['PrivateHealthInsuranceCoverage', 'OtherPublicHealthInsuranceCoverage', \
            'MedicaidCoverage', 'MedicareCoverage', 'Uninsured']
    if by_agegrp:
        df = pd.DataFrame({ \
            'AgeGroup': ['Total', '21-25', '26-34', '35-44', '45-54', '55-64', '65-74', '75-84','85+', \
            '21-64', '45-64', '65+']})

        for var in insurance_var:
            cps.loc[cps[var]<0, var] = 0
            merger = cps.groupby('AgeGroup').apply(lambda x: np.ma.average(x[var], weights = x.Weight)).reset_index().rename(columns = {0:var})
            df = df.merge(merger, on = 'AgeGroup', how = 'left')
            df.loc[df.AgeGroup=='Total', var] = np.ma.average(cps[var], weights = cps.Weight)
            df.loc[df.AgeGroup=='21-64', var] = np.ma.average(cps[cps.AgeGroup1][var], weights = cps[cps.AgeGroup1].Weight)
            df.loc[df.AgeGroup=='45-64', var] = np.ma.average(cps[cps.AgeGroup2][var], weights = cps[cps.AgeGroup2].Weight)
            df.loc[df.AgeGroup=='65+', var] = np.ma.average(cps[cps.AgeGroup3][var], weights = cps[cps.AgeGroup3].Weight)
        pop = cps.groupby('AgeGroup').Weight.sum().reset_index().rename(columns = {'Weight':'Population'})
        df = df.merge(pop, on = 'AgeGroup', how = 'left') 
        df.loc[df.AgeGroup=='Total', 'Population'] = cps.Weight.sum()
        df.loc[df.AgeGroup=='21-64', 'Population'] = cps[cps.AgeGroup1].Weight.sum()
        df.loc[df.AgeGroup=='45-64', 'Population'] = cps[cps.AgeGroup2].Weight.sum()
        df.loc[df.AgeGroup=='65+', 'Population'] = cps[cps.AgeGroup3].Weight.sum()

    else:
        df = pd.DataFrame({'Age': range(21, cps.Age.max()+1)})
        for var in insurance_var:
            cps.loc[cps[var]<0, var] = 0
            merger = cps.groupby('Age').apply(lambda x: np.ma.average(x[var], weights = x.Weight)).reset_index().rename(columns = {0:var})
            df = df.merge(merger, on = 'Age', how = 'left')
        pop = cps.groupby('Age').Weight.sum().reset_index().rename(columns = {'Weight':'Population'})
        df = df.merge(pop, on = 'Age', how = 'left')

    return df

if __name__ == "__main__":
	main()

# # check the pattern of the uninsured population--make sure the majority of the unsured are not kids
# cps = cps[cps.Year>1979]
# dt = cps[cps.Age<65]
# # all people above 1980 have valid data
# dt['UninsuredWeight'] = dt.UninsuredLastYear * dt.Weight
# totpop = pd.DataFrame(dt.groupby('Year').Weight.sum().reset_index())
# totpop.rename(columns={'Weight': 'TotalPopulation'}, inplace = True)

# uninsured = pd.DataFrame(dt.groupby('Year').UninsuredWeight.sum().reset_index())
# uninsured.rename(columns={'UninsuredWeight': 'UninsuredPopulation'}, inplace = True)

# df = totpop.merge(uninsured, on='Year', how='left')
# df['UninsuredKidsPct'] = df['UninsuredPopulation']/df['TotalPopulation']
# from matplotlib import pyplot as plt
# plt.plot(df.Year, df.UninsuredKidsPct)
 



