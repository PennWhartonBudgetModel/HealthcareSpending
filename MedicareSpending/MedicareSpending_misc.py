## Medicare Spending
# https://meps.ipums.org/meps/userNotes_expenditures.shtml
# https://www.medicareinteractive.org/get-answers/medicare-covered-services/home-health-services/eligibility-for-home-health-part-a-or-part-b
import pandas as pd 

# Import the data extracted from MEPS to do some exploration check of the variable properties
df = pd.read_csv(r'C:\Users\yanhe8\Dropbox (Penn)\MEPS Tutorial\DATA\MEPS_Healthcare_Spending_2007_2017.csv')
df.ZIFMCR.value_counts()

# fill the NA with 0 for the Medicare expenditure variables that are not existing in all years
var_list = ["OBOMCR", "OBCMCR", "OBNMCR", "OBEMCR", "OBAMCR", "OBTMCR", "OPOMCR", "OPPMCR", 
"AMCMCR", "AMNMCR", "AMEMCR", "AMAMCR", "AMTMCR", "ZIFMCR", "ZIDMCR", "DVGMCR", "DVOMCR"]

for var in var_list:
	df.loc[df[var].isna(), var] = 0

# do some exploration to see the relationships between different Medicare Expenditure variables
# calculte the sum of the different office visit spending
df['OfficeVisitsMCR'] = df.OBDMCR + df.OBOMCR + df.OBCMCR + df.OBNMCR + df.OBEMCR + df.OBAMCR + df.OBTMCR
df['OfficeVisitsMCR'] = df.OBDMCR + df.OBOMCR
df['difference'] = df.OfficeVisitsMCR - df.OBVMCR
df.difference.value_counts()

df['OutpatientMCR'] = df.OPFMCR + df.OPDMCR + df.OPVMCR + df.OPSMCR + df.OPOMCR + df.OPPMCR + df.AMCMCR + \
	df.AMNMCR + df.AMEMCR + df.AMAMCR + df.AMTMCR
df['OutpatientMCR'] = df.AMCMCR + \
	df.AMNMCR + df.AMEMCR + df.AMAMCR + df.AMTMCR
df['difference'] = df.OutpatientMCR - df.OPTMCR
df.difference.value_counts()

df['AmbulatoryMCR'] = df.AMCMCR + df.AMNMCR + df.AMEMCR + df.AMAMCR + df.AMTMCR
df['MedicalMCR'] = df.OPTMCR
df.loc[df.OPTMCR < df.AmbulatoryMCR, 'MedicalMCR'] = df.AmbulatoryMCR

# check that the sum of the variables that will be used for analysis of different parts is the same as Total Medicare expenses
df['MedicareExp'] = df.OBVMCR + df.OPTMCR + df.ERTMCR + df.IPTMCR + df.DVTMCR + df.HHAMCR + df.HHNMCR + df.VISMCR + \
	df.OTHMCR + df.RXMCR 
df['difference'] = df.MedicareExp - df.TOTMCR
df.difference.value_counts()
'''
 0    381793
-1         1
 1         1
'''

# aggregate the output data from R to a yearly summary of total Medicare Spending
xls = pd.ExcelFile(r'C:\Users\yanhe8\Dropbox (Penn)\MEPS_Healthcare\Output\PerCapitaMedExpenditure.xlsx')
df = pd.read_excel(xls, 'All')
for var in ['total', 'totalRx', 'oop', 'mcare', 'mcareA', 'mcareB', 'mcareC', 'mcareD']:
	df[var + '_Total'] = df[var] * df.TotPop

for var in ['total_Total', 'totalRx_Total', 'oop_Total', 'mcare_Total', 'mcareA_Total', 'mcareB_Total', \
	'mcareC_Total', 'mcareD_Total', 'TotPop', 'MedicarePop']:
	df[var + 'AllAges'] = df.groupby('Year')[var].transform('sum')

df_yearly = df[['Year','total_TotalAllAges', 'totalRx_TotalAllAges', 'oop_TotalAllAges', 'mcare_TotalAllAges', \
	'mcareA_TotalAllAges', 'mcareB_TotalAllAges', 'mcareC_TotalAllAges', 'mcareD_TotalAllAges', 
	'MedicarePopAllAges', 'TotPopAllAges']].drop_duplicates()

df_yearly.to_excel(r'C:\Users\yanhe8\Dropbox (Penn)\MEPS_Healthcare\Output\MedicalExpenditure_Annual.xlsx')

# Get the population growth for people age above 60
df = pd.read_pickle(r'F:\.cache\MicrosimRunner_87c5e4831f42eab6_7968953225587690_2022_d0c6c6cbe6\micro.pkl.gz')
df = df[['Year', 'Age']]
df = df[df.Age>=60]
df['AgeGrp'] = df.Age
df.loc[df.Age>=65, 'AgeGrp'] = 65
df['Wgt'] = 1
df['Pop'] = df.groupby(['Year', 'AgeGrp']).Wgt.transform('sum')
df = df[['Year', 'AgeGrp', 'Pop']].drop_duplicates()
df['PopLastYear'] = df.groupby('AgeGrp').Pop.shift(1)
df['PopGrowth'] = df.Pop/df.PopLastYear
df.to_excel(r'C:\Users\yanhe8\Dropbox (Penn)\MEPS_Healthcare\Output\MedicareExpansionCost\PopGrowth.xlsx')

## get population growth for all ages
df = pd.read_pickle(r'F:\.cache\MicrosimRunner_87c5e4831f42eab6_7968953225587690_2022_d0c6c6cbe6\micro.pkl.gz')
df = df[['Year', 'Age']]
df['Wgt'] = 1
df['Pop'] = df.groupby('Year').Wgt.transform('sum')
df = df[['Year', 'Pop']].drop_duplicates()
df['PopLastYear'] = df.Pop.shift(1)
df['PopGrowth'] = df.Pop/df.PopLastYear-1
df.to_excel(r'E:\OneDrive - PennO365\MedicareExpansionCost\PopGrowth_allAge.xlsx')

# Load the population from CPS, get the population between 60, 61, 62, 63, 64 and above 65
# compare the population from CPS with the population from MEPS
df = pd.read_csv(r'Z:\DatasetProcessor\CPS\Interfaces\2020-06-18-15-54-aherrick-dcab0b7\CPS-ASEC.csv.gz')
df = df[['Year', 'Age', 'Weight']]
df = df[(df.Year >= 2007) & (df.Age >= 60)]
df['AgeGrp'] = df.Age
df.loc[df.Age >= 65, 'AgeGrp'] = 65
df['Pop'] = df.groupby(['Year', 'AgeGrp']).Weight.transform('sum')
df = df[['Year', 'AgeGrp', 'Pop']].drop_duplicates()
df_mcr = pd.read_excel(r'C:\Users\yanhe8\Dropbox (Penn)MEPS_Healthcare\Output\MedicareSpendingWithPop_oldPeople.xlsx')
df_mcr = df_mcr[['Year', 'AgeGrp', 'TotPop']]
df = df.merge(df_mcr, on = ['Year', 'AgeGrp'], how = 'left')
df.rename(columns = {'Pop': 'Pop_CPS', 'TotPop': 'Pop_MEPS'}, inplace = True)
df = df[df.Year <= 2017]
df['PopRatio'] = df.Pop_MEPS/df.Pop_CPS
df.sort_values(by = ['Year', 'AgeGrp'], inplace = True)
df.to_excel(r'C:\Users\yanhe8\Dropbox (Penn)\MEPS_Healthcare\Output\MedicareExpansionCost\PopElderly.xlsx')

## compare the Medicaid per person drug spending and the Medicaid per person Drug spending for each year
xls = pd.ExcelFile(r'C:\Users\yanhe8\Dropbox (Penn)\MEPS_Healthcare\Output\PerCapitaMedExpenditure.xlsx')
mcd = pd.read_excel(xls, 'Only people with Medicaid')
mcr = pd.read_excel(xls, 'Only people with Medicare')
mcr = mcr[['Year', 'Age', 'mcareD', 'MedicarePop']]
mcd = mcd[['Year', 'Age', 'MedicaidDrug', 'MedicaidPop']]
df = mcr.merge(mcd, on = ['Year', 'Age'], how = 'inner')
df = df[df.Age >= 65]
df['MedicaidDrugTot'] = df.MedicaidDrug * df.MedicaidPop
df['MedicareDrugTot'] = df.mcareD * df.MedicarePop
for var in ['MedicaidDrugTot', 'MedicareDrugTot', 'MedicaidPop', 'MedicarePop']:
	df[var + 'Sum'] = df.groupby(['Year'])[var].transform('sum')
df_year = df[['Year', 'MedicaidDrugTotSum', 'MedicareDrugTotSum', 'MedicaidPopSum', 'MedicarePopSum']].drop_duplicates()
df_year['MedicaidDrugPerCapita'] = df_year.MedicaidDrugTotSum/df.MedicaidPopSum
df_year['MedicareDrugPerCapita'] = df_year.MedicareDrugTotSum/df.MedicarePopSum
df_year.to_excel(r'C:\Users\yanhe8\Dropbox (Penn)\MEPS_Healthcare\Output\MedicaidMedicareComparison_old.xlsx')




