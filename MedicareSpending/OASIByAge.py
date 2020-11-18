
import pandas as pd 


## get the population
df = pd.read_csv(r'Z:\Microsim\Interfaces\2018-04-08-jagadeesh-Copy\social-security-longitudinal-wages\WplusSELong1951_2150_c.csv')
df = df[['pid', 'yob', 'yod']]
simPop = pd.DataFrame([])
for yr in range(1995, 2100):

	df_sub = df[(df.yob<=yr) & (df.yod>yr)]
	df_sub['Age'] = yr-df.yob
	df_sub = df_sub[df_sub.Age>=62]
	df_sub['Weight'] = 1
	df_sub['Year'] = yr
	df_sub['Pop'] = df_sub.groupby(['Year', 'Age']).Weight.transform('sum')
	df_sub = df_sub[['Year', 'Age', 'Pop']].drop_duplicates()
	simPop = simPop.append(df_sub)

simPop.sort_values(by=['Year', 'Age'], inplace =True)
simPop.to_excel(r'E:\OneDrive - PennO365\Pop_sim.xlsx')

## get the average
#df = pd.read_csv(r'E:\Repositories\OASIcalculator-sophie\OASIcalculator\Output\ForFelix\population\data.csv.gz')
df = pd.read_csv(r'E:\Repositories\OASIcalculator-sophie\OASIcalculator\Output\ForFelix\Biden\population\data.csv.gz')

df = df[['PersonID', 'YearBorn', 'YearDied', 'Year', 'OldAgeBenefits', 'YearCollection']]
df = df[(df.YearBorn<=df.Year) & (df.YearDied>df.Year)]
df = df[df.Year>=df.YearCollection]

df['Age'] = df.Year-df.YearBorn

df['AverageOASI'] = df.groupby(['Year', 'Age']).OldAgeBenefits.transform('mean')
df['SumOASI'] = df.groupby(['Year', 'Age']).OldAgeBenefits.transform('sum')
avgOASI_byage = df[['Year', 'Age', 'AverageOASI', 'SumOASI']].drop_duplicates()
avgOASI_byage.sort_values(by = ['Year', 'Age'], inplace = True)
avgOASI_byage['AverageOASI'] = avgOASI_byage.AverageOASI *1000
avgOASI_byage['SumOASI'] = avgOASI_byage.SumOASI *1000
avgOASI_byage = avgOASI_byage.merge(simPop, on = ['Year', 'Age'], how = 'left')
avgOASI_byage['AverageOASI_allPopBased'] = avgOASI_byage.SumOASI/avgOASI_byage.Pop

#avgOASI_byage.to_excel(r'E:\OneDrive - PennO365\AverageOAIbyAge_Baseline.xlsx')
avgOASI_byage.to_excel(r'E:\OneDrive - PennO365\AverageOAIbyAge_Biden.xlsx')

#################################################
# check my run results are the same as Sophie's
df['YearOASI'] = df.groupby('Year').OldAgeBenefits.transform('sum')
test = df[['Year', 'YearOASI']].drop_duplicates()
test['YearOASI'] = test.YearOASI * 1000 * 1400/1000000000
test.sort_values(by = 'Year', inplace = True)
#test.to_excel(r'E:\OneDrive - PennO365\AnnualOASDI_Baseline.xlsx')
test.to_excel(r'E:\OneDrive - PennO365\AnnualOASDI_Biden.xlsx')

# Biden
data = pd.read_csv(r'Z:\OASIcalculator\Interfaces\2020-08-17-21-18-ses-9b5e522\oasicalculator3\WriteSeriesInterface_0_OASIcalculator_oasicalculator3_8f13c805c6\aggregates.csv')

# Baseline
data = pd.read_csv(r'Z:\OASIcalculator\Interfaces\2020-08-17-21-18-ses-9b5e522\oasicalculator3\WriteSeriesInterface_0_OASIcalculator_oasicalculator3_fcf2525732\aggregates.csv')
