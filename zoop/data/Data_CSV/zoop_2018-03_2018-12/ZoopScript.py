#Aquatic Veg Study
#Zooplankton Data
#extracting data from Excel workbook(s) and exporting it as .csv files
#output is a folder named after the workbook populated by the indvidual .csv files
#questions: sarah.perry@water.ca.gov

#~~~Variables to Edit~~~#
#name of excel workbook(s)
filePath = "./" #./ for current directory
excelFile = ["extra_sheets.xlsx"] #keep brackets; add multiple if wanted

#choose output path
outputPath = filePath #will be created if it doesn't exist
#~~~~~~~~~~~~~~~~~~~~~~~~#

#import packages
import os
import time
import math
import numpy as np
import pandas as pd
from datetime import datetime

#disable warning for chained assignments
pd.options.mode.chained_assignment = None

#~~~Extract the Data~~~#
#create full file
for wkbk in excelFile:
    excelBook = filePath+wkbk #full path
    excelName,_ = wkbk.split(".",1) #remove extension from name
    
    #grab all the sheet names in a map
    sheetMap = pd.read_excel(excelBook,
                             sheet_name = None)

    #iterate over all the sheets
    for sheetName in sheetMap.keys():
        #read in data, split into two dataframes
        dfOne = pd.read_excel(excelBook,
                               sheet_name = sheetName,
                               header = 16,
                               usecols = list(range(12)))

        dfTwo = pd.read_excel(excelBook,
                               sheet_name = sheetName,
                               header = 16,
                               usecols = list(range(16,35)))

        #create a list of dfs and one to merge over later
        dfAll = [dfOne,dfTwo]
        dfConcat = []

        #~~~Clean Up Data~~~#
        #loop through the dataframes
        for x in range(len(dfAll)):
            #remove rows based on if they're df1 or df2
            if x == 0:
                dfCleaned = dfAll[x][dfAll[x]["Unnamed: 0"] != "Comments:"]
                dfCleaned = dfCleaned[dfCleaned["Unnamed: 0"] !=  "Zoop net type = "]
            else:
                dfCleaned = dfAll[x][dfAll[x]["Unnamed: 17"] != "Unable to make micro tally"]
                dfCleaned = dfCleaned[dfCleaned["Number.1"] !=  "Unable to make meso tally"]

            #delete columns with all NaN values
            dfNaN = dfCleaned.dropna(axis = 1, 
                                  how = "all")
            dfNaN = dfNaN.dropna(axis = 0, 
                                  how = "all")
            
            if len(dfNaN.columns) != 1: #there's data
                #reset index
                dfNaN = dfNaN.reset_index(drop = True)
                
                #rename columns
                if len(dfNaN.columns) == 3: #if all data is there
                    dfNaN.columns = ["taxon","count","subsample"]
                elif len(dfNaN.columns) == 2: #if subsample isn't populated
                    dfNaN["subsample"] = ''
                    dfNaN.columns = ["taxon","count","subsample"]

                #append to df list
                dfConcat.append(dfNaN)

        #merge two dfs if there's data
        if len(dfConcat) == 2: #merge if 2 dfs 
            dfData = pd.concat(dfConcat,
                      ignore_index = True,
                      sort = True)
        elif len(dfConcat) == 1: #if only 1 df
            dfData = dfConcat[0] #first element will be the df
        else:
            continue #restart loop if no data

        #create a list of the categories
        catList = []
        for x in range(len(dfData["taxon"])):
            if dfData["taxon"][x].isupper() == True:
                catList.append(dfData["taxon"][x])

        #add new column for the categories
        dfData["category"] = np.nan

        #Populate the Category Column
        #set starting values for the for loop
        count = 0
        start = 0
        catVal = 1

        #populate category column
        for x in range(start, len(dfData["taxon"])):
            count += 1
            if catVal <= len(catList)-1:
                if dfData["taxon"][x] != catList[catVal]:
                    dfData["category"][x] = catList[catVal-1]
                else:
                    dfData["category"][x] = catList[catVal]
                    start = count
                    catVal += 1
            else:
                dfData["category"][x] = catList[catVal-1]

        #drop all rows with no count/subsample data
        dfData = dfData.dropna(axis = 0, 
                              subset = ["count","subsample"])   

        #reset index
        dfZoo = dfData.reset_index(drop = True)

        #have all values in taxon be capitalized, no whitespace
        for x in range(len(dfZoo["taxon"])):
            if dfZoo["taxon"][x].isupper() == True:
                dfZoo["taxon"][x] = dfZoo["taxon"][x].capitalize()
                
            if dfZoo["taxon"][x].endswith(" "):
                dfZoo["taxon"][x] = dfZoo["taxon"][x].strip()

        #~~~Extract the Metadata~~~#
        dfMeta = pd.read_excel(excelBook,
                               sheet_name = sheetName,
                               header = 4,
                               nrows = 14)

        #create data column
        dateList = dfMeta.iloc[0,5:13].tolist()

        for x in range(len(dateList)):
            dateList[x] = str(int(dateList[x]))

        dateStr = ''.join(dateList)
        dateDate = datetime.strptime(dateStr, "%m%d%Y")
        dfZoo["date"] = dateDate

        #create time column
        timeTime = dfMeta.iloc[3,5]
        dfZoo["time"] = timeTime.strftime('%H:%M')

        #create sample column
        _,sampName = sheetName.split(" ", 1) #remove beginning numbers
        sampName = sampName.lstrip() #remove leading spaces

        if " " in sampName:
            sampName = sampName.replace(" ", "_") #replaces spaces with underscores

        dfZoo["sample"] = sampName #assign name to samp column

        #Populate/Append Remaining Variables
        #v1
        v1 = []
        for valRange in range(12,17): #will look for value over a range (the black box)
            valLoc = dfMeta.iloc[3,valRange]
            if isinstance(valLoc,(int, float, complex)): #check if number; else return error
                if math.isnan(valLoc) == False:
                    v1.append(valLoc)
            else:
                v1.append('NA')
                print("Check sheet %s" % sheetName)

        dfZoo["v1_ml"] = v1[0]

        #v2
        v2 = []
        for valRange in range(1,4):
            valLoc = dfMeta.iloc[6,valRange]
            if isinstance(valLoc,(int, float, complex)):
                if math.isnan(valLoc) == False:
                    v2.append(valLoc)
            else:
                v2.append('NA')
                print("Check sheet %s" % sheetName)

        dfZoo["v2_ml"] = v2[0]

        #sub1
        sub1 = []
        for valRange in range(19,23):
            valLoc = dfMeta.iloc[3,valRange]
            if isinstance(valLoc,(int, float, complex)):
                if math.isnan(valLoc) == False:
                    sub1.append(valLoc)
            else:
                sub1.append('NA')
                print("Check sheet %s" % sheetName)

        dfZoo["sub1_ml"] = sub1[0]

        #sub2
        sub2 = []
        for valRange in range(6,9):
            valLoc = dfMeta.iloc[6,valRange]
            if isinstance(valLoc,(int, float, complex)):
                if math.isnan(valLoc) == False:
                    sub2.append(valLoc)
            else:
                sub2.append('NA')
                print("Check sheet %s" % sheetName)

        dfZoo["sub2_ml"] = sub2[0]

        #reorganize columns
        dfZoo = dfZoo[["sample", "date", "time", "category", "taxon", "count", "subsample",
                       "v1_ml", "sub1_ml", "v2_ml", "sub2_ml"]]

        #export CSV files into own folder
        fullPath = outputPath+excelName+'/'

        if not os.path.exists(fullPath): #will create folder if it doesn't exist
            os.makedirs(fullPath)

        dfZoo.to_csv(fullPath+sampName+'_'+dateDate.strftime('%Y-%m-%d')+'.csv',
                    index = False)
                
print("Done! :)")



