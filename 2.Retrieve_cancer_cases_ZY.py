import pandas as pd
import numpy as np
from datetime import datetime
import pyreadr


def get_date_cancerType(date_file, type_file):
    
    date = pd.read_csv(date_file)
    type = pd.read_csv(type_file)
    

    tmp_date = date.rename(columns={"Unnamed: 0": "ID"})
    date = tmp_date.set_index('ID')
    date_transposed = date.T
    


    tmp_type = type.rename(columns={"Unnamed: 0": "ID"})
    type = tmp_type.set_index('ID')
    type_transposed = type.T

    dimention_date = date_transposed
    d1 = dimention_date.to_dict()
    
    d2={}
    for key, val in d1.items():
        val_arr = np.array(list(val.values()))
        
        new_val_arr = [x for x in val_arr if str(x) != 'nan']
        
        date_arr = [datetime.strptime(date_str, '%Y-%m-%d').date() for date_str in new_val_arr]
        
        d2[key] = [date_arr.index(min(date_arr)), min(date_arr)]
    
    #print(d2)
    
    dimention_type = type_transposed
    d3 = dimention_type.to_dict()
    d4={}
    
    for key, val in d3.items():
        val_arr = np.array(list(val.values()))
        new_val_arr = [x for x in val_arr if str(x) != 'nan']
        d4[key] = new_val_arr
    print(d4)
    
    d5={}
    for k1, v1 in d2.items():
        for k2, v2 in d4.items():
            if k1 == k2:
                d5[k1] = (v1[1], v2[v1[0]], v2[-1])
    df = pd.DataFrame.from_dict(d5)
    final_df = df.T
    final_df = final_df.rename({0: 'First_Diagnose_Date', 1: 'Cancer_description', 2:"cancer_type"}, axis=1)
    return final_df

res = get_date_cancerType("date.csv", "type.csv")
print(res)

res.to_csv("out2.csv")


def collapse_data(infile):
    
    """ a function to choose a Non-Na values for each instance """
    
    df = pd.read_csv(infile)
    print(df.shape)
    dict = df.to_dict('list')
    print(len(dict))
    
    new_dict = {}
    
    for k, v in dict.items():
        
        v = [str(i) for i in v]
        for (i, item) in enumerate(v):
            if item.__contains__("%"):
                v[i] = item.split("%")[0] ##always select the first value of the instance
        
        new_dict[k] = v
    
    print(len(new_dict))
    
    df2=pd.DataFrame.from_dict(new_dict,orient='index').transpose()
    
    print(df2)
    
    df2.to_csv("clean.csv")
    
                
    
res1 = collapse_data("all.csv")





