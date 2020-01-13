import pandas as pd
import numpy as np
def factorQuality(data):
    tb = data.select_dtypes(include=object)
    df = pd.DataFrame()
    for i in range(len(tb.columns)):
        coldata = tb.iloc[:,i]
        uniqueCount = len(set(coldata))
        naCount = np.sum(coldata.isnull())
        naPercentage = '{0:.2%}'.format(naCount / len(coldata))
        if len(coldata.mode()) > 1: 
            print("*%s 有%i個眾數" % (coldata.name, len(coldata.mode())))
            mostAppear = coldata.mode()[0]
        else:
            mostAppear = coldata.mode()[0]
        mostAppearCount = np.sum(coldata == mostAppear)
        unbalence = '{0:.2%}'.format(mostAppearCount / len(coldata))
        ser = pd.Series([uniqueCount, naCount, naPercentage, mostAppear,mostAppearCount, unbalence], index=['不同值個數','空值個數','空值比例','次數最多的值','最多出現次數','不平衡度'])
        df = df.append(pd.DataFrame(ser).T)
    df.index = tb.columns
    return(df)


def numsDataQuality(data):
    import numpy as np
    import pandas as pd
    tb = data.select_dtypes(exclude=object)
    df = pd.DataFrame()
    for i in range(len(tb.columns)):
        coldata = tb.iloc[:,i]
        uniqueCount = len(set(coldata))
        naCount = np.sum(coldata.isnull())
        naPercentage = '{0:.2%}'.format(naCount / len(coldata))
        if len(coldata.mode()) > 1: 
            print("*%s 有%i個眾數" % (coldata.name, len(coldata.mode())))
            mostAppear = coldata.mode()[0]
        else:
            mostAppear = coldata.mode()[0]
        mostAppearCount = np.sum(coldata == mostAppear)
        unbalence = '{0:.2%}'.format(mostAppearCount / len(coldata))
        minNum = np.min(coldata)
        maxNum = np.max(coldata)
        meanNum = np.mean(coldata)
        stdev = np.std(coldata)
        minus3std = (meanNum - 3 * stdev)
        plus3std = (meanNum + 3 * stdev)
        ser = pd.Series([uniqueCount, naCount, naPercentage, mostAppear,mostAppearCount, unbalence, minNum, maxNum, meanNum, stdev, minus3std, plus3std], index=['不同值個數','空值個數','空值比例','次數最多的值','最多出現次數','不平衡度','最小值','最大值','平均值','標準差','M-3','M+3'])
        df = df.append(pd.DataFrame(ser).T)
    df.index = tb.columns
    return(df)
