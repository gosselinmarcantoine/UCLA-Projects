# -*- coding: utf-8 -*-
"""
Spyder Editor

This is a temporary script file.
"""
print('Hi World!!!')

"""
fn & F9 will run the script.
"""

3*2

4-5

b=12/3
b

7^2   # Not exponent

7**2  # Exponent

10/4

10//4 # Floor division

# List
# For an ordered sequence of homogeneous collections, whose values can be 
# changed later in the program
x=[1,3,4,5,2,-2]
x[5]
x[0]  # Sequences are 0-indexed in Python as Java and C, C++
type(x)
max(x)
min(x)
sum(x)

########
# Tuple
########
# For an ordered sequence of heterogenous collections whose valuesd need not be
# changed later in the program
y1=(1,3,5,7,9,11,13)
type(y1)
y1(0:6)
y1[0:6]   # [start:stop] where stop index is NOT INCLUDED!!
y2=("US","Mexico","Canada",1994, 2018)
y2[1:4]
y2[1:]
y2[:4]
y2[-2]  # Count from the right
y2[-4:-2]
y1+y2
len(y1)
y2*3
y2[2]
type(y2[2])  # string
y2[2][0:3]

#######
# List
#######
# Their contents can be modified

x1=["USA","Mexico","Canada","Japan","Europe"]
x1
type(x1)
x1.append("China")
x1
x1.remove("Mexico")
x1
x1.reverse()
x1
x1.sort()
x1
x1.sort(key=len)
x1
x2=x1
x2

x1[1]="UK"
x1
x2

# Tuple cannot be changed
y3=("USA","Mexico","Canada","Japan","Europe")
y3.append("China")
y4=y3
y3[1]="UK"
y5=list(y3)
y5.append("China")
y5
y6 = tuple(y5)
y5.append('Russia')
y5
y6

import math

math.sqrt(49)
  
def myfirstfunction(x):
    y=2*x+3
    print(y)
    
myfirstfunction(10)

for x in range(0,20,5):
    myfirstfunction(x)
    
def newfunction(x,y):
    z=x**2+3*x*y+0.5*y
    print(z)
    
newfunction(1,4)

c = (1,2,2,2,3,4,2)
c.count(2)

import numpy as np
# Generate some random data
data = np.random.randn(2,3)
data

data*10

data1=np.arange(12).reshape((3,4))
data1
data1.T
data1.sum()
data1.mean()
np.mean(data1)
data1.std()
data1.min()
data1.max()

data2=np.array([[7,6,4,5],[3,0,2,1],[11,9,12,10]])
data2

import pandas as pd

ufo=pd.read_csv('http://bit.ly/uforeports')
ufo.shape
ufo.head()
ufo.drop("City",axis=1).head()  # axis=0 --> row; axis=1 --> column
ufo.head()   # City Column is Not really gone!
ufo.drop("City",axis=1, inplace=True) # It permanently changes the data. 
ufo.head()
ufo.dropna(how="any").shape # delete rows containing any missing values
ufo.tail()
ufo.fillna(method="ffill").tail # forward fill
ufo.fillna(method="bfill").tail # back fill

# Import data from computer files
homeprice = pd.read_csv('C:/Users/WYU/Documents/Zip08/2018 Q4 Fall_XData/Data/W02b_homeprice.csv')
type(homeprice)
homeprice.head()
homeprice.columns

# import data from API
import requests
url = 'https://api.census.gov/data/2014/pep/natstprc?get=STNAME,POP&DATE=7&for=state:*'
resp = requests.get(url)
resp
data = resp.json()
df1 = pd.DataFrame(data, columns=['state','pop','date','number'])
df1
df2=df1[1:]
df2

?np.abs







