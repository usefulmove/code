# -*- coding: utf-8 -*-
"""
Created on Sat Aug  8 18:35:16 2020

@author: duane
"""

import pandas as pd
import matplotlib.pyplot as plt

# load housing data set
homes = pd.read_csv('housing.csv')

homes.info()

# get entries for categorical feature
homes.ocean_proximity.value_counts()

homes.describe()

homes.iloc[:, :4].describe()

homes.iloc[:, 4:8].describe()

homes.iloc[:, 8:].describe()

homes.hist(bins = 50, figsize = (20, 15))
plt.show()