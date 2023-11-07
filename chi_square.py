# -*- coding: utf-8 -*-
"""Chi square.ipynb

Automatically generated by Colaboratory.

Original file is located at
    https://colab.research.google.com/drive/1eW8NxVpfyRw6OG57f1LZQPyqUYRjqLz1
"""

!pip install plotnine
import pandas as pd
import scipy.stats as stats
import matplotlib.pyplot as plt
from plotnine import *

df = pd.read_csv("Main_Coder_Only.csv")

contingency_table = pd.crosstab(df['Type of Video'], df['Risk of Bullshit'])

chi2, p, dof, expected = stats.chi2_contingency(contingency_table)

alpha = 0.05 #significance level

print("Chi-square statistic:", chi2)
print('p-value', p)
if p < alpha:
  print('There is a significant relationship between 2 variables')
else:
  print("There is no significant relationship between 2 variables")

plot = (
    ggplot(df, aes(x="Type of Video", fill="Risk of Bullshit")) +
    geom_bar(position="fill") +
    labs(title="Relationship between Video Type and Bullshit Risk",
         x="Type of Video", y="Proportion") +
    theme_bw() +
    theme(axis_text_x=element_text(angle=90, hjust=1))
)

plot.draw()