# -*- coding: utf-8 -*-
"""
Created on Mon Jul 31 15:45:17 2017

@author: Martin Vasilev
"""

import os

if not os.path.exists('Design'):
    os.makedirs('Design')

ID= 1
nsent= 120
ncond= 3
npos= 4 # num of target word positions
nlist= 3 # number of fully counter-balanced lists

full_list= npos*ncond
nsub= nlist*full_list

item= range(1,nsent+1)

S1= [1, 4, 7, 10, 13, 16, 19, 22, 25, 28, 31, 34, 37, 40, 43, 46]
S2= [2, 5, 8, 11, 14, 17, 20, 23, 26, 29, 32, 35, 38, 41, 44, 47]
S3= [3, 6, 9, 12, 15, 18, 21, 24, 27, 30, 33, 36, 39, 42, 45, 48]

P1= [1, 2, 3, 13, 14, 15, 25, 26, 27, 37, 38, 39]
P2= [4, 5, 6, 16, 17, 18, 28, 29, 30, 40, 41, 42]
P3= [7, 8, 9, 19, 20, 21, 31, 32, 33, 43, 44, 45]
P4= [10, 11, 12, 22, 23, 24, 34, 35, 36, 46, 47, 48]


if ID in S1:
	sound= ["SLC"]*int((nsent/ncond)) + ["STD"]*int((nsent/ncond)) + ["DEV"]*int((nsent/ncond))	
	
if ID in S2:
	sound= ["STD"]*int((nsent/ncond)) + ["DEV"]*int((nsent/ncond)) + ["SLC"]*int((nsent/ncond))	
	
if ID in S3:
	sound= ["DEV"]*int((nsent/ncond)) + ["SLC"]*int((nsent/ncond)) + ["STD"]*int((nsent/ncond))	

####

if ID in P1:
	pos= [2, 3, 4, 5]* int(nsent/npos)

if ID in P2:
	pos= [3, 4, 5, 2]* int(nsent/npos)
	
if ID in P3:
	pos= [4, 5, 2, 3]* int(nsent/npos)
	
if ID in P4:
	pos= [5, 2, 3, 4]* int(nsent/npos)
	

c= list(zip(item, sound, pos))
matching = [s for s in c if s[1]=="SLC"]
not_matching= [s for s in c if s[1]!="SLC"]

STD = [i for i, s in enumerate(not_matching) if 'STD' in s[1]]
first= [not_matching[STD[0]]]+ [not_matching[STD[1]]]+ [not_matching[STD[2]]]
del not_matching[STD[0]]
del not_matching[STD[1]-1]
del not_matching[STD[2]-2]

# randomise sounds' block:
from random import shuffle
shuffle(not_matching)
shuffle(matching)

pract= [(121, 'PRAC', 1), (122, 'PRAC', 1), (123, 'PRAC', 1), \
        (124, 'PRAC', 1), (125, 'PRAC', 1), (126, 'PRAC', 1)]
shuffle(pract)
	
if ID%2==1: # odd numbers
	B1= matching
	B2= not_matching
	design= pract+ B1+ first+B2
else: # even numbers
	B1= not_matching
	B2= matching
	design= pract+first+B1+B2
#print(sound)
#print(pos)
print(design)

thefile = open('Design/P'+ str(ID)+ '.txt', 'w')
thefile.write("item sound pos\n") # columns

for item in design:
  thefile.write("%s %s %s\n" % item)
thefile.close()


# questions:
# 13 between 1-40, 13 between 41-80, 13 between 81-120
	