# -*- coding: utf-8 -*-
"""
Created on Mon Jul 31 15:45:17 2017

@author: Martin Vasilev
"""

import os

if not os.path.exists('Design'):
    os.makedirs('Design')

ID= 30
nsent= 120
ncond= 3
npos= 5 # num of target word positions
nlist= 3 # number of fully counter-balanced lists

full_list= npos*ncond
nsub= nlist*full_list

item= range(1,nsent+1)

S1= [1, 4, 7, 10, 13, 16, 19, 22, 25, 28]
S2= [2, 5, 8, 11, 14, 17, 20, 23, 26, 29]
S3= [3, 6, 9, 12, 15, 18, 21, 24, 27, 30]

P1= [1, 2, 3, 16, 17, 18]
P2= [4, 5, 6, 19, 20, 21]
P3= [7, 8, 9, 22, 23, 24]
P4= [10, 11, 12, 25, 26, 27]
P5= [13, 14, 15, 28, 29, 30]

#import numpy
#design= numpy.zeros((nsent,full_list))

#d = [[[], [], []] for x in xrange(nsent)]
d = [[] for x in xrange(nsent)]
d= []

if ID in S1:
	sound= ["SLC"]*int((nsent/ncond)) + ["STD"]*int((nsent/ncond)) + ["DEV"]*int((nsent/ncond))	
	
if ID in S2:
	sound= ["STD"]*int((nsent/ncond)) + ["DEV"]*int((nsent/ncond)) + ["SLC"]*int((nsent/ncond))	
	
if ID in S3:
	sound= ["DEV"]*int((nsent/ncond)) + ["SLC"]*int((nsent/ncond)) + ["STD"]*int((nsent/ncond))	

####

if ID in P1:
	pos= [1, 2, 3, 4, 5]* int(nsent/npos)

if ID in P2:
	pos= [2, 3, 4, 5, 1]* int(nsent/npos)
	
if ID in P3:
	pos= [3, 4, 5, 1, 2]* int(nsent/npos)
	
if ID in P4:
	pos= [4, 5, 1, 2, 3]* int(nsent/npos)
	
if ID in P5:
	pos= [5, 1, 2, 3, 4]* int(nsent/npos)
	

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


# find 'silence' items:
#matching = [s for s in sound if "SLC" in s]
#not_matching= [s for s in sound if "SLC" not in s] 
	
if ID%2==1: # odd numbers
	B1= matching
	B2= not_matching
	design= B1+ first+B2
else: # even numbers
	B1= not_matching
	B2= matching
	design= first+B1+B2
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
	