import matplotlib.pyplot as plt
import random

# Python code to 
# demonstrate readlines()  
  
# Using readlines() 
file1 = open('data1.txt', 'r') 
Lines = file1.readlines() 

x,y = [],[]  
count = 0
# Strips the newline character 
for line in Lines:
	nline = line.strip()
	comma_pos = nline.find(',') 
	x_temp = nline[:comma_pos]
	y_temp = nline[comma_pos+1:]
	x.append(float(x_temp))
	y.append(float(y_temp))
	count+=1

# s_size = int(count/2)
# delta_sample = random.sample(range(count),s_size)

# new_x,new_y = [],[]
# for i in delta_sample:
# 	print(i)
# 	new_x.append(x[i])
# 	new_y.append(y[i])

plt.scatter(x,y)
plt.show()