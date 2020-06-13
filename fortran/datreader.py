import numpy as np
import matplotlib.pyplot as plt

data = np.loadtxt( 'data.dat' )
print(data)
plt.plot(data)
plt.show()
