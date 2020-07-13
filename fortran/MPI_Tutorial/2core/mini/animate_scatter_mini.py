import matplotlib.pyplot as plt
import matplotlib.animation as animation
import numpy as np

def main():
    file1 = open('data1.txt', 'r') 
    Lines = file1.readlines() 

    x,y = [],[]  
    count = 0
    for line in Lines:
        nline = line.strip()
        comma_pos = nline.find(',') 
        x_temp = nline[:comma_pos]
        y_temp = nline[comma_pos+1:]
        x.append(float(x_temp))
        y.append(float(y_temp))
        count+=1

    fig = plt.figure()
    ax = fig.add_subplot(111)
    ax.set_xlim((-50,50))
    ax.set_ylim((-50,50))
    particle, = plt.plot([],[], marker='o', color='r')
    traj, = plt.plot([],[], color='r', alpha=0.5)

    def update(i):
        particle.set_data(x[i],y[i])
        traj.set_data(x[:i+1],y[:i+1])
        return particle,traj

    ani = animation.FuncAnimation(fig, update, frames=range(count), interval=1)
    plt.show()

if __name__ == "__main__":
    main()