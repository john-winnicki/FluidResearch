import matplotlib.pyplot as plt
import matplotlib.animation as animation
import numpy as np

def main():
    file1 = open('particle_locations.txt', 'r') 
    Lines = file1.readlines() 

    x,y,color = [],[],[]  
    count = 0
    for line in Lines:
        x_temp = line[:10].strip()
        y_temp = line[12:21].strip()
        color_temp = int(line[22:].strip())

        x.append(float(x_temp))
        y.append(float(y_temp))

        # print(color_temp)

        if(color_temp==0):
            color.append('red')
        else:
            color.append('blue')
        # print(color[count])
        count+=1
    
    fig = plt.figure()
    ax = fig.add_subplot(111)
    ax.set_xlim((min(x),max(x)))
    ax.set_ylim((min(y),max(y)))
    particle, = plt.plot([],[], marker='o', color='r')
    traj, = plt.plot([],[], color='r', alpha=0.5)

    def update(i):
        particle.set_data(x[i],y[i])
        particle.set_color(color[i])
        traj.set_data(x[:i+1],y[:i+1])
        traj.set_color(color[i])
        return particle,traj

    ani = animation.FuncAnimation(fig, update, frames=range(count), interval=1)
    plt.show()

if __name__ == "__main__":
    main()