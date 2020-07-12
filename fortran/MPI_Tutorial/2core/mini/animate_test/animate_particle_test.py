import matplotlib.pyplot as plt
import matplotlib.animation as animation
import numpy as np

def main():
    t = np.linspace(0,2*np.pi,100)
    x = np.sin(t)
    y = t**2
    fig = plt.figure()
    ax = fig.add_subplot(111)
    ax.set_xlim((-1.1,1.1))
    ax.set_ylim((0,40))
    particle, = plt.plot([],[], marker='o', color='r')
    traj, = plt.plot([],[], color='r', alpha=0.5)

    def update(i):
        particle.set_data(x[i],y[i])
        traj.set_data(x[:i+1],y[:i+1])
        return particle,traj

    ani = animation.FuncAnimation(fig, update, frames=range(100), interval=25)
    plt.show()

if __name__ == "__main__":
    main()