# -*- coding: utf-8 -*-
"""
Created on Tue Sep 18 08:36:06 2018

@author: jyli
"""
import pandas as pd
import numpy as np
import matplotlib.pyplot as plt
from scipy.interpolate import interp2d
filename = '2.3MW/steady_states.txt'



def loadData(filename):
    data = []
    with open(filename) as f:
        f.readline()
        keys = [x.strip() for x in f.readline().split(',')]
        
        for line in f:
            if 'NaN' in line:
                continue
            floatdata = [float(x) for x in line.split()]
            if floatdata[1] < 0: #if negative rotorspeed
                continue
            data.append(floatdata)
        
    return pd.DataFrame(data, columns=keys)



def plot_Cp(data):
    pitches = list(set(data.pitch_in))
    pitches.sort()
    
    plt.figure()
    plt.xlabel('TSR')
    plt.ylabel('Cp')
    color_idx = np.linspace(0, 1, len(pitches))
    for pitch, i in zip(pitches, color_idx):
        condition = data.pitch_in==pitch
        plt.plot(data.TSR[condition], data.CT[condition], c=plt.cm.winter(i), label=str(pitch))
    
    plt.legend()



        
def contourplot(data, xkey, ykey, zkey, N = 50): 

    x = np.linspace(data[xkey].min(), data[xkey].max(), N)
    y = np.linspace(data[ykey].min(), data[ykey].max(), N)
    
    f = interp2d(data[xkey], data[ykey], data[zkey], kind='linear')
    z = f(x, y)
    z[z<=0.1] = 0
    z[z>1] = 1

    
    
    plt.figure()    
    plt.plot(data[xkey], data[ykey], '.k', ms=1)    
    x, y = np.meshgrid(x, y)
    plt.contourf(x,y,z, cmap='Oranges')
    cb = plt.colorbar()
    CS = plt.contour(x, y, z, colors='0.3', linewidths=0.6)
    

    plt.gca().clabel(CS, CS.levels, inline=True, fmt='%1.2f', fontsize=8)
    plt.xlabel(xkey)
    plt.ylabel(ykey)    
    cb.set_label(zkey)
    
    
    
    
    
if __name__ == '__main__':
    data = loadData(filename)
    
    contourplot(data, 'pitch_in', 'TSR', 'CP')
    contourplot(data, 'pitch_in', 'TSR', 'CT')
    contourplot(data, 'pitch_in', 'K', 'CP')
    contourplot(data, 'pitch_in', 'K', 'CT')

    
