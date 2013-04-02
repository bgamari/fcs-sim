#!/usr/bin/python

import numpy as np
from numpy import conj, log
from matplotlib import pyplot as pl

def corr(xs):
    fr = np.fft.rfft(xs)
    s = fr * conj(fr)
    r = np.fft.irfft(s)
    return r / np.mean(xs)

def compute_corr(xs, tau):
    return np.sum(xs * np.roll(xs, tau)) / np.mean(xs)

taus = np.logspace(0, 5, 200)
corrs = []
for i in range(1,100):
    a = np.genfromtxt('h%d'%i)
    #corrs.append(corr(a))
    corrs.append(np.array([compute_corr(a, int(tau)) for tau in taus]))

corrs = np.array(corrs)
g = np.mean(corrs, axis=0)
v = np.std(corrs, axis=0)
#taus = np.arange(len(g))
pl.errorbar(taus, g, yerr=v)
pl.xscale('log')
pl.show()
