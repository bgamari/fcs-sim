#!/usr/bin/python

import sys
import numpy as np
import matplotlib.pyplot as pl
import scipy.integrate
import scipy.optimize

corrs = []
for fname in sorted(sys.argv[1:])[:-1]:
    print '%-20s\t' % fname,
    a = np.genfromtxt(fname, names='tau,g', dtype=float, invalid_raise=False)
    a['tau'] *= 1e-9
    if len(a) == 0:
        continue
    if True:
        # log space
        norm = a[0]['g']
        if np.exp(norm) == 0:
            print 'zeros'
            continue
        a['g'] -= norm
        pl.semilogx(a['tau'], np.exp(a['g']), '-', alpha=0.1)
    else:
        # linear space
        a = a[np.isfinite(a['g'])]
        norm = a[0]['g']
        a['g'] /= norm
        pl.semilogx(a['tau'], a['g'], '-', alpha=0.1)

    print a.shape
    corrs.append(a)
    #pl.semilogx(a['tau'], a['g'], '-')

corrs = np.vstack(corrs)
corrs['g'] = np.exp(corrs['g'])
#pl.hist2d(np.log10(corrs['tau'].flatten()), corrs['g'].flatten(), bins=(100,100), vmax=300)

mu = np.mean(corrs['g'], axis=0)
err = np.std(corrs['g'], axis=0)
print np.nonzero(np.logical_not(np.isfinite(corrs['g'])))
pl.errorbar(corrs['tau'][0], mu, yerr=err, c='k')

def f(tau, g0, a, *ds):
    return g0 * sum((1 + tau/d)**-1 * (1 + a**2 * tau / d)**(-1./2) for d in ds)

def fAspect(tau, g0, *ds):
    a = 2
    return g0 * sum((1 + tau/d)**-1 * (1 + a**2 * tau / d)**(-1./2) for d in ds)

if True:
    #p0 = (1, 1e-3); fitFunc = fAspect
    p0 = (1, 2, 1e-3); fitFunc = f
    p1,covar = scipy.optimize.curve_fit(fitFunc, corrs[0]['tau'], mu, p0=p0)
    print p1
    xs = np.logspace(np.log10(corrs[0][0]['tau']), 1, 1000)
    pl.plot(xs, fitFunc(xs, *p1), 'r')

pl.show()
