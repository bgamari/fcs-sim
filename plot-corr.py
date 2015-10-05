#!/usr/bin/python

import sys
import numpy as np
import matplotlib.pyplot as pl
import scipy.integrate
import scipy.optimize

corrs = []
for fname in sorted(sys.argv[1:]):
    print '%-20s\t' % fname,
    a = np.genfromtxt(fname, names='tau,g', dtype=None, invalid_raise=False)
    a['tau'] *= 1e-9
    print a.shape
    if True:
        # log space
        norm = a[0]['g']
        a['g'] -= norm
        pl.semilogx(a['tau'], np.exp(a['g']), '-', alpha=0.1)
    else:
        # linear space
        a = a[np.isfinite(a['g'])]
        norm = a[0]['g']
        a['g'] /= norm
        pl.semilogx(a['tau'], a['g'], '-', alpha=0.1)

    corrs.append(a)
    #pl.semilogx(a['tau'], a['g'], '-')

corrs = np.vstack(corrs[:-1])
corrs['g'] = np.exp(corrs['g'])
#pl.hist2d(np.log10(corrs['tau'].flatten()), corrs['g'].flatten(), bins=(100,100), vmax=300)

mu = np.mean(corrs['g'], axis=0)
err = np.std(corrs['g'], axis=0)
pl.errorbar(corrs['tau'][0], mu, yerr=err, c='0.5')

def f(tau, g0, a, *ds):
    return g0 * sum((1 + tau/d)**-1 * (1 + a**2 * tau / d)**(-1./2) for d in ds)

if False:
    p1,covar = scipy.optimize.curve_fit(f, corrs[0]['tau'], mu, p0=(1, 1, 1e5))
    print p1
    xs = np.logspace(0, 7, 1000)
    pl.plot(xs, f(xs, *p1), 'k')

pl.show()
