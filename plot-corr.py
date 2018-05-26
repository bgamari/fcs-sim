#!/usr/bin/env nix-shell
#!nix-shell -p python3Packages.numpy python3Packages.matplotlib python3Packages.scipy python3Packages.pyqt5 -i python3

import sys
import numpy as np
import scipy.integrate
import scipy.optimize
import argparse

parser = argparse.ArgumentParser()
parser.add_argument('-o', '--output', type=argparse.FileType('w'), help='Output path')
parser.add_argument('-O', '--corr-output', type=argparse.FileType('w'), help='Aggregate correlation output path')
parser.add_argument('-f', '--fit', action='store_true')
parser.add_argument('files', nargs='+', type=argparse.FileType('r'), help='Input correlations')
args = parser.parse_args()

import matplotlib
if args.output:
    matplotlib.use('Agg')
else:
    matplotlib.use('Qt5Agg')
import matplotlib.pyplot as pl

corrs = []
norms = []
fnames = sorted(f.name for f in args.files)
expectedLen = len(np.genfromtxt(fnames[0], names='tau,g', dtype=float, invalid_raise=False))
for fname in fnames:
    print('%-20s\t' % fname,)
    a = np.genfromtxt(fname, names='tau,g', dtype=float, invalid_raise=False)
    if len(a) != expectedLen:
        print('wrong len')
        continue
    if not np.all(np.isfinite(a['g'])):
        print('NaN')
        continue

    a['tau'] *= 1e-9
    if False:
        # log space
        norm = a[0]['g']
        if np.exp(norm) == 0:
            print('zeros')
            continue
        a['g'] -= norm
    else:
        # linear space
        print(a['g'][0]**2)
        a = a[np.isfinite(a['g'])]
        norm = a[0]['g']
        norms.append(norm)
        a['g'] /= norm

    if not np.all(np.isfinite(a['g'])):
        print('NaN2')
        continue
    print(a.shape, a['tau'][0], a['tau'][-1])
    corrs.append(a)
    #pl.semilogx(a['tau'], a['g'], '-')

corrs = np.vstack(corrs)
#corrs['g'] *= np.mean(norms)

for corr in corrs:
    pl.semilogx(corr['tau'], corr['g'], '-', alpha=0.2)

pl.xlabel(r'$\tau$ (s)')
pl.ylabel(r'$G(\tau)$')
pl.suptitle('Molecule diffusion inside "sticky" droplet, simulated')

#corrs['g'] = np.exp(corrs['g'])
#pl.hist2d(np.log10(corrs['tau'].flatten()), corrs['g'].flatten(), bins=(100,100), vmax=300)

mu = np.mean(corrs['g'], axis=0)
err = np.std(corrs['g'], axis=0) / np.sqrt(corrs.shape[0])
print(np.nonzero(np.logical_not(np.isfinite(corrs['g']))))
pl.errorbar(corrs['tau'][0], mu, yerr=err, c='k', ecolor='0.5', linewidth=1)
pl.axhline(0, c='k')

if args.corr_output:
    np.savetxt(args.corr_output, np.array([corrs[0]['tau'], mu+1, err**2]).T)

def f(tau, g0, a, *ds):
    return g0 * sum((1 + tau/d)**-1 * (1 + a**2 * tau / d)**(-1./2) for d in ds)

def fAspect(tau, g0, *ds):
    a = 2
    return g0 * sum((1 + tau/d)**-1 * (1 + a**2 * tau / d)**(-1./2) for d in ds)

if args.fit:
    #p0 = (1, 1e-3); fitFunc = fAspect
    p0 = (1, 2, 1e-3); fitFunc = f
    p1,covar = scipy.optimize.curve_fit(fitFunc, corrs[0]['tau'], mu, p0=p0)
    print(p1)
    xs = np.logspace(np.log10(corrs[0][0]['tau']), 1, 1000)
    pl.plot(xs, fitFunc(xs, *p1), 'k')

if args.output:
    pl.savefig(args.output.name)
else:
    pl.show()
