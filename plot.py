import tables
import numpy as np
import pylab as pl

class laplace(object):
    def __init__(self, iout=100, type_="seq"):
        self.iout  = iout
        self.fname = "data_" + type_ + "_%06d" %iout

    def read(self):
        f = tables.openFile(self.fname)
        self.data = f.root.array[:].transpose()
        f.close()

    def plot(self, log=False, show=True, *args, **kwargs):
        if not log:
            pl.imshow(self.data[:,:,0], *args, **kwargs)
        else:
            pl.imshow(np.log10(self.data[:,:,0]), *args, **kwargs)
        pl.colorbar()
        if show:
            pl.show()

def compare(iout=100, compList=['seq', 'omp', 'oacc', 'cuda'], ref='seq', log=False, *args, **kwargs):
    data = {}
    for type_ in compList:
        data[type_] = laplace(iout, type_)
        data[type_].read()
    if len(compList) == 2: pl.figure(figsize=(14,3))
    if len(compList) == 3: pl.figure(figsize=(10,5))
    if len(compList) == 4: pl.figure(figsize=(14,5))
    for i, key in enumerate(data):
        if len(compList) == 2:
            pl.subplot(1,len(compList)+1,i+1)
            pl.title(key)
            if log:
                pl.imshow(np.log10(data[key].data[:,:,0]), *args, **kwargs)
            else:
                pl.imshow(data[key].data[:,:,0], *args, **kwargs)
        else:
            pl.subplot(2,len(compList),i+1)
            pl.title(key)
            if log:
                pl.imshow(np.log10(data[key].data[:,:,0]), *args, **kwargs)
            else:
                pl.imshow(data[key].data[:,:,0], *args, **kwargs)
            if i+1 == len(compList):
                pl.colorbar()
            pl.subplot(2,len(compList),len(compList)+i+1)
            if log:
                pl.imshow(np.log10(data[key].data[:,:,0]/data[ref].data[:,:,0]), *args, **kwargs)
            else:
                pl.imshow(data[key].data[:,:,0]/data[ref].data[:,:,0], *args, **kwargs)
            if i+1 == len(compList):
                pl.colorbar()
    if len(compList) == 2:
        pl.subplot(1,len(compList)+1,3)
        compList.remove(ref)
        other = compList[0]
        if log:
            pl.imshow(np.log10(data[other].data[:,:,0]/data[ref].data[:,:,0]), *args, **kwargs)
        else:
            pl.imshow(data[other].data[:,:,0]/data[ref].data[:,:,0], *args, **kwargs)
        pl.colorbar()
    pl.show()
