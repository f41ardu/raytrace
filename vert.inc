c
c --- commonvariablen fuer Histogrammer
c
        integer ianz,ndata
c   pblv = dble(ndata) 1000 Arrayelemente
c   8 Histogramme 
        real*8 pbav(10),pbev(10),pblv(10)  
        integer iarray(10,1000)
c
        common/hist/pbav,pbev,pblv,iarray,ianz,ndata

