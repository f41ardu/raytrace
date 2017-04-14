c        
        subroutine histinit(inr,xa,xe,hdata)
c
c --- Histogramm inr loeschen und vorbereiten
c
        implicit none
        integer i,j,inr,hdata
        real*8 xa,xe
        include 'vert.inc'
c
c     ndata niemals groesser 1000, sonst    
c --- nur veraendern, wenn die felder im common mitgeandert werden
c
        ndata = 1000
        ianz = 10
c
        if(inr.lt.1.or.inr.ge.ianz) then
                
                write(6,*) ' Error: Falsches Histogramm ',inr
                return
        
        endif
                
        pbav(inr) = xa
        pbev(inr) = xe
        pblv(inr) = dble(hdata)
        
        do j=1,ianz
           do i=1,ndata
              iarray(j,i) = 0.d0
           end do
        end do
        
        return 
        end
c        
        subroutine histogram(inr,yhist)

        implicit none

        integer inr,ix
        real*8 yhist
        include 'vert.inc'


        if(inr.lt.1.or.inr.ge.ianz) then
                
                write(6,*) ' Error: Falsches Histogramm ',inr
                return
        
        endif
        
        if(pbav(inr).eq.0.d0.and.pbev(inr).eq.0.d0) then
                write(6,*) ' Error: Histogramm nicht definiert ',inr
                return
        endif
c
c --- index im histogram berechnen
c
        ix = int((pblv(inr)-1.d0)/(pbev(inr)-pbav(inr))*
     .          (yhist-pbav(inr))+1.d0)
c        
        if(ix.ge.0.and.ix.le.int(pblv(inr))) then

                iarray(inr,ix) = iarray(inr,ix) + 1
        else
c hier muesste ein Zahler ergaenz werden, der oberhalb/unterhalb summiert
c            write(6,*) ' Inr_Error : ',inr,'  Histo Bereichsfehler ix= ',ix  
c               
        endif

        return
        end
c
        subroutine histowrite(inr,name)

        implicit none

        integer i,inr
        integer len,clen
        real*8 xhilf
        character*(*) name
        character*12 name1
        
        include 'vert.inc'

        clen = len(name)
        do i=1,clen
           name1(i:i) = name(i:i)
        end do
        open(11,file=name)

        if(inr.lt.1.or.inr.ge.ianz) then
                
                write(6,*) ' Error: Falsches Histogramm ',inr
                return
        
        endif
        
        if(pbav(inr).eq.0.d0.and.pbev(inr).eq.0.d0) then
                write(6,*) ' Error: Histogramm nicht definiert ',inr
                return
        endif
c
c --- index im histogram berechnen
c
        do i=1,int(pblv(inr))
           
           xhilf = dble(i-1)*(pbev(inr)-pbav(inr))/(pblv(inr)
     .                  -1)+pbav(inr)

           write(11,*) real(xhilf),iarray(inr,i)
                                     
        end do
        close(11)
        return
        end
        function histoindex(inr,yhist)

        implicit none

        integer inr,ix,histoindex
        real*8 yhist
        include 'vert.inc'


        if(inr.lt.1.or.inr.ge.ianz) then
                
                write(6,*) ' Error: Falsches Histogramm ',inr
                return
        
        endif
        
        if(pbav(inr).eq.0.d0.and.pbev(inr).eq.0.d0) then
                write(6,*) ' Error: Histogramm nicht definiert ',inr
                return
        endif
c
c --- index im histogram berechnen
c
        ix = int((pblv(inr)-1.d0)/(pbev(inr)-pbav(inr))*
     .          (yhist-pbav(inr))+1.d0)
c        
        if(ix.ge.0.and.ix.le.int(pblv(inr))) then 

                histoindex = ix
        else
                histoindex = -1
        endif

        return
        end


