        subroutine braggit(x1,x2,c1,c2,a0,ah)
c
c --- der braggfall 
c       
        complex x1,x2,c1,c2,a0,ah
        complex a(2,2),z(2),b(2)
        real rcond
        integer ipvt(2)
        integer i
c
        a(1,1) = (1.,0.)
        a(1,2) = (1.,0.)
c
        a(2,1) = c1*x1
        a(2,2) = c2*x2
c
        call cgeco(a,2,2,ipvt,rcond,z)
c        
        if(rcond+1. .eq. 1.) then 
                write(6,*) 'Singulaere Matrix '
                return
        endif
c        
        b(1) = (1.,0.)
c        
        do i = 2,2
                b(i) = (0.,0.)
        end do
c
        call cgesl(a,2,2,ipvt,b,0)
c
        a0 = c1*b(1) + c2*b(2)
c        
        ah = (x1*b(1) + x2*b(2)) 
c        
        return
        end
c
        subroutine laueit(x1,x2,c1,c2,a0,ah)
c
c --- der lauefall
c       
        complex x1,x2,c1,c2,a0,ah
        complex a(2,2),z(2),b(2)
        real rcond
        integer ipvt(2)
        integer i
c
        a(1,1) = (1.,0.)
        a(1,2) = (1.,0.)
c
        a(2,1) = x1
        a(2,2) = x2
c
        call cgeco(a,2,2,ipvt,rcond,z)
c        
        if(rcond+1. .eq. 1.) then 
                write(6,*) 'Singulaere Matrix '
                return
        endif
c        
        b(1) = (1.,0.)
c        
        do i = 2,2
                b(i) = (0.,0.)
        end do
c
        call cgesl(a,2,2,ipvt,b,0)
c
        a0 = c1*b(1) + c2*b(2)
c        
        ah = c1*x1*b(1) + c2*x2*b(2) 
c        
        return
        end
