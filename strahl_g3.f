        subroutine strahl_generieren(strahl_p0,strahl_a,lambda)
c
c --- erzeugt einen Strahl in der Quelle 
c     mit DORIS Parametern G3
        implicit none
        real strahl_p0(3),strahl_a(3)
        real div_y,div_z,div_h,div_v,lambda
c function's
        real gasdev,ran3
c 
c --- diverse divergenzparameter der maschine
c
        div_y =   2.012*2.*0.6931
        div_z =   0.694*2.*0.6932
        div_v =   (0.562e-3*2.*0.6931)
        div_h =   (0.030e-3*2.*0.6931)
c
        strahl_p0(1) = 0. 
        strahl_p0(2) = gasdev(0)/2.5*div_y 
        strahl_p0(3) = gasdev(0)/2.5*div_z 
c        
        strahl_a(1) = 1. 
        strahl_a(3) = sin(gasdev(0)/2.5*div_h) 
        strahl_a(2) = sin(gasdev(0)/2.5*div_v) 
        lambda = ran3(0)
c
        return 
        end
