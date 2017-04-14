        subroutine strahl_generieren(strahl_p0,strahl_a,lambda)
c
c --- erzeugt einen Strahl in der Quelle 
c     mit DORIS Parametern G3
        implicit none
        real strahl_p0(3),strahl_a(3),rd
        real div_y,div_z,hdiv,vdiv,lambda
c function's
        real gasdev,ran3
c 
c --- diverse divergenzparameter der maschine
c
        div_y =   1.067
        div_z =   0.456
        hdiv =   0.421*10e-3
        vdiv =   0.0273*10e-3
c
        strahl_p0(1) = 0. 
        strahl_p0(2) = gasdev(0)*div_y 
        strahl_p0(3) = gasdev(0)*div_z 
c        
        strahl_a(1) = 1. 
        strahl_a(2) = gasdev(0)*hdiv 
        strahl_a(3) = gasdev(0)*vdiv 
        lambda = ran3(0)
c
        return 
        end
