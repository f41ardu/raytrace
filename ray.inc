
c 
c --- common deklarationen f�r ray
c
        real px(nelements,3),u(nelements,3),v(nelements,3)
        real n(nelements,3),offset(nelements)       
        real P0(3),a(3)
c
        common/geometrie/px,u,v,n,offset
        common/strahl/P0,a

