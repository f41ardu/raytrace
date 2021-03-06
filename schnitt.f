c
        function flaechen_laue(strahl_p0,strahl_a,
     .                            s_x,s_v,s_u,s_surf,s_net,
     .                            reflex_p0,reflex_a,angle)
c
c ---   prueft ob es einen schnittpunkt des aktuellen strahls mit 
c       der angew�hlten fl�che gibt, berechnet den schnittpunkt  
c       mit der fl�chen und generiert den neuen strahl, der von dieser fl�che 
c       ausgeht
c
        implicit none
        include 'param.inc'
        integer i,j
        integer flaechen_laue
        real k,n,m
        real strahl_p0(3),strahl_a(3)
        real reflex_p0(3),reflex_a(3)
        real s_x(3),s_u(3),s_v(3),s_surf(3),s_net(3),help(3)
        real verbindung(3)                               

        real projektion,s,angle

c       function
        real skalar_produkt,winkel

        flaechen_laue = -1

        projektion = skalar_produkt(strahl_a,s_surf)
        
        if(abs(projektion).ge.1.e-8) then 
                 flaechen_laue = 1
        else 
c es gibt keinen
                 flaechen_laue = 0
                 return
        endif
        
        call vektor_subtraktion(strahl_p0,s_x,verbindung)
c pukt auf der geometrischen oberflaeche 
        s =  -skalar_produkt(verbindung,s_surf)/projektion
        
        call punkt_einer_geraden(strahl_p0,strahl_a,s,reflex_p0) 
        
        flaechen_laue = 0
        do i = 1,3
            do j = 3,1,-1
                  if(i .ne. j .and. s_u(j) .ne.0. ) then 
                     n = s_v(i)*s_u(j) - s_v(j)*s_u(i)
                     if(abs(n).ge.1.e-9) then 
                      m = ((reflex_p0(j)-s_x(j))* s_v(i) -
     .                     (reflex_p0(i)-s_x(i))* s_v(j))/
     .                     n
                      if(m .gt. 0. .and. m .lt. 1.) then
                        k = (reflex_p0(i) - s_x(i) - m*s_u(i))/s_v(i)
                        if(k .gt.0. .and. k .lt. 1.) then
                              flaechen_laue = 1
                              goto 100
                        endif
                      endif
                     endif
                  endif
            end do
        end do 
100     if(flaechen_laue.eq.0) return
c
c winkel zwischen einfallendem strahl und spiegelnder ebene
c reflektierter strahl        
c        
        k = skalar_produkt(s_net,strahl_a)/skalar_produkt(s_net,s_net)
        call vektor_mit_skalar(-2.*k,s_net,verbindung)
        call vektor_addition(strahl_a,verbindung,reflex_a)
c        
        call vektor_mit_skalar(-k,s_net,verbindung)
        call vektor_addition(strahl_a,verbindung,help) 
        angle = winkel(strahl_a,help)
c
        return
        end
c
c
