        function skalar_produkt(vektor1,vektor2)
c
c ---   vektor skalarproduk
c
        implicit none
c
        include 'param.inc'
        real vektor1(3),vektor2(3),skalar_produkt
        
        skalar_produkt = 0
        skalar_produkt = vektor1(x)*vektor2(x)+
     .                   vektor1(y)*vektor2(y)+
     .                   vektor1(z)*vektor2(z)
        return 
        end 
c
        
        subroutine vektor_addition(vektor1,vektor2,vektorout)
c
c ---   vektor addition
c
        implicit none
c
        include 'param.inc'
        integer iloop
        real vektor1(3),vektor2(3),vektorout(3)

        do iloop = 1,3
                vektorout(iloop) = vektor1(iloop) + vektor2(iloop)
        end do
        return 
        end 
c
        subroutine vektor_subtraktion(vektor1,vektor2,vektorout)
c
c ---   vektor subtraktion
c
        implicit none
c
        include 'param.inc'
        integer iloop
        real vektor1(3),vektor2(3),vektorout(3)

        do iloop = 1,3
                vektorout(iloop) = vektor1(iloop) - vektor2(iloop)
        end do
        return 
        end 
c
        subroutine vektor_kreuzprodukt(vektor1,vektor2,vektorout)
c
c ---   vektor kreuzprodukt
c
        implicit none
c
        include 'param.inc'
        real vektor1(3),vektor2(3),vektorout(3)

        vektorout(x) = vektor1(y)*vektor2(z)  - vektor1(z)*vektor2(y)
        vektorout(y) = vektor1(z)*vektor2(x)  - vektor1(x)*vektor2(z)
        vektorout(z) = vektor1(x)*vektor2(y)  - vektor1(y)*vektor2(x)
        return 
        end 
c
        subroutine vektor_mit_skalar(skalar,vektorin,vektorout)
c
c ---   multilpikation eines Vektors mit einem Skalar
c
        implicit none 
c
        include 'param.inc'
        integer iloop
        real vektorin(3),vektorout(3),skalar
        do iloop = 1,3
           vektorout(iloop) = skalar*vektorin(iloop)
        end do
        return 
        end
c
        function vektor_laenge(vektor)
c
c --- Laenge eines Vektors
c
        implicit none
c
        include 'param.inc'
        real vektor(3),vektor_laenge

        vektor_laenge = sqrt(vektor(x)**2+vektor(y)**2+vektor(z)**2)
        return 
        end
c
        subroutine vektor_norm(vektor,norm)
c
c ---   normierter Vektor
c
        implicit none
c
        include 'param.inc'
        integer iloop
        real vektor(3),norm(3),vektor_laenge

        vektor_laenge = sqrt(vektor(x)**2+vektor(y)**2+vektor(z)**2)
        do iloop = 1,3
                norm(iloop) = vektor(iloop)/vektor_laenge
        end do
        return 
        end
c
        function winkel(vektor1,vektor2)
c
c --- Winkel zwischen den Vektoren w,v uebers Skalarprodukt
c
        implicit none
c
        include 'param.inc'
        real vektor1(3),vektor2(3),v1,v2,winkel

        v1 = sqrt(vektor1(x)**2+vektor1(y)**2+vektor1(z)**2) 
        v2 = sqrt(vektor2(x)**2+vektor2(y)**2+vektor2(z)**2) 

        winkel= acos((vektor1(x)*vektor2(x)+
     .                vektor1(y)*vektor2(y)+
     .                vektor1(z)*vektor2(z))/(v1*v2))

        return
        end
c
        subroutine punkt_einer_geraden(P0,a,skalar,punkt)
c
c --- punkt einer geraden aus aufpunkt, richtungsvektor und skalar
c
        implicit none
c
        include 'param.inc'
        real punkt(3),P0(3),a(3),skalar

        punkt(x) = P0(x) + a(x)*skalar
        punkt(y) = P0(y) + a(y)*skalar
        punkt(z) = P0(z) + a(z)*skalar
        return 
        end
c
        function flaechen_schnitt(type,strahl_p0,strahl_a,
     .                            s_x,s_v,s_u,s_n,
     .                            reflex_p0,reflex_a,angle)
c
c ---   prueft ob es einen schnittpunkt des aktuellen strahls mit 
c       der angew„hlten fl„che gibt, berechnet den schnittpunkt  
c       mit der fl„chen und generiert den neuen strahl, der von dieser fl„che 
c       ausgeht
c
        implicit none
        include 'param.inc'
        integer i,j
        integer type
        integer flaechen_schnitt
        real k,n,m
        real strahl_p0(3),strahl_a(3)
        real reflex_p0(3),reflex_a(3)
        real s_x(3),s_u(3),s_v(3),s_n(3),help(3)
        real verbindung(3)                               

        real projektion,s,angle

c       function
        real skalar_produkt,winkel

        flaechen_schnitt = -1

       
        
        projektion = skalar_produkt(strahl_a,s_n)
        
        if(abs(projektion).ge.1.e-8) then 
                 flaechen_schnitt = 1
        else 
c es gibt keinen
                 flaechen_schnitt = 0
                 return
        endif
        
        call vektor_subtraktion(strahl_p0,s_x,verbindung)

        s =  -skalar_produkt(verbindung,s_n)/projektion

        
        call punkt_einer_geraden(strahl_p0,strahl_a,s,reflex_p0) 
        
        flaechen_schnitt = 0
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
                              flaechen_schnitt = 1
                              goto 100
                        endif
                      endif
                     endif
                  endif
            end do
        end do 
100     if(flaechen_schnitt.eq.0) return
c
c winkel zwischen einfallendem strahl und spiegelnder ebene
c reflektierter strahl        
c        
        k = skalar_produkt(s_n,strahl_a)/skalar_produkt(s_n,s_n)
        call vektor_mit_skalar(-2.*k,s_n,verbindung)
        call vektor_addition(strahl_a,verbindung,reflex_a)
c        
        call vektor_mit_skalar(-k,s_n,verbindung)
        call vektor_addition(strahl_a,verbindung,help) 
        angle = winkel(strahl_a,help)
c
        if(type.eq.1) then 
             do i = 1,3
                reflex_a(i) =  strahl_a(i)
             end do
        endif 
                   
        return
        end
c
c
        function flaechen_lb(strahl_p0,strahl_a,
     .                            s_x,s_v,s_u,s_n,s_n1,
     .                            reflex_p0,reflex_a,angle)
c
c ---   prueft ob es einen schnittpunkt des aktuellen strahls mit 
c       der angewaehlten flaeche gibt, berechnet den schnittpunkt  
c       mit der flaechen und generiert den neuen strahl, der von dieser flaeche 
c       ausgeht
c
        implicit none
        include 'param.inc'
        integer i,j
        integer flaechen_lb
        real k,n,m
        real strahl_p0(3),strahl_a(3)
        real reflex_p0(3),reflex_a(3)
        real s_x(3),s_u(3),s_v(3),s_n(3),s_n1(3),help(3)
        real verbindung(3)                               
        real projektion,s,angle
c       function
        real skalar_produkt,winkel
c --- init
        flaechen_lb = -1
c
        projektion = skalar_produkt(strahl_a,s_n)
c        
        if(abs(projektion).ge.1.e-8) then 
                 flaechen_lb = 1
        else 
c es gibt keinen
                 flaechen_lb = 0
                 return
        endif
c        
        call vektor_subtraktion(strahl_p0,s_x,verbindung)
c
        s =  -skalar_produkt(verbindung,s_n)/projektion
c        
        call punkt_einer_geraden(strahl_p0,strahl_a,s,reflex_p0) 
c        
        flaechen_lb = 0
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
                              flaechen_lb = 1
                              goto 100
                        endif
                      endif
                     endif
                  endif
            end do
        end do 
100     if(flaechen_lb.eq.0) return
c
c winkel zwischen einfallendem strahl und spiegelnder ebene
c             ---> reflektierter strahl        
c        
        k = skalar_produkt(s_n1,strahl_a)/skalar_produkt(s_n1,s_n1)
        call vektor_mit_skalar(-2.*k,s_n1,verbindung)
        call vektor_addition(strahl_a,verbindung,reflex_a)
c        
        call vektor_mit_skalar(-k,s_n1,verbindung)
        call vektor_addition(strahl_a,verbindung,help) 
        angle = winkel(strahl_a,help)
c
        return
        end
c
