	 program raytrace
c     interferometer 
c     raytracing einen laue - bragg Monochromator
c     Idee: ausgehend von einem Punkt S auf einer detektor- oder 
c           filmebenen wird ein gerader strahl bis in die quelle 
c           ueber alle spiegel des interferometers zurueckverfolgt
c           und dann der phasenunterschied zwischen den strahlen 
c           ueber beide wege vergliechen
c     1. Iteration  geometrische Naeherung
c        d.h. keine dynamischen reflexionen keine eindringtiefen 
c             etc. 
c     2. Iteration implementiert (dynamische Theorie) 
c
c        es werden folgende unterprogramme benoetigt 
c        
c        dreh: fuehrt drehung eines punktes um den koordinatenursprung 
c              aus
c        trans: fuehrt eine translation aus
c        flaechen_schnitt: berechnet fuer einen strahl aus der quelle
c           den schnittpunkt mit einer blende, oder den reflektierten 
c           strahl an einem spiegel
c        
c        beschreibung der daten
c
c        ein spiegel oder eine blende wird durch folgende 
c        felder definiert ein aufpunkt und 2 richtungsvektoren
c
c        bi_ : Blenden
c        bi_x(3) Aufpunkt der blende i 
c        bi_u(3) Richtungsvektor 1 des biegers i
c        bi_v(3) Richtungsvektor 2 des biegers i
c        bi_n(3) Orientierungsvektor der oberflaeche n = u x v
c
c        si_ : Spiegel
c        si_x(3) Aufpunkt des spiegels i 
c        si_u(3) Richtungsvektor 1 des spiegels i
c        si_v(3) Richtungsvektor 2 des spiegels i
c        si_n(3) Orientierungsvektor der oberlaeche n = u x v
c        offset(i) beschreibt den offsetwinkel der netzebene zur oberflaeche
c        damit koennen  in  der  oberflaeche h und l netzebene beschrieben werden
c        das entspricht dem asymmetriewinkel der dynamischen theorie
c
c        ein strahl wird beschrieben durch einen 
c        aufpunkt und durch eine richtung 
c        
c        s(3) : aufpunkt 
c        r(3) : richtungsvektor
c
c
c
c
        implicit none
        include 'param.inc'
        integer i,j,l
        integer hit_it,yes,no
        integer number_hits
        integer l_ix,l_ix_det
        integer ilaue,ibragg
        real pi,dtp,ptd
        real t,winkel,m(3,3),xd(3)
        real dp,dpp
c strahlen
        real strahl_p0(3),strahl_a(3)
        real Intensitaet,Sum_Intensitaet
        real neu0_p0(3),neu0_a(3)  
        real neu1_p0(3),neu1_a(3)
        real neu2_p0(3),neu2_a(3)  
        real neu3_p0(3),neu3_a(3)  
        real neu4_p0(3),neu4_a(3)
        real lambda,lambda_x
c blenden
        real b1_px(3),b1_v(3),b1_u(3),b1_n(3)
        real b2_px(3),b2_v(3),b2_u(3),b2_n(3)
c spiegel
        real s1_px(3),s1_v(3),s1_u(3),s1_n(3)
        real s2_px(3),s2_v(3),s2_u(3),s2_n(3)
        real bragg
c
        real s
        real cut,mc_trans
        real offset(10)
        parameter(yes=1,no=0)
c function
        integer histoindex
        real flaechen_schnitt,reflex
c
        open(13,'/home/thomas_old/fortran/raytrace/m1.dat')
        open(12,'/home/thomas_old/fortran/raytrace/m2.dat')
        open(14,'/home/thomas_old/fortran/raytrace/b1.dat')
        open(15,'/home/thomas_old/fortran/raytrace/b2.dat')
c        open(13,'strahl.dat')
         open(30,file='/home/thomas_old/fortran/raytrace/rock_temp.dat')
         open(31,
     +   file='/home/thomas_old/fortran/raytrace/rockd_temp.dat')        
c
111     format(3f12.6,i10)
c
        pi = 4.0*atan(1.0)
        dtp = pi/180.0
        ptd = 180.0/pi
c
c --- init fuer reflex
c
         ilaue = 1
    	 ibragg = 2
        call fspline(12349./1.66289,1)
c
c
c  translation 1. blende
c
        xd(x) = 8900. 
        xd(y) = 0.
        xd(z) = 0.
c
c  erzeugen der 1.blende 
c
        b1_px(x) = -38.5 
        b1_px(y) = -17.0
        b1_px(z) = 0.
c
        b1_v(x) = 77. 
        b1_v(y) = 0.
        b1_v(z) = 0.
c
        b1_u(x) = 0.
        b1_u(y) = 34.
        b1_u(z) = 0.
        t = 90.*dtp
        call matrix(0.0,1.0,0.0,t,m)
c 
c  drehen der 1. blende  
c
        call vektor_kreuzprodukt(b1_u,b1_v,b1_n)
        call dreh(m,b1_px,b1_px)
        call dreh(m,b1_v,b1_v)        
        call dreh(m,b1_u,b1_u)
        call dreh(m,b1_n,b1_n)
c 
c  translation des aufpunkts
c
        call trans(xd,b1_px,b1_px)
c und Kontrollausgabe fuer einen Gnu 3-D PLot
        write(14,*) b1_px(1),b1_px(2),b1_px(3)
c        
        write(14,*) b1_px(1)+b1_v(1),b1_px(2)+b1_v(2),
     .              b1_px(3)+b1_v(3)    
        write(14,*) b1_px(1)+b1_v(1)+b1_u(1),b1_px(2)+b1_v(2)+
     .              b1_u(2),b1_px(3)+b1_v(3)+b1_u(3)    
        write(14,*) b1_px(1)+b1_u(1),b1_px(2)+b1_u(2),
     .              b1_px(3)+b1_u(3)    
c     
        write(14,*) b1_px(1),b1_px(2),b1_px(3)

        close(14)

c
c  translation 2. blende
c
        xd(x) = 18208. 
        xd(y) = 0.
        xd(z) = 0.
c
c  erzeugen der 2.blende 
c
        b2_px(x) = -7.5
        b2_px(y) = -12.
        b2_px(z) =  0.
c
        b2_v(x) = 15.
        b2_v(y) = 0.
        b2_v(z) = 0.
c
        b2_u(x) = 0.
        b2_u(y) = 24.
        b2_u(z) = 0.
        t = 90.*dtp
        call matrix(0.0,1.0,0.0,t,m)
c 
c  drehen der 2. blende  
c
        call vektor_kreuzprodukt(b2_u,b2_v,b2_n)
        call dreh(m,b2_px,b2_px)
        call dreh(m,b2_v,b2_v)        
        call dreh(m,b2_u,b2_u)
        call dreh(m,b2_n,b2_n)
c 
c  translation des aufpunkts
c
        call trans(xd,b2_px,b2_px)
c und Kontrollausgabe fuer einen Gnu 3-D PLot
        write(15,*) b2_px(1),b2_px(2),b2_px(3)
c        
        write(15,*) b2_px(1)+b2_v(1),b2_px(2)+b2_v(2),
     .              b2_px(3)+b2_v(3)    
        write(15,*) b2_px(1)+b2_v(1)+b2_u(1),b2_px(2)+b2_v(2)+
     .              b2_u(2),b2_px(3)+b2_v(3)+b2_u(3)    
        write(15,*) b2_px(1)+b2_u(1),b2_px(2)+b2_u(2),
     .              b2_px(3)+b2_u(3)    
c     
        write(15,*) b2_px(1),b2_px(2),b2_px(3)

        close(15)
c
c --- Monochromatoroffset cut 
c     Laue - Bragg Mono 
        cut = 0.
c
c drehwinkel + drehmatrix  des ersten Kristalls
c
        bragg = 25.6633364 !Si 220 bei 1.66289 Angstr. ohne Brechungkorrektur
        t =(bragg)*dtp
        call matrix(0.0,1.0,0.0,t,m)
c
c  translation 1. Kristall
c
        xd(x) = 29278. 
        xd(y) = 0.
        xd(z) = 10.
c
c  erzeugen des 1. Spiegels (Laue)
c
        s1_px(x) = 0.
        s1_px(y) = -5. 
        s1_px(z) = -20.
c
        s1_v(x) = 0.
        s1_v(y) = 10.
        s1_v(z) = 0.
c
        s1_u(x) = 0.
        s1_u(y) = 0.
        s1_u(z) = 20.
c 
c  drehen des 1. kristall  
c
        call vektor_kreuzprodukt(s1_u,s1_v,s1_n)
        
        call dreh(m,s1_px,s1_px)
        call dreh(m,s1_v,s1_v)        
        call dreh(m,s1_u,s1_u)
        call dreh(m,s1_n,s1_n)
c 
c  translation des aufpunkts
c
        call trans(xd,s1_px,s1_px)
c
c und Kontrollausgabe fuer einen Gnu 3-D PLot
        write(13,*) s1_px(1),s1_px(2),s1_px(3)
c        
        write(13,*) s1_px(1)+s1_v(1),s1_px(2)+s1_v(2),
     .              s1_px(3)+s1_v(3)    
        write(13,*) s1_px(1)+s1_v(1)+s1_u(1),s1_px(2)+s1_v(2)+
     .              s1_u(2),s1_px(3)+s1_v(3)+s1_u(3)    
        write(13,*) s1_px(1)+s1_u(1),s1_px(2)+s1_u(2),
     .              s1_px(3)+s1_u(3)    
c     
        write(13,*) s1_px(1),s1_px(2),s1_px(3)


c
c
c  translation 2. Kristall
c
        xd(x) = 29278. 
        xd(y) = 0.
        xd(z) = 10.
c
c  erzeugen des 2. 
c
        s2_px(x) = 0.
        s2_px(y) = -5.
        s2_px(z) = 0.
c         
        s2_v(x) = 50.
        s2_v(y) = 0.
        s2_v(z) = 0.
c
        s2_u(x) = 0.
        s2_u(y) = 10.
        s2_u(z) = 0.
c 
c  drehen des 2. kristalls 
        t = (bragg)*dtp
        call matrix(0.0,1.0,0.0,t,m)
c
        call vektor_kreuzprodukt(s2_u,s2_v,s2_n)
        call dreh(m,s2_px,s2_px)
        call dreh(m,s2_v,s2_v)        
        call dreh(m,s2_u,s2_u)
        call dreh(m,s2_n,s2_n)
c 
c  translation des aufpunkts
c
        call trans(xd,s2_px,s2_px)
c
c
c und Kontrollausgabe fuer einen Gnu 3-D PLot
c
        write(12,*) s2_px(1),s2_px(2),s2_px(3)
c                
        write(12,*) s2_px(1)+s2_v(1),s2_px(2)+s2_v(2),
     .              s2_px(3)+s2_v(3)    
        write(12,*) s2_px(1)+s2_v(1)+s2_u(1),s2_px(2)+s2_v(2)+
     .              s2_u(2),s2_px(3)+s2_v(3)+s2_u(3)    
        write(12,*) s2_px(1)+s2_u(1),s2_px(2)+s2_u(2),
     .              s2_px(3)+s2_u(3)    
c     
        write(12,*) s2_px(1),s2_px(2),s2_px(3)
c
c        
        close(11)
        close(12)
c
c ab hier kommen die Strahlen
c
        call histinit(1,1.66,1.666,50)
       do dp = -15.,15.,.5        
        dpp = dp/3600.
        Sum_Intensitaet = 0.
        number_hits = 0
c
c  translation 1. Kristall
c
        xd(x) = 29278. 
        xd(y) = 0.
        xd(z) = 10.
c
c  erzeugen des 1. Spiegels (Laue)
c
        s1_px(x) = 0.
        s1_px(y) = -5. 
        s1_px(z) = -20.
c
        s1_v(x) = 0.
        s1_v(y) = 10.
        s1_v(z) = 0.
c
        s1_u(x) = 0.
        s1_u(y) = 0.
        s1_u(z) = 20.
c 
c  drehen des 1. kristall  
c
       t =(bragg+dpp)*dtp
        call matrix(0.0,1.0,0.0,t,m)
c
        call vektor_kreuzprodukt(s1_u,s1_v,s1_n)
        
        call dreh(m,s1_px,s1_px)
        call dreh(m,s1_v,s1_v)        
        call dreh(m,s1_u,s1_u)
        call dreh(m,s1_n,s1_n)
c 
c  translation des aufpunkts
c
        call trans(xd,s1_px,s1_px)

c 
c --- hier gehts los
c        
         do i = 1,200000 
c       
            call strahl_generieren(strahl_p0,strahl_a,lambda_x)
            lambda = 1.66289 + (lambda_x-0.5)*1.e-1 !1.e-3
c            l_ix = histoindex(1,lambda)
c            l_ix_det = histoindex(2,lambda)
c blende 1
            hit_it = flaechen_schnitt(strahl_p0,strahl_a,b1_px,
     .            b1_v,b1_u,b1_n,neu0_p0,neu0_a,winkel)
            if(hit_it.eq.no) goto 100 
c blende 2
            hit_it = flaechen_schnitt(neu0_p0,neu0_a,b2_px,
     .            b2_v,b2_u,b2_n,neu1_p0,neu1_a,winkel)
             if(hit_it.eq.no) goto 100 
c
c 1. monochromatorspiegel
c
            hit_it = flaechen_schnitt(neu1_p0,neu1_a,s1_px,
     .            s1_v,s1_u,s1_n,neu2_p0,neu2_a,winkel)
c
c --- sonderbehandlung laue
c     
            do l = 1,3
               neu2_a(l) = -neu2_a(l)
            end do
            winkel = pi/2. - winkel
c             
            if(hit_it.eq.no) goto 100 
            winkel = winkel*ptd
            intensitaet = reflex(lambda,winkel,ilaue,.7)
c            write(6,*) winkel,intensitaet
            if(intensitaet. eq. 0. ) goto 100
c
c 2. monochromatorspiegel
c
            hit_it = flaechen_schnitt(neu2_p0,neu2_a,s2_px,
     .            s2_v,s2_u,s2_n,neu3_p0,neu3_a,winkel)
c          
            if(hit_it.eq.no) goto 100 
            winkel = winkel*ptd 
            intensitaet = intensitaet
     .               *reflex(lambda,winkel,ibragg,10.)
c            write(6,*) winkel,intensitaet 
            if(intensitaet. eq. 0. ) goto 100
c
              Sum_Intensitaet = Sum_Intensitaet + 
     .                                Intensitaet
              number_hits = number_hits + 1

         call histogram(1,lambda)  
100      continue
c        
         end do
c
         write(30,111) dp,bragg+dpp,Sum_intensitaet,
     .                 number_hits
c        write(6,*) dp
        end do
        close(30)
c
c --- ausgaben der Verteilungen 
c
        call histowrite(1,'l_ver.dat')
c        
	end
	stop