	program raytrace
c 
c raytracing fuer Laue Bragg Monochromator
c
c 
c
	implicit none
	include 'param.inc'
        integer i,j,l
        integer hit_it,yes,no
	integer number_hits
	integer ilaue,ibragg
	real pi,dtp,ptd
	real t,winkel,m(3,3),xd(3)
	real t_surf,t_net,m_surf(3,3),m_net(3,3)
	real dp,dpp
        real intensitaet,sum_intensitaet
c strahlen
	real strahl_p0(3),strahl_a(3)
	real neu0_p0(3),neu0_a(3)
	real neu1_p0(3),neu1_a(3)
	real neu2_p0(3),neu2_a(3)
	real neu3_p0(3),neu3_a(3)
	real neu4_p0(3),neu4_a(3)
	real neu5_p0(3),neu5_a(3)
        real lambda,lambda_x
c blenden
	real b1_px(3),b1_v(3),b1_u(3),b1_n(3)
        real b2_px(3),b2_v(3),b2_u(3),b2_n(3)
        real b3_px(3),b3_v(3),b3_u(3),b3_n(3)
        real b4_px(3),b4_v(3),b4_u(3),b4_n(3)
        real b5_px(3),b5_v(3),b5_u(3),b5_n(3)
c spiegel
	real s1_px(3),s1_v(3),s1_u(3),s1_n(3),s1_net(3),s1_surf(3)
	real s2_px(3),s2_v(3),s2_u(3),s2_n(3),s2_net(3),s2_surf(3)
	real bragg
c hilfsvariablen
	real s,mc_trans
	parameter(yes=1,no=0)
        real rocking(100)
c function
	real flaechen_schnitt,flaechen_lb,reflex
c
	open(12,"/home/thomas_old/fortran/raytrace/m1.dat")
        open(13,"/home/thomas_old/fortran/raytrace/m2.dat")
        open(14,"/home/thomas_old/fortran/raytrace/b1.dat")
        open(15,"/home/thomas_old/fortran/raytrace/b2.dat")
        open(16,"/home/thomas_old/fortran/raytrace/b3.dat")
        open(17,"/home/thomas_old/fortran/raytrace/b4.dat")
c
        open(30,"/home/thomas_old/fortran/raytrace/rc.dat")
c
111    format(3f12.6,i10)
c
	pi = 4.*atan(1.)
	dtp = pi/180.
	ptd = 180./pi
c
c --- initialisierung fuer den reflex
c
	ilaue = 1
        ibragg = 2
	call fspline(12349./1.66289,1)
c
c --- 1. Blende
c
	xd(x) = 8900.
	xd(y) = 0.
	xd(z) = 0.
c
c ---
c
	b1_px(x) = -38.5
	b1_px(y) = -17.
     	b1_px(z) = 0.
c
	b1_v(x) = 77.
	b1_v(y) = 0.
     	b1_v(z) = 0.
c	
	b1_u(x) = 0.
	b1_u(y) = 34.
     	b1_u(z) = 0.
c
        t = 90.*dtp
	call matrix(0.0,1.0,0.0,t,m)
c
c --- drehen der Blende
c
	call vektor_kreuzprodukt(b1_u,b1_v,b1_n)
	call dreh(m,b1_px,b1_px)
        call dreh(m,b1_v,b1_v)
        call dreh(m,b1_u,b1_u)
        call dreh(m,b1_n,b1_n)
c
c --- tanslation des Aufpunkts
c
	call trans(xd,b1_px,b1_px)
c
c --- Kontrollausgabe (3 - D Gnuplot)
c
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
c --- 2. Blende
c
	xd(x) = 18208.
	xd(y) = 0.
	xd(z) = 0.
c
c ---
c
	b2_px(x) = -7.5
	b2_px(y) = -12.
     	b2_px(z) = 0.
c
	b2_v(x) = 15.
	b2_v(y) = 0.
     	b2_v(z) = 0.
c	
	b2_u(x) = 0.
	b2_u(y) = 24.
     	b2_u(z) = 0.
c
        t = 90.*dtp
	call matrix(0.0,1.0,0.0,t,m)
c
c --- drehen der Blende
c
	call vektor_kreuzprodukt(b2_u,b2_v,b2_n)
	call dreh(m,b2_px,b2_px)
        call dreh(m,b2_v,b2_v)
        call dreh(m,b2_u,b2_u)
        call dreh(m,b2_n,b2_n)
c
c --- tanslation des Aufpunkts
c
	call trans(xd,b2_px,b2_px)
c
c --- Kontrollausgabe (3 - D Gnuplot)
c
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
c --- 3. Blende
c
	xd(x) = 29078.
	xd(y) = 0.
	xd(z) = 0.
c
c ---
c
	b3_px(x) = -1.
	b3_px(y) = -5.
     	b3_px(z) = 0.
c
	b3_v(x) = 2.
	b3_v(y) = 0.
     	b3_v(z) = 0.
c	
	b3_u(x) = 0.
	b3_u(y) = 10.
     	b3_u(z) = 0.
c
        t = 90.*dtp
	call matrix(0.0,1.0,0.0,t,m)
c
c --- drehen der Blende
c
	call vektor_kreuzprodukt(b3_u,b3_v,b3_n)
	call dreh(m,b3_px,b3_px)
	call dreh(m,b3_v,b3_v)
        call dreh(m,b3_u,b3_u)
        call dreh(m,b3_n,b3_n)
c
c --- tanslation des Aufpunkts
c
	call trans(xd,b3_px,b3_px)
c
c --- Kontrollausgabe (3 - D Gnuplot)
c
        write(16,*) b3_px(1),b3_px(2),b3_px(3)
c
        write(16,*) b3_px(1)+b3_v(1),b3_px(2)+b3_v(2),
     .              b3_px(3)+b3_v(3)
        write(16,*) b3_px(1)+b3_v(1)+b3_u(1),b3_px(2)+b3_v(2)+
     .              b3_u(2),b3_px(3)+b3_v(3)+b3_u(3)
        write(16,*) b3_px(1)+b3_u(1),b3_px(2)+b3_u(2),
     .              b3_px(3)+b3_u(3)
c
        write(16,*) b3_px(1),b3_px(2),b3_px(3)

        close(16)
c
c --- 4. Blende
c
	xd(x) = 29078. + 380.
	xd(y) = 0.
	xd(z) = 20.
c
c ---
c
	b4_px(x) = -1.
	b4_px(y) = -5.
     	b4_px(z) = 0.
c
	b4_v(x) = 2.
	b4_v(y) = 0.
     	b4_v(z) = 0.
c	
	b4_u(x) = 0.
	b4_u(y) = 10.
     	b4_u(z) = 0.
c
        t = 90.*dtp
	call matrix(0.0,1.0,0.0,t,m)
c
c --- drehen der Blende
c
	call vektor_kreuzprodukt(b4_u,b4_v,b4_n)
	call dreh(m,b4_px,b4_px)
	call dreh(m,b4_v,b4_v)
        call dreh(m,b4_u,b4_u)
        call dreh(m,b4_n,b4_n)
c
c --- tanslation des Aufpunkts
c
	call trans(xd,b4_px,b4_px)
c
c --- Kontrollausgabe (3 - D Gnuplot)
c
        write(17,*) b4_px(1),b4_px(2),b4_px(3)
c
        write(17,*) b4_px(1)+b4_v(1),b4_px(2)+b4_v(2),
     .              b4_px(3)+b4_v(3)
        write(17,*) b4_px(1)+b4_v(1)+b4_u(1),b4_px(2)+b4_v(2)+
     .              b4_u(2),b4_px(3)+b4_v(3)+b4_u(3)
        write(17,*) b4_px(1)+b4_u(1),b4_px(2)+b4_u(2),
     .              b4_px(3)+b4_u(3)
c
        write(17,*) b4_px(1),b4_px(2),b4_px(3)

        close(17)
c 
c --- Kristalle
c
	bragg = 25.6633364 !Si220 
        t_surf = bragg*dtp
	t_net  = (bragg + 270.)*dtp
        call matrix(0.0,1.0,0.0,t_surf,m_surf)
        call matrix(0.0,1.0,0.0,t_net,m_net)
c
c --- translation
c
	xd(x) = 29278.
	xd(y) = 0.
	xd(z) = 10.
c
c --- 
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
c --- drehen 
c
	call vektor_kreuzprodukt(s1_u,s1_v,s1_n)
c
	call dreh(m_surf,s1_px,s1_px)
	call dreh(m_surf,s1_v,s1_v)
	call dreh(m_surf,s1_u,s1_u)
	call dreh(m_net,s1_n,s1_n)
c
c --- translation
c
	call trans(xd,s1_px,s1_px)
c
	
c --- Kontrollausgabe (3 - D Gnuplot)
c
        write(12,*) s1_px(1),s1_px(2),s1_px(3)
c
        write(12,*) s1_px(1)+s1_v(1),s1_px(2)+s1_v(2),
     .              s1_px(3)+s1_v(3)
        write(12,*) s1_px(1)+s1_v(1)+s1_u(1),s1_px(2)+s1_v(2)+
     .              s1_u(2),s1_px(3)+s1_v(3)+s1_u(3)
        write(12,*) s1_px(1)+s1_u(1),s1_px(2)+s1_u(2),
     .              s1_px(3)+s1_u(3)
c
        write(12,*) s1_px(1),s1_px(2),s1_px(3)

        close(12)
c ---- schnipp
	bragg = 25.6633364 !Si220 
        t_surf = bragg*dtp
	t_net  = (bragg + 180.)*dtp
        call matrix(0.0,1.0,0.0,t_surf,m_surf)
        call matrix(0.0,1.0,0.0,t_net,m_net)
c
c --- translation
c
	xd(x) = 29278.
	xd(y) = 0.
	xd(z) = 10.
c
c --- 
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
c --- drehen 
c
	call vektor_kreuzprodukt(s2_u,s2_v,s2_n)
c
	call dreh(m_surf,s2_px,s2_px)
	call dreh(m_surf,s2_v,s2_v)
	call dreh(m_surf,s2_u,s2_u)
	call dreh(m_net,s2_n,s2_n)
c
c --- translation
c
	call trans(xd,s2_px,s2_px)
c
	
c --- Kontrollausgabe (3 - D Gnuplot)
c
        write(13,*) s2_px(1),s2_px(2),s2_px(3)
c
        write(13,*) s2_px(1)+s2_v(1),s2_px(2)+s2_v(2),
     .              s2_px(3)+s2_v(3)
        write(13,*) s2_px(1)+s2_v(1)+s2_u(1),s2_px(2)+s2_v(2)+
     .              s2_u(2),s2_px(3)+s2_v(3)+s2_u(3)
        write(13,*) s2_px(1)+s2_u(1),s2_px(2)+s2_u(2),
     .              s2_px(3)+s2_u(3)
c
        write(13,*) s2_px(1),s2_px(2),s2_px(3)

        close(13)
c
c --- nun geths los
c
	call histinit(1,-.02,.02,50)
	call histinit(2,-.002,.002,50)
c
	do dp = -5.,15.,1.
c
    	  dpp = dp/3600.
	  sum_intensitaet = 0.
	  number_hits = 0 
          t_surf = (bragg+dpp)*dtp
	  t_net  = (bragg + dpp + 270.)*dtp
          call matrix(0.0,1.0,0.0,t_surf,m_surf)
          call matrix(0.0,1.0,0.0,t_net,m_net)
c
c --- translation
c
	  xd(x) = 29278.
	  xd(y) = 0.
	  xd(z) = 10.
c
c --- 
c
	  s1_px(x) = 0.
	  s1_px(y) = -5.
	  s1_px(z) = -10.
c
	  s1_v(x) = 0.
	  s1_v(y) = 10.
	  s1_v(z) = 0.
c
	  s1_u(x) = 0.
	  s1_u(y) = 0.
	  s1_u(z) = 20.
c	
c --- drehen 
c
	  call vektor_kreuzprodukt(s1_u,s1_v,s1_n)
c
	  call dreh(m_surf,s1_px,s1_px)
	  call dreh(m_surf,s1_v,s1_v)
	  call dreh(m_surf,s1_u,s1_u)
	  call dreh(m_net,s1_n,s1_n)
c
c --- translation
c
	  call trans(xd,s1_px,s1_px)
c
	  do i = 1,2000
c
	    call strahl_generieren(strahl_p0,strahl_a,lambda_x)
	    lambda = 1.66289 + (lambda_x - 0.5)*1.e-3
c
	    call histogram(1,strahl_a(2))
	    call histogram(2,strahl_a(3))
c --- BLENDE 1
	    hit_it = flaechen_schnitt(1,strahl_p0,strahl_a,b1_px,
     .               b1_v,b1_u,b1_n,neu0_p0,neu0_a,winkel)
	    if(hit_it .eq. no) goto 100
c --- BLENDE 2
	    hit_it = flaechen_schnitt(1,neu0_p0,neu0_a,b2_px,
     .               b2_v,b2_u,b2_n,neu1_p0,neu1_a,winkel)
            if(hit_it .eq. no) goto 100
c --- BLENDE 3
	    hit_it = flaechen_schnitt(1,neu1_p0,neu1_a,b3_px,
     .               b3_v,b3_u,b3_n,neu2_p0,neu2_a,winkel)
	    if(hit_it .eq. no) goto 100
c --- 1. Monochromatorspiegel 
	    hit_it = flaechen_schnitt(2,neu2_p0,neu2_a,s1_px,
     .               s1_v,s1_u,s1_n,neu3_p0,neu3_a,winkel)
	    write(6,*) winkel*ptd
100 	    continue	
          end do 

 	end do 
	end



