      function reflex(lambda,dt_geom)
c
c      
c --- Berechnung der Reflektionskurven
c     im Zweistrahlfall nach den im Zachariasen  
c     angegebenen Gleichungen (modifiziert) 
c     
c
      implicit none
c      
      complex  chi0,chih,z,x1,x2,d01,d02
      complex  c1,c2,phi1,phi2
      complex DH,D0
      complex a(3),roots(2)
      real IB0,IBH,reflex
      real lambda,pol 
      real gamma0,gammah,b,alpha,theta,dp,k0,t1,asy
      real pi
      real gtheta ,dt_geom
      real h,k,l
c      
      integer imat
      logical laue,bragg,par,sigma
c
      pi = 4.e0*atan(1.e0)  
c      
c --- eingabe parmeter fest verdrahtet      
c
      imat = 1
      asy = 0.
      t1 = 5.
      bragg=.true.
      sigma=.true.
      h = 1      
      k = 1
      l = 1
      call strfac(real(h),real(k),real(l),real(lambda),chih,imat)
      call strfac(0.e0,0.e0,0.e0,real(lambda),chi0,imat)
c
      k0 = 1.d0/(lambda*1e-7)
c      
      theta = gtheta(h,k,l,lambda,imat)*180./pi
c      
      
      asy =  asy*pi/180.e0
c
c Laue 
      if(laue) then
         gamma0 = cos(theta - asy)
         gammah = cos(theta + asy)
      endif
c Bragg
      if(bragg) then
         gamma0 =  sin(theta + asy)
         gammah = -sin(theta - asy)
      endif
c
      b = gamma0/gammah
c     
      if(par) then
        pol = cos(2.e0*theta) !paralell Pol
      endif
c     
      if(sigma) then
         pol = 1.e0     
      endif
c      
      dp = (theta - dt_geom)*pi/180.
c         
         alpha = 2.*dp*sin(2.*theta)
c         
         z = chi0*(1. - b)/2. + b/2.*alpha
c
         a(1) = chi0**2 - chi0 * alpha - (pol*chih)**2
c         
         a(2) = cmplx(2.*alpha,0.) - 2.*chi0/b - 2.*chi0
c         
         a(3) = cmplx(4./b,0.)
c
         call zroots(a,2,roots,.true.)
c         
         d01 = roots(1)
         d02 = roots(2)
c         
         x1 = (2.*d01-chi0)/(pol*chih)
         x2 = (2.*d02-chi0)/(pol*chih)
c         
         phi1 = 2.0*pi*t1*k0*d01/gamma0
         phi2 = 2.0*pi*t1*k0*d02/gamma0
c         
         call me_cexp(c1,(0.e0,-1.e0)*phi1)
         call me_cexp(c2,(0.e0,-1.e0)*phi2)
c
         if(laue) then
            call laueit(x1,x2,c1,c2,d0,dh)
         endif
c        
         if(bragg) then
             call braggit(x1,x2,c1,c2,d0,dh)
         endif
c         
         IBH = (CABS(DH)**2)/abs(b)
         IB0 = (CABS(D0)**2)/abs(b)
         reflex = IBH
c        
         return
         end
c
        subroutine me_cexp(result,arg)
c
c--- Zerlegung der complexen Exponentialfunktion
c    Auswertung der trignometischen Funktionen ueber 
c    Hauptwerte  
c
        complex result,arg
        real rarg,iarg,pi
        real resultrarg,resultiarg
        real emuet
c
        pi = 4.e0*atan(1.e0)
c
        result = (0.e0,0.e0)
c        
        rarg = real(arg)
        iarg = aimag(arg)
c
        iarg = mod(iarg,2.e0*pi)
c        
        if(rarg.ge.0.e0) then
           if(rarg.ge.20.0) then 
             emuet = exp(20.0)
           else
             emuet=exp(rarg)
           endif
        endif
c        
        if(rarg.lt.0.e0) then
           if(rarg.le.-20.0) then
              emuet = exp(-20.0)
           else 
              emuet=exp(rarg)
           endif
        endif
c        
        resultrarg=cos(iarg)
        resultiarg=sin(iarg)
c
        result = emuet*cmplx(resultrarg,resultiarg)
        return
c        
        end
 
