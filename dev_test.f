 	program dev_test
c
c --- 	testen der Verteilungsfunktionen
c
        
        real strahl_p0(3),strahl_a(3)

        open(18,'/home/thomas_old/fortran/raytrace/hit.dat')
        open(19,'/home/thomas_old/fortran/raytrace/hit1.dat')
        call histinit(1,-.01,.01,50)
        call histinit(2,-.001,.001,50)
                
	do i = 1, 25000
	  call strahl_generieren(strahl_p0,strahl_a,lambda_x)
 	  write(18,*) (strahl_p0(l),l=1,3)  
          write(19,*) (strahl_a(l),l=1,3)
          call histogram(1,strahl_a(2))
          call histogram(2,strahl_a(3))
        write(6,*) strahl_a(2),strahl_a(3)
        end do 
        close(18)
        close(19)
        call histowrite(1,'h_ver.dat')
        call histowrite(2,'v_ver.dat')
c             
        end
	