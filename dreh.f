        subroutine dreh(m,x_input,x_out)
c
c --- Koordinatendrehung
c
        implicit none
        integer i
        real m(3,3),x_in(3),x_out(3),x_input(3)
        do i = 1,3
                x_in(i) = x_input(i)
        end do

        x_out(1) = m(1,1)*x_in(1)+m(1,2)*x_in(2)+m(1,3)*x_in(3)
        x_out(2) = m(2,1)*x_in(1)+m(2,2)*x_in(2)+m(2,3)*x_in(3)
        x_out(3) = m(3,1)*x_in(1)+m(3,2)*x_in(2)+m(3,3)*x_in(3)

        return
        end
c
        subroutine bdreh(m,xd,x_in,x_out)
c
c --- drehung um einen beliebigen Punkt
c       
        implicit none
        integer iloop
        real m(3,3),xd(3),x_in(3),x_out(3),gx(3)
        
        do iloop = 1,3
          gx(iloop) = x_in(iloop) -xd(iloop)
        end do
c
c --- Drehen des Richtungsvektors
c
        call dreh(m,gx,x_out)
c
c --- Ruecktranslation
c
        do iloop = 1,3
          x_out(iloop) = x_out(iloop) + xd(iloop)
        end do
c
c --- das war's
c
        return
        end
c
        subroutine trans(xd,x,x1)
c
c ---  translation eines punktes
c       
        implicit none
        integer iloop
        real xd(3),x(3),x1(3)
c
        do iloop=1,3
           x1(iloop) = x(iloop) + xd(iloop)
        end do

c
c --- das war's
c
        return
        end
c
        subroutine matrix(alpha,beta,gamma,theta,m)
c
c --- erzeuge eine drehmatrix
c
        implicit none

        real alpha,beta,gamma,theta,m(3,3)
c
c --- drehmatrix
c
        m(1,1) = cos(theta)+alpha*alpha*(1.d0-cos(theta))
        m(1,2) = gamma*sin(theta)+alpha*beta*(1.d0-cos(theta))
        m(1,3) = -beta*sin(theta)+alpha*gamma*(1.d0-cos(theta))
c
        m(2,1) = -gamma*sin(theta)+alpha*beta*(1.d0-cos(theta))
        m(2,2) = cos(theta)+beta*beta*(1.d0-cos(theta))
        m(2,3) = alpha*sin(theta)+beta*gamma*(1.d0-cos(theta))
c
        m(3,1) = beta*sin(theta)+gamma*alpha*(1.d0-cos(theta))
        m(3,2) = -alpha*sin(theta)+gamma*beta*(1.d0-cos(theta))
        m(3,3) = cos(theta)+gamma*gamma*(1.d0-cos(theta))
c
c --- und Tschuess
c
        return
        end


