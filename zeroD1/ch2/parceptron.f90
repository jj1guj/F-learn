module parceptron
    use matrix_operation
    implicit none
contains
    subroutine AND(x1,x2,ans)
        implicit none
        real,intent(in)::x1,x2
        real,intent(out)::ans
        real::tmp,x(2),w(2),b
        W=(/0.5,0.5/)
        x=(/x1,x2/)
        b=-0.7
        call hadamard(1,2,W,X)
        tmp=sum(W)+b
        if(tmp<=0)then
            ans=0
        else
            ans=1
        end if

        return
    end subroutine AND

    subroutine NAND(x1,x2,ans)
        implicit none
        real,intent(in)::x1,x2
        real,intent(out)::ans
        real::tmp,x(2),w(2),b
        W=(/-0.5,-0.5/)
        x=(/x1,x2/)
        b=0.7
        call hadamard(1,2,W,X)
        tmp=sum(W)+b
        if(tmp<=0)then
            ans=0
        else
            ans=1
        end if

        return
    end subroutine NAND

    subroutine OR(x1,x2,ans)
        implicit none
        real,intent(in)::x1,x2
        real,intent(out)::ans
        real::tmp,x(2),w(2),b
        W=(/0.5,0.5/)
        x=(/x1,x2/)
        b=-0.2
        call hadamard(1,2,W,X)
        tmp=sum(W)+b
        if(tmp<=0)then
            ans=0
        else
            ans=1
        end if

        return
    end subroutine OR

    subroutine XOR(x1,x2,ans)
        real,intent(in)::x1,x2
        real,intent(out)::ans
        real::s1,s2

        call NAND(x1,x2,s1)
        call OR(x1,x2,s2)
        call AND(s1,s2,ans)

        return
    end subroutine XOR
end module parceptron

program main
    use parceptron
    implicit none
    real::ans
    integer(8)::i,j
    do i=0,1
        do j=0,1
            call XOR(real(i),real(j),ans)
            print*,i,j,ans
        end do
    end do
end program main