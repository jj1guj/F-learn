module loss_function
    implicit none
    
contains
    subroutine meam_squared_error(Y,T,error)
        implicit none
        real,intent(in) :: Y(:,:)
        integer,intent(in)::T(:,:)
        real,intent(out) :: error
        real::error_calc
        integer::i,j,N
        integer,allocatable::Yshape(:)
        Yshape=shape(Y)
        N=Yshape(2)
        error=0

        do i=1,N
            error_calc=0
            do j=1,Yshape(1)
                error_calc=error_calc+(Y(j,i)-real(T(j,i)))**2
            end do
            error_calc=error_calc*0.5
            error=error+error_calc
        end do
        error=error/real(N)
    end subroutine meam_squared_error

    subroutine cross_entropy_error(Y,T,error)
        implicit none
        real,intent(in) :: Y(:,:)
        integer,intent(in)::T(:,:)
        real,intent(out) :: error
        real::error_calc,delta
        integer::i,j,N
        integer,allocatable::Yshape(:)
        Yshape=shape(Y)
        N=Yshape(2)
        error=0
        delta=1e-7

        do i=1,N
            error_calc=0
            do j=1,Yshape(1)
                error_calc=error_calc-real(T(j,i))*log(Y(j,i)+delta)
            end do
            error=error+error_calc
        end do
        error=error/real(N)
    end subroutine cross_entropy_error
end module loss_function