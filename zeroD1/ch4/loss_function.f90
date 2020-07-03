module loss_function
    implicit none
    
contains
    !Y: 出力ラベル, T: 正解ラベル
    function meam_squared_error(Y,T)
        implicit none
        integer::T(:,:)
        real::Y(:,:)
        real::meam_squared_error,error_calc
        integer i,j,N
        integer,allocatable::Yshape(:)

        Yshape=shape(Y)
        N=Yshape(2)
        meam_squared_error=0

        do i=1,N
            error_calc=0
            do j=1,Yshape(1)
                error_calc=error_calc+(Y(j,i)-real(T(j,i)))**2
            end do
            error_calc=0.5*error_calc
            meam_squared_error=meam_squared_error+error_calc
        end do
        meam_squared_error=meam_squared_error/real(N)
    end function

    !Y: 出力ラベル, T: 正解ラベル
    function cross_entropy_error(Y,T)
        implicit none
        integer::T(:,:)
        real::Y(:,:)
        real::cross_entropy_error,error_calc,delta
        integer i,j,N
        integer,allocatable::Yshape(:)

        Yshape=shape(Y)
        N=Yshape(2)
        cross_entropy_error=0
        delta=1e-7

        do i=1,N
            error_calc=0
            do j=1,Yshape(1)
                error_calc=error_calc-real(T(j,i))*log(Y(j,i)+delta)
            end do
            cross_entropy_error=cross_entropy_error+error_calc
        end do
        cross_entropy_error=cross_entropy_error/real(N)
    end function
end module loss_function