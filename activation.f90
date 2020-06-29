module activation
    implicit none
    
contains
    subroutine step_function(x,y)
        implicit none
        real,intent(in) :: x(:,:)
        real,allocatable,intent(out) ::  y(:,:)
        integer::i,j
        integer,allocatable::Xshape(:)
        Xshape=shape(x)
        allocate(y(Xshape(1),Xshape(2)))

        !$omp parallel private(i)
        !$omp do
        do j=1,Xshape(1)
            !$omp parallel
            !$omp do
            do i=1,Xshape(2)
                if(x(j,i)>0)then
                    y(j,i)=1
                else
                    y(j,i)=0
                end if
            end do
            !$omp end do
            !$omp end parallel
        end do
        !$omp end do
        !$omp end parallel
    end subroutine step_function

    subroutine sigmoid(x,y)
        implicit none
        real,intent(in) :: x(:,:)
        real,allocatable,intent(out) ::  y(:,:)
        integer::i,j
        integer,allocatable::Xshape(:)
        Xshape=shape(x)
        allocate(y(Xshape(1),Xshape(2)))

        !$omp parallel private(i)
        !$omp do
        do j=1,Xshape(1)
            !$omp parallel
            !$omp do
            do i=1,Xshape(2)
                y(j,i)=1/(1+exp(-1*x(j,i)))
            end do
            !$omp end do
            !$omp end parallel
        end do
        !$omp end do
        !$omp end parallel
    end subroutine sigmoid

    subroutine relu(x,y)
        implicit none
        real,intent(in) :: x(:,:)
        real,allocatable,intent(out) ::  y(:,:)
        integer::i,j
        integer,allocatable::Xshape(:)
        Xshape=shape(x)
        allocate(y(Xshape(1),Xshape(2)))

        !$omp parallel private(i)
        !$omp do
        do j=1,Xshape(1)
            !$omp parallel
            !$omp do
            do i=1,Xshape(2)
                if(x(j,i)>0)then
                    y(j,i)=x(j,i)
                else
                    y(j,i)=0
                end if
            end do
            !$omp end do
            !$omp end parallel
        end do
        !$omp end do
        !$omp end parallel
    end subroutine relu
end module activation