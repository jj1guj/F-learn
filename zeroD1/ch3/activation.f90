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

    subroutine softmax(x,y)
        implicit none
        real,intent(in) :: x(:,:)
        real,allocatable,intent(out) ::  y(:,:)
        real::exp_sum
        real,allocatable::c(:,:),x2(:,:)
        integer::i,j
        integer,allocatable::Xshape(:)
        Xshape=shape(x)
        allocate(y(Xshape(1),Xshape(2)))
        allocate(c(Xshape(1),Xshape(2)))
        allocate(x2(Xshape(1),Xshape(2)))
        
        !オーバーフロー対策の下処理
        c=maxval(x)
        x2(:,:)=x(:,:)-c

        exp_sum=sum(exp(x2))

        !$omp parallel private(i)
        !$omp do
        do j=1,Xshape(1)
            !$omp parallel
            !$omp do
            do i=1,Xshape(2)
                y(j,i)=exp(x2(j,i))/exp_sum
            end do
            !$omp end do
            !$omp end parallel
        end do
        !$omp end do
        !$omp end parallel
    end subroutine softmax
end module activation