module activation
    implicit none
    
contains
    function step_function(x)
        implicit none
        real::x(:,:)
        real,allocatable::step_function(:,:)
        integer::i,j
        integer,allocatable::Xshape(:)
        Xshape=shape(x)
        allocate(step_function(Xshape(1),Xshape(2)))

        !$omp parallel private(i)
        !$omp do
        do j=1,Xshape(1)
            !$omp parallel
            !$omp do
            do i=1,Xshape(2)
                if(x(j,i)>0)then
                    step_function(j,i)=1
                else
                    step_function(j,i)=0
                end if
            end do
            !$omp end do
            !$omp end parallel
        end do
        !$omp end do
        !$omp end parallel
    end function

    function sigmoid(x)
        implicit none
        real::x(:,:)
        real,allocatable::sigmoid(:,:)
        integer::i,j
        integer,allocatable::Xshape(:)
        Xshape=shape(x)
        allocate(sigmoid(Xshape(1),Xshape(2)))

        !$omp parallel private(i)
        !$omp do
        do j=1,Xshape(1)
            !$omp parallel
            !$omp do
            do i=1,Xshape(2)
                sigmoid(j,i)=1/(1+exp(-1*x(j,i)))
            end do
            !$omp end do
            !$omp end parallel
        end do
        !$omp end do
        !$omp end parallel
    end function

    function relu(x)
        implicit none
        real::x(:,:)
        real,allocatable::relu(:,:)
        integer::i,j
        integer,allocatable::Xshape(:)
        Xshape=shape(x)
        allocate(relu(Xshape(1),Xshape(2)))

        !$omp parallel private(i)
        !$omp do
        do j=1,Xshape(1)
            !$omp parallel
            !$omp do
            do i=1,Xshape(2)
                if(x(j,i)>0)then
                    relu(j,i)=x(j,i)
                else
                    relu(j,i)=0
                end if
            end do
            !$omp end do
            !$omp end parallel
        end do
        !$omp end do
        !$omp end parallel
    end function

    function softmax(x)
        implicit none
        real::x(:,:),exp_sum
        real,allocatable::softmax(:,:),c(:,:)
        integer::i,j
        integer,allocatable::Xshape(:)
        Xshape=shape(x)
        allocate(softmax(Xshape(1),Xshape(2)))
        allocate(c(Xshape(1),Xshape(2)))
        
        !オーバーフロー対策の下処理
        c=maxval(x)
        x(:,:)=x(:,:)-c(:,:)
        exp_sum=sum(exp(x))


        !$omp parallel private(i)
        !$omp do
        do j=1,Xshape(1)
            !$omp parallel
            !$omp do
            do i=1,Xshape(2)
                softmax(j,i)=exp(x(j,i))/exp_sum
            end do
            !$omp end do
            !$omp end parallel
        end do
        !$omp end do
        !$omp end parallel
    end function
end module activation