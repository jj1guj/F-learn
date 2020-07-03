module matrix_operation
    implicit none
    
contains
    !行列積を計算する
    function dot(A,B)
        real::A(:,:),B(:,:)
        real,allocatable::dot(:,:)
        integer::Ashape(2),Bshape(2)
        integer::i,j,k
        Ashape=shape(A)
        Bshape=shape(B)
        allocate(dot(Bshape(1),Ashape(2)))

        !$omp parallel private(j,k)
        !$omp do
        do i=1,Ashape(2)
            !$omp parallel private(k)
            !$omp do
            do j=1,Bshape(1)
                dot(j,i)=0
                do k=1,Ashape(1)
                    dot(j,i)=dot(j,i)+A(k,i)*B(j,k)
                end do
            end do
            !$omp end do
            !$omp end parallel
        end do
        !$omp end do
        !$omp end parallel
    end function

    !形がAshapeの行列を0~1の乱数で初期化する
    function random_init(Ashape)
        integer::Ashape(2),seedsize,i
        integer,allocatable::seed(:)
        real::random_init(Ashape(1),Ashape(2))

        call random_seed(size=seedsize)
        allocate(seed(seedsize))
        do i=1,seedsize
            call system_clock(count=seed(i))
        end do
        call random_number(random_init)
    end function

    !一次元配列の最大値のインデックスを返す
    function argmax(A)
        integer::argmax,i,Ashape(1)
        real::A(:),amax
        amax=maxval(A)
        Ashape=shape(A)
        do i=1,Ashape(1)
            if(A(i)==amax)then
                argmax=i
                exit
            end if
        end do
    end function
end module matrix_operation