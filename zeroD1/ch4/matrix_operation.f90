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
end module matrix_operation