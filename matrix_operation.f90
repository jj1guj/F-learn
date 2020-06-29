module matrix_operation
    implicit none
    
contains
    !行列積を計算する
    subroutine dot(A,B,C)
        implicit none
        real,intent(in)::A(:,:),B(:,:)
        real,allocatable,intent(out)::C(:,:)
        integer::i,j,k
        integer::Ashape(2),Bshape(2)
        Ashape=shape(A)
        Bshape=shape(B)
        allocate(C(Bshape(1),Ashape(2)))

        if(Ashape(1)/=Bshape(2))then
            print'(A)',"CAN NOT CALC!"
        else
            !$omp parallel private(j,k)
            !$omp do
            do i=1,Ashape(2)
                !$omp parallel private(k)
                !$omp do
                do j=1,Bshape(1)
                    C(j,i)=0
                    do k=1,Ashape(1)
                        C(j,i)=C(j,i)+A(k,i)*B(j,k)
                    end do
                end do
                !$omp end do
                !$omp end parallel
            end do
            !$omp end do
            !$omp end parallel
            return
        end if
    end subroutine dot
end module matrix_operation