module matrix_operation
    implicit none
    
contains
    !行列積を計算する
    subroutine dot(Arow,Aclm,Brow,Bclm,A,B,C)
        implicit none
        integer(8),intent(in)::Arow,Aclm,Brow,Bclm
        real,intent(in)::A(Aclm,Arow),B(Bclm,Brow)
        real,intent(out)::C(Bclm,Arow)
        integer(8)::i,j,k

        if(Aclm/=Brow)then
            print'(A)',"CAN NOT CALC!"
        else
            !$omp parallel private(j,k)
            !$omp do
            do i=1,Arow
                !$omp parallel private(k)
                !$omp do
                do j=1,Bclm
                    C(j,i)=0
                    do k=1,Aclm
                        C(j,i)=C(j,i)+A(k,i)*B(j,k)
                    end do
                end do
                !$omp end do
                !$omp end parallel
            end do
            !$omp end do
            !$omp end parallel
        end if

        return
    end subroutine dot
    
    subroutine plus(Arow,Aclm,A,b)
        implicit none
        integer(8),intent(in)::Arow,Aclm
        real,intent(in)::b(Aclm,Arow)
        real,intent(inout)::A(Aclm,Arow)
        integer(8)::i,j

        !$omp parallel private(j)
        !$omp do
        do i=1,Arow
            !$omp parallel
            !$omp do
            do j=1,Aclm
                A(j,i)=A(j,i)+b(j,i)
            end do
            !$omp end do
            !$omp end parallel
        end do
        !$omp end do
        !$omp end parallel

        return
    end subroutine plus
end module matrix_operation

program main
    use matrix_operation
    implicit none
    integer(8)::Arow,Aclm,Brow,Bclm
    real,allocatable::A(:,:),B(:,:),C(:,:)
    read*,Arow,Aclm,Brow,Bclm
    allocate(A(Aclm,Arow))
    allocate(B(Bclm,Brow))
    allocate(C(Bclm,Arow))
    read*,A,B
    call dot(Arow,Aclm,Brow,Bclm,A,B,C)
    print*,C
    deallocate(C)
end program main

