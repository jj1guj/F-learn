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
            return
        end if
    end subroutine dot
end module matrix_operation