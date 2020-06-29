program main
    use matrix_operation
    implicit none
    integer::ar,ac,br,bc
    real,allocatable::A(:,:),B(:,:),C(:,:)
    read*,ar,ac,br,bc
    allocate(A(ac,ar))
    allocate(B(bc,br))
    read*,A,B
    call dot(A,B,C)
    print*,C
end program main