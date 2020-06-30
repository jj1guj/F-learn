program main
    use matrix_operation
    implicit none
    integer::ar,ac,br,bc
    real,allocatable::A(:,:),B(:,:)
    read*,ar,ac,br,bc
    allocate(A(ac,ar))
    allocate(B(bc,br))
    read*,A,B
    print*,dot(A,B)
end program main