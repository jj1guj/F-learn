program main
    use activation
    implicit none
    real::A(2,1)
    real,allocatable::B(:,:)
    A(1,1)=2
    A(2,1)=-1
    call step_function(A,B)
    print*,B

    call sigmoid(A,B)
    print*,B

    call relu(A,B)
    print*,B
end program main