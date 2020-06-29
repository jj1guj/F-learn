program main
    use activation
    implicit none
    real::A(2,1),A2(3,1)
    real,allocatable::B(:,:)
    A(1,1)=2
    A(2,1)=-1
    A2=reshape((/1010,1000,990/),(/3,1/))
    call step_function(A,B)
    print*,B
    deallocate(B)

    call sigmoid(A,B)
    print*,B
    deallocate(B)

    call relu(A,B)
    print*,B
    deallocate(B)

    call softmax(A2,B)
    print*,B
end program main