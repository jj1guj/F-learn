program main
    use activation
    implicit none
    real::A(2,1)
    A(1,1)=2
    A(2,1)=-1
    print*,step_function(A)

    print*,sigmoid(A)

    print*,relu(A)

    print*,softmax(A)
end program main