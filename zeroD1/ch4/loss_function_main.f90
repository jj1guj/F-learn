program main
    use loss_function
    implicit none
    real::Y(10,1)
    integer::T(10,1)
    T=reshape((/0,0,1,0,0,0,0,0,0,0/),(/10,1/))
    Y=reshape((/0.1,0.05,0.6,0.0,0.05,0.1,0.0,0.1,0.0,0.0/),(/10,1/))

    print*,meam_squared_error(Y,T)

    print*,cross_entropy_error(Y,T)
end program main