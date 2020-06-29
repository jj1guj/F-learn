program main
    use loss_function
    implicit none
    real::Y(10,1),ans
    integer::T(10,1)
    T=reshape((/0,0,1,0,0,0,0,0,0,0/),(/10,1/))
    Y=reshape((/0.1,0.05,0.6,0.0,0.05,0.1,0.0,0.1,0.0,0.0/),(/10,1/))

    call meam_squared_error(Y,T,ans)
    print*,ans

    call cross_entropy_error(Y,T,ans)
    print*,ans
end program main