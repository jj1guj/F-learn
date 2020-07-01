program main
    use matrix_operation
    use activation
    use optimize
    use loss_function
    implicit none
    integer::t(3,1)
    real::W(3,2),x(2,1)
    real,allocatable::dW(:,:)
    W=reshape((/0.47355232,0.9977393,0.84668094,0.85557411,0.03563661,0.69422093/),(/3,2/))
    x=reshape((/0.6,0.9/),(/2,1/))
    t=reshape((/0,0,1/),(/3,1/))

    !勾配を求める
    dW=numerical_gradient(loss,W,x,t)
    print*,dW
    W=gradient_decent(loss,W,x,t,0.01,10000)
    print*,W
contains
    function loss(W,x,t)
        real,allocatable::z(:,:),y(:,:)
        integer::t(:,:)
        real::loss,W(:,:),x(:,:)
        !predict
        z=dot(x,W)
        y=softmax(z)
        
        loss=cross_entropy_error(y,t)
    end function
end program main