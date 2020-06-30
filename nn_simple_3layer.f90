program main
    use matrix_operation
    use activation
    implicit none
    real::W1(3,2),X(2,1),B1(3,1),W2(2,3),B2(2,1),W3(2,2),B3(2,1)
    real,allocatable::A1(:,:),Z1(:,:),A2(:,:),Z2(:,:),A3(:,:),Y(:,:)
    !入力ベクトル
    X=reshape((/1.0,0.5/),(/2,1/))

    !1層目
    W1=reshape((/0.1,0.3,0.5,0.2,0.4,0.6/),(/3,2/))
    B1=reshape((/0.1,0.2,0.3/),(/3,1/))

    !2層目
    W2=reshape((/0.1,0.4,0.2,0.5,0.3,0.6/),(/2,3/))
    B2=reshape((/0.1,0.2/),(/2,1/))

    !3層目
    W3=reshape((/0.1,0.3,0.2,0.4/),(/2,2/))
    B3=reshape((/0.1,0.2/),(/2,1/))

    !1層目
    A1=dot(X,W1)
    A1(:,:)=A1(:,:)+B1(:,:)
    Z1=sigmoid(A1)
    
    !2層目
    A2=dot(Z1,W2)
    A2(:,:)=A2(:,:)+B2(:,:)
    Z2=sigmoid(A2)
    deallocate(Z1)
    deallocate(A2)

    !3層目
    A3=dot(Z2,W3)
    A3(:,:)=A3(:,:)+B3(:,:)
    Y=softmax(A3)
    deallocate(Z2)
    deallocate(A3)

    print*,Y
end program main