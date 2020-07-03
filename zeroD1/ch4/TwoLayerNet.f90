module TwoLayerNet
    use matrix_operation
    use activation
    use loss_function
    implicit none
contains
    subroutine init(input_size,hidden_size,output_size,W_list,W_shape)
        integer,intent(in)::input_size,hidden_size,output_size
        integer,allocatable,intent(out)::W_shape(:,:)
        real,allocatable,intent(out)::W_list(:,:,:)
        real,allocatable::W(:,:),b(:,:)

        allocate(W_list(4,max(input_size,hidden_size,output_size),max(input_size,hidden_size,output_size)))
        allocate(W_shape(2,4))

        !1層目
        W=random_init((/hidden_size,input_size/))
        allocate(b(hidden_size,1))
        b=0
        W_list(1,:,:)=W
        W_shape(:,1)=shape(W)
        W_list(2,:,:)=b
        W_shape(:,2)=shape(b)
        deallocate(W)
        deallocate(b)

        !2層目
        W=random_init((/output_size,hidden_size/))
        allocate(b(output_size,1))
        b=0
        W_list(3,:,:)=W
        W_shape(:,3)=shape(W)
        W_list(4,:,:)=b
        W_shape(:,4)=shape(b)
        deallocate(W)
        deallocate(b)
    end subroutine init

    function predict(W_list,W_shape,x)
        integer::W_shape(:,:)
        real::x(:,:),W_list(:,:,:)
        real,allocatable::a(:,:),z(:,:),predict(:,:)
        
        a=dot(x,W_list(1,1:W_shape(1,1),1:W_shape(2,1)))
        a(:,:)=a(:,:)+W_list(2,1:W_shape(1,2),1:W_shape(2,2))
        z=sigmoid(a)
        deallocate(a)

        a=dot(z,W_list(3,1:W_shape(1,3),1:W_shape(2,3)))
        deallocate(z)
        a(:,:)=a(:,:)+W_list(4,1:W_shape(1,4),1:W_shape(2,4))
        predict=softmax(a)
        deallocate(a)
    end function

    function loss(W_list,W_shape,x,t)
        integer,allocatable::xshape(:)
        real,allocatable::z(:,:),y(:,:)
        integer::t(:,:),W_shape(:,:),N,i
        real::loss,W_list(:,:,:),x(:,:,:)
        
        xshape=shape(x)
        N=xshape(1)

        !predict
        allocate(y(W_shape(1,4),N))
        do i=1,N
            z=predict(W_list,W_shape,x(i,:,:))
            y(:,i)=z(:,1)
        end do
        
        loss=cross_entropy_error(y,t)
    end function

    function accuracy(W_list,W_shape,x,t)
        integer::t(:,:),i,N,t_count=0,W_shape(:,:)
        integer,allocatable::xshape(:)
        real::W_list(:,:,:),x(:,:,:),accuracy
        real,allocatable::y(:,:)

        xshape=shape(x)
        N=xshape(1)

        do i=1,N
            y=predict(W_list,W_shape,x(:,:,i))
            if(t(argmax(y(:,1)),i)==1)t_count=t_count+1
        end do

        accuracy=real(t_count)/real(N)
    end function

    function numerical_gradient(f,W_list,W_shape,x,t)
        interface
            real function f(W_list,W_shape,x,t)
                integer::W_shape(:,:),t(:,:)
                real::W_list(:,:,:),x(:,:,:)
            end function
        end interface
        integer::W_shape(:,:),t(:,:),N,i,j,k
        integer,allocatable::W_list_shape(:)
        real::W_list(:,:,:),x(:,:,:),h=1e-4,fxh1,fxh2
        real,allocatable::numerical_gradient(:,:,:),tmp_val(:,:,:)
        W_list_shape=shape(W_list)
        N=W_list_shape(1)
        allocate(numerical_gradient(W_list_shape(1),W_list_shape(2),W_list_shape(3)))
        tmp_val=W_list

        do k=1,N
            do i=1,W_shape(2,k)
                do j=1,W_shape(1,k)
                    !f(x+h)を計算
                    W_list(k,j,i)=tmp_val(k,j,i)+h
                    fxh1=f(W_list,W_shape,x,t)

                    !f(x-h)を計算
                    W_list(k,j,i)=tmp_val(k,j,i)-h
                    fxh2=f(W_list,W_shape,x,t)

                    !勾配を計算
                    numerical_gradient(k,j,i)=(fxh1-fxh2)/(2*h)

                    !値をもとに戻す
                    W_list(k,j,i)=tmp_val(k,j,i)
                end do
            end do
        end do
    end function
end module TwoLayerNet