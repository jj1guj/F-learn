program TwoLayerNet_main
    use TwoLayerNet
    implicit none
    integer::iters_num,batch_size,i
    integer,allocatable::W_shape(:,:)
    real::learning_rate
    real,allocatable::W_list(:,:,:)

    !初期化
    call init(784,50,10,W_list,W_shape)
    
    !ハイパーパラメータ
    iters_num=10000
    batch_size=100
    learning_rate=0.1

    do i=1,iters_num
    end do
end program TwoLayerNet_main