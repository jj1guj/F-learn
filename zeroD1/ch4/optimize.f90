module optimize
    implicit none
contains
    function numerical_gradient(f,W,x,t)
        interface
            real function f(Win,xin,tin)
                integer::tin(:,:)
                real::xin(:,:),Win(:,:)
            end function
        end interface
        integer::t(:,:),i,j
        integer,allocatable::Wshape(:)
        real::x(:,:),W(:,:),tmp_val,h=1e-4,fxh1,fxh2
        real,allocatable::numerical_gradient(:,:)
        Wshape=shape(W)
        allocate(numerical_gradient(Wshape(1),Wshape(2)))
        
        do i=1,Wshape(2)
            do j=1,Wshape(1)
                tmp_val=W(j,i)
                !f(x+h)を計算
                W(j,i)=tmp_val+h
                fxh1=f(W,x,t)

                !f(x-h)を計算
                W(j,i)=tmp_val-h
                fxh2=f(W,x,t)

                !勾配の計算
                numerical_gradient(j,i)=(fxh1-fxh2)/(2*h)
                W(j,i)=tmp_val !値をもとに戻す
            end do
        end do
    end function
end module optimize