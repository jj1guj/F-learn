program main
    use matrix_operation
    implicit none
    integer::ar,ac,br,bc
    integer::m_shape(2)=(/3,3/)
    real,allocatable::A(:,:),B(:,:)
    !read*,ar,ac,br,bc
    ar=2
    ac=2
    br=2
    bc=2
    allocate(A(ac,ar))
    allocate(B(bc,br))
    !read*,A,B
    A=reshape((/1,2,3,4/),(/ac,ar/))
    B=reshape((/5,6,7,8/),(/bc,br/))

    print*,dot(A,B)

    print*,random_init(m_shape)
end program main