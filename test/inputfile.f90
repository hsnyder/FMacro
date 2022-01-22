
!$template(T)
    subroutine whatever(a,x,b,o)
       type(T), intent(in)  :: a, x(:), b
       type(T), intent(out) :: o(:)
       o(:) = a*x(:) + b
    end subroutine
!$end template

interface whatever
!$template(T)
    subroutine whatever
!$end template
end interface
