
!$macro(T)
    subroutine whatever(a,x,b,o)
       type(T), intent(in)  :: a, x(:), b
       type(T), intent(out) :: o(:)
       o(:) = a*x(:) + b
    end subroutine
!$end macro

interface whatever
!$macro(T)
    subroutine whatever
!$end macro
end interface
