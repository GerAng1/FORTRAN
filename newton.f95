program newton
    implicit none

    integer :: current_iter=-1, max_it
    real :: x_i=0., x_nexti=0., ans=0., tolerance
    real :: f_i, d_i

    print*, "Newton-Raphson Method"
    print*, ""

    print*, "What is your output tolerance?: "
    read*, tolerance
    print*, "What is your max number of iterations?: "
    read*, max_it

    print*, "Your function:"
    print*, "f(x) = 10 + (2 * x) - 3 * (x**2) + 5 * (x**3) - 4 * (x**4) + 7 * (x**5)"
    print*, ""
    print*, "Your function differentiated:"
    print*, "f(x) = 2 - (6 * x) + 15 * (x**2) - 16 * (x**3) + 35 * (x**4)"

    print*, ""
    print*, "What will be your initial value?: "
    read *, x_i

    do
        f_i = 10 + (2 * x_i) - 3 * (x_i**2) + 5 * (x_i**3) - 4 * (x_i**4) + 7 * (x_i**5)
        d_i = 2 - (6 * x_i) + 15 * (x_i**2) - 16 * (x_i**3) + 35 * (x_i**4)

        print*, "f(xi) is now: ", f_i
        print*, "f'(xi) is: ", d_i

        current_iter = current_iter + 1
        print*, ""
        print*, "Current Iteration: ", current_iter

        x_nexti = (x_i - (f_i / d_i))
        print*, "'xi+1' is now: ", x_nexti

        if (current_iter.GT.max_it) then
            print*, "Max iteration reached. Terminating."
            ans = x_nexti
            exit
        else if (abs(f_i).LE.tolerance) then
            print*, "Tolerance reached."
            ans = x_nexti
            exit
        else
            x_i = x_nexti
        end if
    end do
    print*, ""
    print*, "The root is close to: ", ans
end program newton
