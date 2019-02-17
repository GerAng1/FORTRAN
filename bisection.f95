program bisection_2
  implicit none
  real :: a,b,c,ans,fa,fb,fc,tolerance
  integer :: max_iter,counter=-1
  integer :: max_reach

  write(*,*) 'What is your tolerance ?'
  read (*,*) tolerance
  write(*,*) 'How many iterations do you wish to do?'
  read (*,*) max_iter

  write(*,*) 'Input your upper limit, a'
  read (*,*) a
  fa=(9000/a)*(1-(1/(1+a)**24))-179000

   If (fa<0) then
     write(*,*) 'f(a) is not in the upper interval, pick another "a".'
     stop 0

   else if (abs(fa)<tolerance) then
     ans=a
     write(*,*) 'The root is: ',ans
     stop 0
   end if

  write(*,*) 'Input your lower limit, b'
  read (*,*) b
  fb=(9000/b)*(1-(1/(1+b)**24))-179000

   if (fb>0) then
     write(*,*) 'f(b) is not in the lower interval, pick another "b".'
     stop 0

   else if (abs(fb)<tolerance) then
     ans=b
     write(*,*) 'The root is: ',ans
     stop 0
   end if

  max_reach = 0


  Do while (max_reach.eq.0)
    counter = counter + 1
    c=(a+b)/2
    fc=(9000/c)*(1-(1/(1+c)**24))-179000
    write(*,*) 'Iteration: ',counter
    write(*,*) 'f(c) is: ',fc

    if (counter>=max_iter) then
      write(*,*) 'Mas iteration reached. Terminating'
      ans = c
      max_reach=1

    else
      if (0>fc.and.abs(fc)>tolerance) then
        write(*,*) 'Changing "b".'
        b=c
        max_reach=0

      else if (fc>0.and.abs(fc)>tolerance) then
        write(*,*) 'Changing "a".'
        a=c
        max_reach=0

      else
        write(*,*) 'Tolerance reached!'
        ans=c
        max_reach=1
      end if
    end if
  end do
  write(*,*) 'The root is: ',ans
  write(*,*) 'Terminating'

end program bisection_2
