program problem001
   k=0

   do i = 1, 999
     if((mod(i,3).eq.0) .or. (mod(i,5).eq.0)) then
         k= k + i
      end if
   end do

    write(*,*) k

end program problem001
