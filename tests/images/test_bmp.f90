! test_bmp.f90 --
!     Test the module to read BMP-files
!
program test_bmp
    use bmpfile

    implicit none

    integer, parameter                      :: byte = one_byte

    integer(4), allocatable, dimension(:,:) :: image
    integer                                 :: error
    integer                                 :: i, j
    integer                                 :: red, green, blue
    integer(kind=byte)                      :: scale
    integer(kind=byte)                      :: max_colour
    integer(kind=four_bytes)                :: full_colour1, full_colour2

    character(len=256)                      :: message

    ! Test the colour functions
    !
    full_colour1 = RGB( 210,      220,      230      )
    full_colour2 = RGB( -46_byte, -36_byte, -26_byte )

    if ( full_colour1 /= full_colour2 ) then
        write(*,*) 'Problem constructing colours:'
        write(*,*) '    4-byte and 1-byte integers give different results'
    endif

    error = openbitmap( image, "clouds.bmp" )
    write(*,*) 'Error:  ', error
    write(*,*) 'Size:   ', shape(image)
    if ( error /= 0 ) then
        call GetErrorMessage(message)
        write(*,*) 'Message:', trim(message)
        stop
    endif

    !
    ! Simple test regarding the manipulation of colours:
    ! Turn the image into black-white and save it
    !
    do j = 1,size(image,2)
        do i = 1,size(image,1)
            scale = (GetColorValue(image(i,j), 'r') + &
                     GetColorValue(image(i,j), 'g') + &
                     GetColorValue(image(i,j), 'b')) / 3
            image(i,j) = RGB(scale, scale, scale)
        enddo
    enddo

    write(*,*) 'Save new bitmap ...'

    error = savebitmap( image, "clouds_bw.bmp" )

    deallocate( image )

    !
    ! Construct an image with red, blue and green patches and overlaps thereof
    ! Note: simply adding is only possible with separated colours
    !
    write(*,*) 'Constructing new image ...'
    allocate( image(300,300) )
    image = 0

    red   = RGB( -1_byte,  0_byte,  0_byte )  ! -1 is (unsigned) 255, but this is not accepted
    green = RGB(  0_byte, -1_byte,  0_byte )
    blue  = RGB(  0_byte,  0_byte, -1_byte )

    image(  1:300,  1:100) = image(  1:300,  1:100) + red
    image(  1:100,  1:300) = image(  1:100,  1:300) + green
    image(201:300,  1:300) = image(201:300,  1:300) + blue
    image(  1:200,201:300) = image(  1:200,201:300) + blue
    image(201:300,201:300) = image(201:300,201:300) + red    + green

    error = savebitmap( image, "colours.bmp" )

    write(*,*) 'Done'
end program test_bmp
