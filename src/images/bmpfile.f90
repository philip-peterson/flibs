! bmpfile.f90 --
!     Routines to read and save BMP files and to a certain extent
!     manipulate the images
!
!     Provided by Denis Yavna, slightly adjusted by Arjen Markus to
!     make more suitable for use as a general library.
!
!     Note:
!     The present version uses several F2003 features (NEWUNIT and streams),
!     there is another version which uses F95 only and a single extension to
!     deal with binary files.
!

! bmp_conversions --
!     Module with auxiliary routines to get and set colour components
!
module bmp_conversions
    use integral_types
    implicit none


    private
    public :: RGB, GetColorValue, GetIntegerColor

    interface RGB
        module procedure RGB_bytes
        module procedure RGB_integers
    end interface

    contains

    ! RGB --
    !     Return an integer that has the three colour components encoded
    !
    ! Arguments:
    !     r        Red colour (0 to 255 for integers, -128 to 127 for bytes)
    !     g        Green colour (0 to 255 for integers, -128 to 127 for bytes)
    !     b        Blue colour (0 to 255 for integers, -128 to 127 for bytes)
    !
    pure integer(four_bytes) function RGB_bytes(r,g,b)
        integer(one_byte),intent(in):: r,g,b
        RGB_bytes=UnsignedChar2Int4(r)+UnsignedChar2Int4(g)*256+UnsignedChar2Int4(b)*256*256
    end function RGB_bytes

    pure integer(four_bytes) function RGB_integers(r,g,b)
        integer(four_bytes),intent(in):: r,g,b
        RGB_integers=r+g*256+b*256*256
    end function RGB_integers

    ! UnsignedChar2Int4 --
    !     Helper function for byte values
    !
    ! Arguments:
    !     arg      One-byte integer to be interpreted as unsigned
    !
    ! Result:
    !     Ordinary four bits integer
    !
    pure integer(four_bytes) function UnsignedChar2Int4(arg)
        integer(one_byte),intent(in):: arg
        if (arg .ge. 0) then
            UnsignedChar2Int4=arg
            return
        endif
        UnsignedChar2Int4=256+arg
    end function UnsignedChar2Int4

    ! GetColorValue --
    !     Get the individual colour components as bytes
    !
    ! Arguments:
    !     arg      Composite colour value
    !     clr      One of 'r', 'g' or 'b' for respective components
    !
    ! Result:
    !     Byte value of the colour component
    !
    ! Note:
    !     The byte value is a _signed_ value encoding the range 0 to 255.
    !     The value -1 corresponds to the _maximum_ value 255. This is
    !     the reason for providing an additional routine, GetIntegerColor
    !     that returns integer values between 0 and 255 instead.
    !
    pure integer(one_byte) function GetColorValue(arg,clr)
        integer(four_bytes),intent(in):: arg
        character*1,intent(in):: clr
        select case (clr)
        case('r')
            GetColorValue=iand(int(255,four_bytes),arg)
        case('g')
            GetColorValue=ISHFTC(iand(int(65280,four_bytes),arg),-8)
        case('b')
            GetColorValue=ISHFTC(iand(int(16711680,four_bytes),arg),-16)
        case default
            GetColorValue=0
!           write (*,*) "Ку"
        endselect
    end function GetColorValue

    pure integer function GetIntegerColor(arg,clr)
        integer(four_bytes),intent(in) :: arg
        character*1,intent(in)         :: clr

        integer(one_byte)              :: color

        color = GetColorValue(arg,clr)
        if ( color < 0 ) then
            GetIntegerColor = 255 + color
        else
            GetIntegerColor = color
        endif
    end function GetIntegerColor
end module bmp_conversions

! bmpfile --
!     Routines to read and save Windows BMP files
!
!     Note:
!     Not all types of BMP files are supported
!
module bmpfile
    use bmp_conversions
    use integral_types
    implicit none

    private
    public :: OpenBitmap, SaveBitmap, GetErrorMessage
    public :: BadFile, IOError, BadFrmt

    public :: RGB, GetColorValue, GetIntegerColor  ! From bmp_conversions
    public :: one_byte, two_bytes, four_bytes      ! From integral_types

    character(len=80) :: message = ' '

    !Заголовок битмапа
    !Bitmap header
    type t_bmp
        character(2) bfType
        integer(four_bytes) bfSize
        integer(four_bytes) bfReserved
        integer(four_bytes) bfOffs
        integer(four_bytes) biSize
        integer(four_bytes) biWidth
        integer(four_bytes) biHeight
        integer(two_bytes)  biPlanes
        integer(two_bytes)  biBitCnt
        integer(four_bytes) biCompr
        integer(four_bytes) biSizeIm
        integer(four_bytes) biXPels
        integer(four_bytes) biYPels
        integer(four_bytes) biClrUsed
        integer(four_bytes) biClrImp
    end type t_bmp

    integer(four_bytes),parameter :: BadFile = 1
    integer(four_bytes),parameter :: IOError = 2
    integer(four_bytes),parameter :: BadFrmt = 3
!   integer(four_bytes), allocatable :: ImageForOutput(:,:)

    contains
        ! GetErrorMessage --
        !     Get the error message
        !
        ! Arguments:
        !     bitmap           Bitmap structure
        !     unitnum          Unit-number for the BMP file
        !
        ! Result:
        !     Zero or an error code
        !
        subroutine GetErrorMessage( msg )
            character(len=*) :: msg

            msg = message
        end subroutine GetErrorMessage

        ! GetBitmap --
        !
        !Читает заголовок битмапа. Если (вроде) нет ошибок, возвращает 0.
        !Reads a bitmap header. If no errors returns 0.
        !
        ! Arguments:
        !     bitmap           Bitmap structure
        !     unitnum          Unit-number for the BMP file
        !
        ! Result:
        !     Zero or an error code
        !
        integer(four_bytes) function GetBitmap(bitmap, unitnum)
            type (t_bmp) bitmap
            integer(four_bytes) unitnum
            integer(four_bytes) status
            character(20) msg

            read(unitnum,iostat=status) bitmap%bfType
            if (status.ne.0) then
                msg = ''; write(msg, *) status
                !msg = 'Ошибка чтения из файла # ' // trim(adjustl(msg))
                message = 'Error reading file - iostat = ' // trim(adjustl(msg))
                GetBitmap = IOError
                return
            endif
            !Неправильный файл подсунули
            !Incorrect file type
            if (bitmap%bfType.ne.'BM') then
                message = 'Error reading file - not a proper BMP file'
                GetBitmap = BadFile
                return
            endif

            read(unitnum,iostat=status) bitmap%bfSize, &
                &       bitmap%bfReserved,      &
                &       bitmap%bfOffs,          &
                &       bitmap%biSize,          &
                &       bitmap%biWidth,         &
                &       bitmap%biHeight,        &
                &       bitmap%biPlanes,        &
                &       bitmap%biBitCnt,        &
                &       bitmap%biCompr,         &
                &       bitmap%biSizeIm,        &
                &       bitmap%biXPels,         &
                &       bitmap%biYPels,         &
                &       bitmap%biClrUsed,       &
                &       bitmap%biClrImp
            if (status.ne.0) then
                msg = ''; write(msg, *) status
                !msg = 'Ошибка чтения из файла # ' // trim(adjustl(msg))
                message   = 'Error reading file - iostat = ' // trim(adjustl(msg))
                GetBitmap = IOError
                return
            endif
            !Не умеем читать жатый битмап
            !Unable to read rle-compressed bitmap
            if (bitmap%biCompr.ne.0) then
                message = 'Error reading file - bad or unsupported (compressed) format'
                GetBitmap = BadFrmt
                return
            endif
            !Вроде бы и RGB, а вроде бы и с палитрой -- фигня
            !Unable to read rgb bitmap with palette
            if (bitmap%biBitCnt.eq.24.and.bitmap%biClrUsed.ne.0) then
                message = 'Error reading file - bad or unsupported (compressed) format'
                GetBitmap = BadFrmt
                return
            endif
            GetBitmap = 0
        end function GetBitmap

        ! ReadPalette --
        !Читает палитру, если получается - возвращает 0.
        !Reads palette, returns 0 if success
        !
        ! Arguments:
        !     array     Palette array to be filled
        !     unitnum   Unit number for the BMP file
        !
        ! Result:
        !     Zero or an error code
        !
        integer(four_bytes) function ReadPalette(array, unitnum)
            integer(four_bytes) array(0:), status
            integer unitnum
            integer(two_bytes) i
            integer(one_byte) b,g,r,rgbres
            character(20) msg
            do i = 0, size(array)-1
                read(unitnum, iostat = status) b,g,r,rgbres
                if (status.ne.0) then
                    msg = ''; write(msg, *) status
                    !msg = 'Ошибка чтения из файла # ' // trim(adjustl(msg))
                    message   = 'Error reading file - iostat = ' // trim(adjustl(msg))
                    ReadPalette = IOError
                    return
                endif
                array(i) = RGB(r,g,b)
            enddo
            ReadPalette = 0
        end function ReadPalette

        ! TransposeBits --
        !В 1 и 4-хбитных файлах порядок следования точек обратный (от старшего к младшему)
        !Эта функция его "восстанавливает": в случае 1-битного изображения меняет порядок
        !на обратный(?), в случае 4-хбитного - меняет местами четвёрки бит
        !1- and 4-bit bitmaps use a reverse order of bits in data block
        !This function changes (makes direct) the bit sequence
        !
        ! Arguments:
        !     var             Variable to be transposed
        !     bitcount        Number of bits
        !
        ! Result:
        !     Transposed bits
        !
        integer(four_bytes) function TransposeBits(var,bitcount)
            integer(four_bytes) var,swp
            integer(two_bytes) bitcount
            integer(one_byte) i,j
            if (bitcount.eq.1) then
                do i=0,3
                    do j=0,7
                        call mvbits(var,i*8+j,1,swp,i*8+7-j)
                    enddo
                enddo
            elseif (bitcount.eq.4) then
                do i=0,3
                    call mvbits(var,i*8,4,swp,i*8+4)
                    call mvbits(var,i*8+4,4,swp,i*8)
                enddo
            endif
            TransposeBits=swp
        end function TransposeBits

    !Эта функция создаёт структуру "палитра виндовс" и возвращает на неё ссылку.
    !Не используем пока.
    !This function creates "windows palette" structure and returns corresponding pointer.
    !Not used at this moment.

    !       integer(four_bytes) function GetWinPalette(array)
    !           integer(four_bytes) array(0:)
    !           integer(four_bytes) nColors, i, status
    !           type(T_PALETTEENTRY) PalEntry(0:256)
    !           type(T_LOGPALETTE) LogPal
    !           equivalence(LogPal,PalEntry)
    !
    !           nColors = size(array)
    !           LogPal%palVersion = #300
    !           LogPal%palNumEntries = nColors
    !
    !           do i = 0, nColors-1
    !               PalEntry(i+1)%peRed = GetRedValue(array(i))
    !               PalEntry(i+1)%peGreen = GetGreenValue(array(i))
    !               PalEntry(i+1)%peBlue = GetBlueValue(array(i))
    !               PalEntry(i+1)%peFlags = 0
    !           end do
    !
    !           GetWinPalette = CreatePalette(LogPal)
    !       end function GetWinPalette

    ! SaveBitmap --
    !Сохраненяет битмапа (только rgb-типа)
    !Saves the bitmap (rgb-type only)
    !
    ! Arguments:
    !     arg             Integer allocatable array holding the image
    !     fname           Name of the BMP file
    !
    ! Result:
    !     Zero or an error code
    !
    integer(four_bytes) function SaveBitmap(arg,fname)
        integer(four_bytes) arg(1:,1:),unitnum,i,status,j,strlen
    !   integer(one_byte),parameter :: chr255=Z'FF'
        integer(one_byte),parameter :: chr255=-1
        character(*) fname
    !   If (fname(LEN_TRIM(fname)-3:LEN_TRIM(fname))) .ne. '.bmp') fname = TRIM(fname)//'.bmp'
        !Если (if) gfortran (Fortran 2003)
        open (newunit=unitnum,file=fname,access='stream',iostat=status) !,status='replace')
        !Если (if) openf95 (Fortran 95)
        !open (unitnum,file=fname,form='binary',iostat=status) !,status='replace')
        write(unitnum) "BM"                                    !bfType As Integer
        write(unitnum) int(54 + ubound(arg,1)*ScanAlign(ubound(arg,2)),four_bytes) !bfSize As Long
    !   write(*,*) int(54 + ubound(arg,1)*ScanAlign(ubound(arg,2)),four_bytes) !bfSize As Long
        write(unitnum) int(0,four_bytes)                                !bfReserved1 As Integer + bfReserved2 As Integer
        write(unitnum) int(54,four_bytes)                               !bfOffBits As Long

        write(unitnum) int(40,four_bytes)                             !biSize As Long
        write(unitnum) int(ubound(arg,2))                    !biWidth As Long
        write(unitnum) int(ubound(arg,1))                    !biHeight As Long
        write(unitnum) int(1,two_bytes)                              !biPlanes As Integer
        write(unitnum) int(24,two_bytes)                             !biBitCount As Integer
        write(unitnum) int(0,four_bytes)                              !biCompression As Long
        write(unitnum) int(ubound(arg,1)*ubound(arg,2),four_bytes)    !biSizeImage As Long
        write(unitnum) int(0,four_bytes) !biXPelsPerMeter As Long
        write(unitnum) int(0,four_bytes) !biYPelsPerMeter As Long
        write(unitnum) int(0,four_bytes) !biClrUsed As Long
        write(unitnum) int(0,four_bytes) !biClrImportant As Long
        strlen=ScanAlign(ubound(arg,2))-ubound(arg,2)*3
    !   write(*,*) ScanAlign(ubound(arg,2)), ubound(arg,2)
        do i=ubound(arg,1),1,-1
            do j=1,ubound(arg,2)
                write(unitnum) GetColorValue(arg(i,j),'b'),GetColorValue(arg(i,j),'g'),GetColorValue(arg(i,j),'r')
            enddo
            write(unitnum) (chr255,j=1,strlen)
        enddo
        close(unitnum)
        SaveBitmap=0
    end function SaveBitmap

    ! ScanAlign --
    !     Compute the number of bytes to align the scan line
    !
    ! Arguments:
    !     pwidth        Width of the scan
    !
    ! Result:
    !     Number of extra bytes
    !
    integer(four_bytes) function ScanAlign(pwidth)
        integer(four_bytes) pwidth
        ScanAlign = iand((pwidth*3 + 3), Z'FFFFFFFC')
    !    if (real(pwidth*3)/real(4) .gt. aint(real(pwidth * 3)/real(4))) then
    !       ScanAlign = int( aint( real(pwidth*3) / real(4) ) )*4 + 4
    !    else
    !       ScanAlign=pwidth
    !    endif
    end function ScanAlign

    ! OpenBitmap --
    !     Open the BMP file and return the image as an array
    !
    ! Arguments:
    !     ImageForOutput     Integer allocatable array with the image
    !     fname              Name of the BMP file
    !
    ! Result:
    !     Zero or an error code
    !
    integer(four_bytes) function OpenBitmap(ImageForOutput,fname)
    !   use conversions

        type (t_bmp) bitmap

        integer(one_byte), allocatable :: BmpTripleData(:)
        integer(four_bytes) status,i,j,k,pos,step,offset,unitnum
        integer(four_bytes), allocatable :: ImageForOutput(:,:),BmpData(:),palette(:)
    !   integer(four_bytes), allocatable :: BmpData(:),palette(:)
        character(*) fname
        character(20) msg

        !Открытие файла
        !Opening file
        !Если (if) gfortran (Fortran 2003)
        open (newunit=unitnum,file=fname,access='stream',form='unformatted',iostat=status, status='old')
        !Если (if) openf95 (Fortran 95)
        !open (1,file=fname,form='binary',iostat=status, status='old')

        !Проверка успешности открытия файла
        !Verifying file open
        if (status.ne.0) then
            msg = ''; write(msg, *) status
            !msg = 'Ошибка доступа к файлу # ' // trim(adjustl(msg))
            message = 'Error opening file - iostat = ' // trim(adjustl(msg))
            OpenBitmap=IOError
            return
        endif

        !Считывание заголовка
        !Reading the header
        status = GetBitmap(bitmap, unitnum)

        !Проверка правильности считывания заголовка
        !Verifying the header
        if (status.ne.0) then
            msg = ''; write(msg, *) status
            !msg = 'Ошибка функции GetBitmap # ' // trim(adjustl(msg))
            message = 'Error reading file - iostat = ' // trim(adjustl(msg))
            close(unitnum)
            OpenBitmap=IOError
            return
        endif

        allocate(ImageForOutput(bitmap%biHeight,bitmap%biWidth))

        select case (bitmap%biBitCnt)
            case(1,4,8) !Растр с палитрой !Raster with palette
                allocate(BmpData((bitmap%bfSize-bitmap%bfOffs)/4))
                !Считывание палитры !Reading the palette
                if (bitmap%biClrUsed.eq.0) then
                    allocate(palette(0:2**bitmap%biBitCnt-1))
                    status = ReadPalette(palette, unitnum)
                else
                    allocate(palette(0:bitmap%biClrUsed-1))
                    status = ReadPalette(palette, unitnum)
                endif

                !Проверка успешности считывания палитры
                !Verifying the palette
                if (status.ne.0) then
                    !msg = 'Ошибка функции ReadPalette # ' // trim(adjustl(msg))
                    message = 'Error in function ReadPalette - ' // message
                    deallocate(BmpData,ImageForOutput,palette)
                    close(unitnum)
                    OpenBitmap=IOError
                    return
                endif

                !Считывание области данных
                !Reading the data block
                read(unitnum, iostat=status) BmpData
                close(unitnum)

                !Проверка успешности считывания области данных
                !Verifying the data block
                if (status.ne.0) then
                    msg = ''; write(msg, *) status
                    !msg = 'Ошибка чтения данных # ' // trim(adjustl(msg))
                    message = 'Error reading data block # ' // trim(adjustl(msg))
                    deallocate(BmpData,ImageForOutput,palette)
                    OpenBitmap=IOError
                    return
                endif

                !"Переворачивание" байтов в одно- и четырехбитных растрах
                !Transposing the data bits
                if ((bitmap%biBitCnt.eq.1).or.(bitmap%biBitCnt.eq.4)) then
                    do i=1,ubound(BmpData,1)
                        BmpData(i)=TransposeBits(BmpData(i),bitmap%biBitCnt)
                    enddo
                endif

                offset=0
                step = 32/bitmap%biBitCnt
                do i=1,bitmap%biHeight
                    do j=0,bitmap%biWidth-1,step
                        offset=offset+1
                        do k=1,step
                            pos=j+k
                            if (pos.gt.bitmap%biWidth) exit
                            ImageForOutput(bitmap%biHeight-i+1,pos)=palette(        &
                        &   int(ibits(BmpData(offset),((k-1)*bitmap%biBitCnt),bitmap%biBitCnt),two_bytes))
                        enddo
                    enddo
                enddo
                deallocate(BmpData,palette)
                OpenBitmap=status

            case(24) !RGB data
                allocate(BmpTripleData(bitmap%bfSize-bitmap%bfOffs))
                !Проверка размерности созданного массива
                !Verifying the array dim-s
                !write(*,*) bitmap%bfSize,bitmap%bfOffs

                !Считывание области данных
                !Reading data block
                read(unitnum, iostat=status) BmpTripleData
                close(1)

                !Проверка правильности считывания области данных
                !Verifying the data block
                if (status.ne.0) then
                    msg = ''; write(msg, *) status
                    msg = 'Ошибка чтения данных # ' // trim(adjustl(msg))
                    message = 'Error verifying data block # ' // trim(adjustl(msg))
                    deallocate(BmpTripleData,ImageForOutput)
                    OpenBitmap=BadFile
                    return
                endif

                offset=-2
                k=ubound(BmpTripleData,1)/bitmap%biHeight-bitmap%biWidth*3
                !!Проверка правильности расчёта числа байт, необходимых для выравнивания строки по двойному слову
                !!Можно использовать функцию ScanAlign
                !!Verifying the number of bits needed to align the lines at a double word. Function ScanAlign can be used
                !!write(*,*) k,ubound(BmpTripleData,1),bitmap%biHeight,bitmap%biWidth
                do i=1,bitmap%biHeight
                    do j=1,bitmap%biWidth
                        offset=offset+3
                        ImageForOutput(bitmap%biHeight-i+1, j) = &
                            RGB(BmpTripleData(offset+2), BmpTripleData(offset+1),BmpTripleData(offset))
                    enddo
                    offset=offset+k
                enddo
                deallocate(BmpTripleData)
                OpenBitmap=status
            case default
                !write (*,*) 'Неизвестный формат файла'
                message = 'Unknown file format'
                close(1)
                OpenBitmap=BadFile
        endselect

    !   write (*,*) ubound(ImageForOutput,1),ubound(ImageForOutput,2)
    !   write (*,*) ImageForOutput(170,702)
    !   write (*,*) GetColorValue(ImageForOutput(170,702),'r')
    !   write (*,*) GetColorValue(ImageForOutput(170,702),'g')
    !   write (*,*) GetColorValue(ImageForOutput(170,702),'b')

    !   status=SaveBitmap(ImageForOutput,'./grrr.bmp')
    !   deallocate(ImageForOutput)
    end function OpenBitmap

end module bmpfile
