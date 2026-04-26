program main
    implicit none

    character :: board(3, 3)
    character :: player
    integer :: turn

    call initBoard(board)
    player = "X"

    do turn = 1, 9
        call printBoard(board)
    end do

contains

    subroutine initBoard(b)
        character, intent(out) :: b(3, 3)

        b = '.'
    end subroutine

    subroutine printBoard(b) ! Prints the board in a pretty format
        character, intent(in) :: b(3, 3)
        integer :: spot
        print *, " "

        do spot = 1, 3 ! actual printing
            write(*, "(1X,A,2X,A,2X,A)") b(spot, 1), b(spot, 2), b(spot, 3)
        end do

        print *, " "
    end subroutine
end program main
