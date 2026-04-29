program main
    implicit none

    character :: board(3, 3)
    character :: player = 'X'
    integer :: turn
    integer :: row
    integer :: column
    integer :: winner
    logical :: valid
    character, parameter :: EMPTY = '.'

    ! initialize "title" screen
    print *, "TicTacToe, but written in Fortran!"

    call initBoard(board)

    do turn = 1, 9
        call printBoard(board)

        valid = .false.

        do while (.not. valid)
            write(*, "(A)", advance = "no") "Enter row and column (example: '1 3'): "
            read(*, *, iostat = winner) row, column

            if (winner /= 0) then
                print *, "Invalid input."
                cycle
            end if

            if (row < 1 .or. row > 3 .or. column < 1 .or. column > 3) then
                print *, "Out of range. Rows/columns must be 1 to 3."
            else if (board(row, column) /= EMPTY) then
                print *, "This spot is already taken! Try again."
            else
                valid = .true.
            end if
        end do

        board(row, column) = player
        winner = checkWinner(board)

        if (winner /= 0) then
            call printBoard(board)

            print *, "Player ", player, " wins! "
            stop
        end if

        if (turn == 9) then
            call printBoard(board)

            print *, "It's a draw!"
            stop
        end if

        if (player == 'X') then
            player = 'O'
        else
            player = 'X'
        end if
    end do

contains

    subroutine initBoard(b)
        character, intent(out) :: b(3, 3)

        b = EMPTY
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

    integer function checkWinner(b) ! Check if a player has won in a given board.
        character, intent(in) :: b(3,3)
        integer :: idx

        checkWinner = 0

        ! Rows and columns
        do idx = 1, 3
            if (b(idx, 1) /= EMPTY .and. b(idx, 1) == b(idx, 2) .and. b(idx, 2) == b(idx, 3)) then
                checkWinner = 1
                return
            end if

            if (b(1, idx) /= EMPTY .and. b(1, idx) == b(2, idx) .and. b(2, idx) == b(3, idx)) then
                checkWinner = 1
                return
            end if
        end do

        ! Diagonals (top left -> bottom right)
        if (b(1, 1) /= EMPTY .and. b(1, 1) == b(2, 2) .and. b(2, 2) == b(3, 3)) then
            checkWinner = 1
            return
        end if

        ! Diagonals (top right -> bottom left)
        if (b(1, 3) /= EMPTY .and. b(1, 3) == b(2, 2) .and. b(2, 2) == b(3, 1)) then
            checkWinner = 1
            return
        end if
    end function

end program main
