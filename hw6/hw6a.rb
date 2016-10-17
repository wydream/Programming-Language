# University of Washington, Programming Languages, Homework 6, hw6runner.rb

# This is the only file you turn in, so do not modify the other files as
# part of your solution.

class MyPiece < Piece

  # The constant All_My_Pieces should be declared here
  # class array holding all the pieces and their rotations
  All_My_Pieces = [[[[0, 0], [1, 0], [0, 1], [1, 1]]], # square (only needs one)
                   rotations([[0, 0], [-1, 0], [1, 0], [0, -1]]), # T
                   [[[0, 0], [-1, 0], [1, 0], [2, 0]], # long (only needs two)
                    [[0, 0], [0, -1], [0, 1], [0, 2]]],
                   rotations([[0, 0], [0, -1], [0, 1], [1, 1]]), # L
                   rotations([[0, 0], [0, -1], [0, 1], [-1, 1]]), # inverted L
                   rotations([[0, 0], [-1, 0], [0, -1], [1, -1]]), # S
                   rotations([[0, 0], [1, 0], [0, -1], [-1, -1]]), # Z

                   rotations([[0, 0], [-1, 0], [1, 0], [0, -1], [-1, -1]]), #new1
                   [[[0, 0], [-1, 0], [1, 0], [2, 0], [-2, 0]], #new2
                    [[0, 0], [0, -1], [0, 1], [0, 2], [0, -2]]], #inverted new2
                   rotations([[0, 0], [1, 0], [0, -1]])]

  Cheat_piece =
      [[[[0, 0]]]]

  def self.cheat_piece(board)
    MyPiece.new(Cheat_piece.sample, board)
  end

  def self.next_piece (board)
    MyPiece.new(All_My_Pieces.sample, board)
  end

end

class MyBoard < Board

  def initialize (game)
    @grid = Array.new(num_rows) { Array.new(num_columns) }
    @current_block = MyPiece.next_piece(self)
    @score = 0
    @game = game
    @delay = 500
    @cheat_flag = false
  end

  # rotates the current piece clockwise
  def rotate_180d
    if !game_over? and @game.is_running?
      @current_block.move(0, 0, 2)
    end
    draw
  end

  # gets the next piece
  def next_piece
    if @cheat_flag
      @current_block = MyPiece.cheat_piece(self)
      @cheat_flag = false
    else
      @current_block = MyPiece.next_piece(self)
    end
    @current_pos = nil
  end

  def store_current
    locations = @current_block.current_rotation
    displacement = @current_block.position
    (0..4).each{|index|
      current = locations[index]
      if current != nil and @current_pos[index] != nil
        @grid[current[1]+displacement[1]][current[0]+displacement[0]] = @current_pos[index]
      end
    }
    remove_filled
    @delay = [@delay - 2, 80].max
  end

  def cheat
    if @score >= 100 and !@cheat_flag
      @score -= 100
      @cheat_flag = true
    end
  end

end

class MyTetris < Tetris
  # creates a canvas and the board that interacts with it
  def set_board
    @canvas = TetrisCanvas.new
    @board = MyBoard.new(self)
    @canvas.place(@board.block_size * @board.num_rows + 3,
                  @board.block_size * @board.num_columns + 6, 24, 80)
    @board.draw
  end

  def key_bindings
    super
    @root.bind('u', proc { @board.rotate_180d })
    @root.bind('c', proc { @board.cheat })
  end
end


