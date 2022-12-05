require "./src/models/position.rb"
require "./src/models/ship.rb"
require "./src/models/game_board.rb"
xy = Position.new(1,2)
ship = Ship.new(xy,"Right",3)
board = GameBoard.new(10,10)
board.add_ship(ship)
puts board.to_s
board.attack_pos(xy)
puts board.to_s
xy = Position.new(1,3)
board.attack_pos(xy)
xy = Position.new(1,4)
board.attack_pos(xy)
puts board.to_s
board.num_successful_attacks
board.all_sunk?