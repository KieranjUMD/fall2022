require_relative '../models/game_board'
require_relative '../models/ship'
require_relative '../models/position'

# return a populated GameBoard or nil
# Return nil on any error (validation error or file opening error)
# If 5 valid ships added, return GameBoard; return nil otherwise
def read_ships_file(path)
    num_ships = 0
    board = GameBoard.new 10, 10
    if File.file?(path) then 
        file = File.open(path, "r")
        file.readlines.each do |line|
            if (line =~ /^\(\d+,\d+\), (Right|Up|Left|Down), [1-5]$/) == 0 && num_ships != 5 then
                entry = line.split(',', 4)
                pos = Position.new(entry[0].delete("(").to_i, entry[1].delete(")").to_i)
                ship = Ship.new(pos, entry[2].strip, entry[3].strip.to_i)
                board.add_ship(ship)
                num_ships += 1
            end
        end
        file.close
    end
    if num_ships != 5 then
        board = nil
    end
    board
end


# return Array of Position or nil
# Returns nil on file open error
def read_attacks_file(path)
    arr = []
    if File.file?(path) then
        file = File.open(path, "r")
        file.readlines.each do |line|
            if (line =~ /^\(\d+,\d+\)$/) == 0 then
                entry = line.split(',', 2)
                pos = Position.new(entry[0].delete("(").to_i, entry[1].delete(")").to_i)
                arr.push(pos)
            end
        end
        [Position.new(1, 1)]
    else
        arr = nil
    end
    arr
end


# ===========================================
# =====DON'T modify the following code=======
# ===========================================
# Use this code for reading files
# Pass a code block that would accept a file line
# and does something with it
# Returns True on successfully opening the file
# Returns False if file doesn't exist
def read_file_lines(path)
    return false unless File.exist? path
    if block_given?
        File.open(path).each do |line|
            yield line
        end
    end

    true
end
