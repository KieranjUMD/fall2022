class GameBoard
    # @max_row is an `Integer`
    # @max_column is an `Integer`
    attr_reader :max_row, :max_column, :board_setup

    def initialize(max_row, max_column)
        @max_row = max_row
        @max_column = max_column
        @board_setup = Hash.new 0
        @succ_attacks = 0
        @total_ships_locs = 0
    end

    # adds a Ship object to the GameBoard
    # returns Boolean
    # Returns true on successfully added the ship, false otherwise
    # Note that Position pair starts from 1 to max_row/max_column
    def add_ship(ship)
        direction = -1
        valid = true
        if ship.orientation == "Down" || ship.orientation == "Right" then
            direction = 1
        end
        
        if ship.orientation == "Up" || ship.orientation == "Down" then
            for i in 0..ship.size-1 do
                if valid then
                    valid = valid_pos(ship.start_position.row + direction*i, ship.start_position.column)
                end
            end
            if valid then
                for i in 0..ship.size-1 do
                    board_setup[(ship.start_position.row + direction*i)*max_column + ship.start_position.column] = 1
                end
            end
        elsif ship.orientation == "Left" || ship.orientation == "Right" then
            for i in 0..ship.size-1 do
                if valid then
                    valid = valid_pos(ship.start_position.row, ship.start_position.column + direction*i)
                end
            end  
            if valid then
                for i in 0..ship.size-1 do
                    board_setup[(ship.start_position.row)*max_column + ship.start_position.column + direction*i] = 1
                end
            end      
        end
        if valid then
            @total_ships_locs += ship.size
        end
        valid
    end

    def valid_pos(row, column)
        valid = true
        if row<1 || row>max_row || column<1 || column>max_column then
            valid = false
        elsif board_setup[row*max_column + column] != 0
            valid = false
        end
        valid
    end

    # return Boolean on whether attack was successful or not (hit a ship?)
    # return nil if Position is invalid (out of the boundary defined)
    def attack_pos(position)
        # check position
        ret_val = false
        if position.row<1 || position.row>max_row || position.column<1 || position.column>max_column then
            ret_val = nil
        elsif board_setup[position.row*max_column + position.column] == 1
            ret_val = true
            @succ_attacks += 1
            board_setup[position.row*max_column + position.column] = 2
        end 
        # update your grid

        # return whether the attack was successful or not
        ret_val
    end

    # Number of successful attacks made by the "opponent" on this player GameBoard
    def num_successful_attacks
        @succ_attacks
    end

    # returns Boolean
    # returns True if all the ships are sunk.
    # Return false if at least one ship hasn't sunk.
    def all_sunk?
        @succ_attacks == @total_ships_locs
    end


    # String representation of GameBoard (optional but recommended)
    def to_s
        ret_val = ""
        for row in 1..max_row do
            for column in 1..max_column do
                ret_val += (board_setup[row*max_column + column]).to_s + " "
            end
            ret_val+= "\n"
        end
        ret_val
    end
end
