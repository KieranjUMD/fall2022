class PhoneBook
    def initialize
        @allNames = Hash.new
        @listedNumbers = Hash.new
    end

    def add(name, number, is_listed)
        retVal = false
        if @allNames[name] == nil then
            if is_listed then
                if @listedNumbers[number] == nil then
                    @allNames[name] = number
                    @listedNumbers[number] = name
                    retVal = true
                end
            else
                @allNames[name] = number
                retVal = true
            end
        end
        retVal
    end

    def lookup(name)
        listedName = @listedNumbers[@allNames[name]]
        number = nil
        if listedName == name then
            number = @allNames[name]
        end
        number
    end

    def lookupByNum(number)
        @listedNumbers[number]
    end

    def namesByAc(areacode)
        arr = Array.new
        for key,value in @allNames do
            if value[0,3] == areacode then
                arr.push key
            end
        end
        arr
    end
end