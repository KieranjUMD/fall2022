def fib(n)
    arr = []
    if n>0 then arr.push 0
        if n>1 then arr.push 1
            if n>2 then
                for i in 3..n do
                    arr.push arr[-1]+arr[-2]
                end
            end
        end
    end
    arr

end

def isPalindrome(n)
    str = n.to_s
    bool = true
    for i in 0..str.length-1 do
        if str[i]!=str[str.length-1-i] then
            bool = false
        end
    end
    bool
end

def nthmax(n, a)
    big = nil
    if a.length != 0 && a.length>n then
        for i in 0..n do 
            big = a[0]
            bigloc = 0
            for j in 0..a.length-1 do
                if a[j]>big then
                    big = a[j]
                    bigloc = j
                end
            end
            a.delete_at bigloc
        end
    end
    big
end

def freq(s)
    h = Hash.new 0
    for i in 0..s.length-1 do
        h[s[i]] += 1
    end
    max = ""
    maxval = 0
    for key,value in h do
        if value > maxval then
            max = key
            maxval = value
        end
    end
    max
end

def zipHash(arr1, arr2)
    arr = nil
    if arr1.length == arr2.length then
        arr = Hash.new
        for i in 0..arr1.length-1 do
            arr[arr1[i]] = arr2[i]
        end
    end
    arr

end

def hashToArray(hash)
    arr = Array.new(hash.length){Array.new(2)}
    i = 0
    for key,value in hash do
        arr[i][0] = key
        arr[i][1] = value
        i+=1
    end
    arr
end
