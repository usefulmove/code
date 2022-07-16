#!julia

using Random

# create vector of random integers
v = rand(UInt32, (4)) 

function print_vector(vec)
    for e in vec
        println(e)
    end
end

println("orig:") 
print_vector(v)

# find index of minimum element in vector
function min_index(vec)
    m_val = 2^32 
    m_ind = 0
    for i = 1:length(vec)
        if vec[i] < m_val
            m_ind = i
            m_val = vec[i]
        end
    end

    return m_ind
end

# swap vector elements
function swap(vec, i, j)
    println("swap: swapping ", vec[i], " and ", vec[j])
    o = vec[i]
    vec[i] = vec[j]
    vec[j] = o
end

# selection sort
function ssort!(vec)
    if length(vec) > 1
        ind = min_index(vec)
        if ind > 1
            println("ssort: swap ", vec[1], " and ", vec[ind])
            swap(vec, 1, ind)
        else
            println("no swap")
        end

        ssort!(vec[2:end])
    else
        println("no swap (termination condition)")
    end
end

ssort!(v)

println("sorted:") 
print_vector(v)
