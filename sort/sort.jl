#!julia

using Random

# create vector of random integers
v = rand(UInt32, (10)) 

function print_vector(vec)
    for e in vec
        println(e)
    end
    println()
end

println("orig:") 
print_vector(v)

# find index of minimum element in vector
function min_index(vec)
    m_val = typemax(UInt32)
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
    #println("swapping ", vec[i], " and ", vec[j])
    o = vec[i]
    vec[i] = vec[j]
    vec[j] = o
end

# selection sort
function ssort!(vec)
    if length(vec) > 1
        ind = min_index(vec)
        if ind > 1
            swap(vec, 1, ind)
        end

        # recursive call to ssort 
        vec[2:end] = ssort!(vec[2:end])
    end

    return vec
end

# quicksort
function qsort!(vec)
    if length(vec) > 1
        # use last element as pivot
        ins = 1
        for icomp in 1:(length(vec)-1)
            if vec[icomp] < vec[end] 
                swap(vec, icomp, ins)
                ins += 1
            end
        end
        swap(vec, ins, length(vec))

        vec[1:ins-1] = qsort!(vec[1:ins-1])
        vec[ins+1:end] = qsort!(vec[ins+1:end])
    end

    return vec
end

# insertion short
function isort!(vec)
    if length(vec) > 1
        for i in 2:length(vec)
            vec = insload(vec, i)
        end
    end

    vec
end

function insload(vec, loc)
    if (loc > 1) && (vec[loc] < vec[loc-1])
        swap(vec, loc, loc-1)
        vec = insload(vec, loc-1)
    end

    vec
end

println("ssort:") 
print_vector(ssort!(copy(v)))

println("qsort:") 
print_vector(qsort!(copy(v)))

println("isort:") 
print_vector(isort!(copy(v)))
